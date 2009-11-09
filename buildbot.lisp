;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE-BUILDBOT; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2009 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(defpackage desire-buildbot
  (:use :common-lisp :iterate :alexandria :pergamum :executor :desire)
  (:export
   #:test-all-converted-modules))

(in-package :desire-buildbot)

(define-executable ssh)

(defun compile-shell-command (commands &aux (output ""))
  (iter (for command in (butlast commands))
        (setf output (concatenate 'string output command " &&" #(#\Newline)))
        (finally
         (return (concatenate 'string output (lastcar commands))))))

(defun invoke-with-captured-external-output-and-status (fn external-invocation-fn)
  (let* (status
         condition
         (output
          (with-output-to-string (capture)
            (let ((*executable-standard-output-direction* capture))
              (handler-case (setf status (funcall external-invocation-fn))
                (serious-condition (c)
                  (setf condition c)))
              (when (open-stream-p capture)
                (finish-output capture))))))
    (funcall fn status output condition)))

(defun run-remote-commands (hostname username commands &optional verbose)
  (let (successp
        condition)
    (let ((output
           (with-output-to-string (capture)
             (let ((*executable-standard-output-direction* capture)
                   (commands (compile-shell-command commands)))
               (when verbose
                 (format t "~@<;;; ~@;executing following commands as user \"~A\" on ~A:~%~A~:@>~%" username hostname commands))
               (handler-case
                   (setf successp (with-input-from-string (stream commands)
                                    (with-executable-input-stream stream
                                      (ssh `(,username "@" ,hostname) "bash" "-s"))))
                 (serious-condition (c)
                   (setf condition c)))
               (when (open-stream-p capture)
                 (finish-output capture))
               (when (and verbose condition)
                 (format t "~@<;;; ~@;encountered condition while operating as user \"~A\" on ~A:~%~A~:@>~%" username hostname condition))))))
      (apply #'values
             successp
             output
             (when condition (list condition))))))

(defun watch-remote-commands (hostname username commands &optional verbose)
  (let (successp
        condition)
    (let ((output
           (with-output-to-string (capture)
             (desire::with-pipe-stream (pipe :element-type 'character :buffering :none)
               (let ((*executable-standard-output-direction* pipe)
                     (commands (compile-shell-command commands)))
                 (when verbose
                   (format t "~&~@<;;; ~@;executing following commands as user \"~A\" on ~A:~%~A~:@>~%" username hostname commands))
                 (handler-case
                     (let ((process (with-asynchronous-execution
                                      (with-input-from-string (stream commands)
                                        (with-executable-input-stream stream
                                          (ssh `(,username "@" ,hostname) "bash" "-s"))))))
                       (close (two-way-stream-output-stream pipe))
                       (iter (for line = (read-line pipe nil nil))
                             (while line)
                             (format t "> ~A~%" line)
                             (finish-output)
                             (write-line line capture)))
                   (serious-condition (c)
                     (setf condition c)))
                 (when (and verbose condition)
                   (format t "~@<;;; ~@;encountered condition while operating as user \"~A\" on ~A:~%~A~:@>~%" username hostname condition)))))))
      (apply #'values
             successp
             output
             (when condition (list condition))))))

(defvar *default-buildslave-host* "betelheise")
(defvar *default-buildslave-username* "empty")
(defvar *default-buildslave-master* :git.feelingofgreen.ru)

(defparameter *bootstrap-script-location* "http://www.feelingofgreen.ru/shared/git/desire/climb.sh")
(defparameter *clean-command* "rm -rf desr")
(defparameter *download-bootstrapper-command* (format nil "wget ~A -O climb.sh" *bootstrap-script-location*))
(defparameter *run-tests-command* (format nil "bash climb.sh -v -s desire-buildbot -x '(progn (desire-buildbot::test-all-converted-modules) (sb-ext:quit))' ~~/desr"))

(defparameter *buildslave-commands* `(,*clean-command*
                                      ,*download-bootstrapper-command*
                                      ,*run-tests-command*))

(defun test-module (m)
  (multiple-value-bind (fetch-error fetch-successp)
      (let ((*fetch-errors-serious* t))
        (with-collected-conditions (error)
          (lust m)))
    (multiple-value-bind (load-error load-successp)
        (when fetch-successp
          (with-collected-conditions (error)
            (require (name m))))
      (let ((condition (or fetch-error load-error)))
        (list* :fetch-successp fetch-successp :load-successp load-successp
               (when condition
                 `(:condition ,condition)))))))

(defun test-all-converted-modules ()
  (let ((converted-modules (mapcar #'module (desire::distributor-converted-modules (distributor *buildslave-master*)))))
    (syncformat t "~&~S~%" :beginning-of-test-results-marker)
    (iter (for m in converted-modules)
          (syncformat t "~S~%" (list* (name m) (test-module m))))))

(defun run-build-tests (&key (purge nil) (hostname *default-buildslave-host*) (username *default-buildslave-username*))
  (find-executable 'ssh)
  (watch-remote-commands hostname username (xform (not purge) #'rest *buildslave-commands*))
  (values))