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
   ;; buildslave entry point
   #:test-converted-modules
   ;; buildmaster
   #:run-build-tests))

(in-package :desire-buildbot)

(define-condition build-test-error (desire-error) ())
(define-simple-error build-test-error)

(defvar *default-buildslave-host* "betelheise")
(defvar *default-buildslave-username* "empty")
(defvar *default-buildslave-master* :git.feelingofgreen.ru)

(defparameter *bootstrap-script-location* "http://www.feelingofgreen.ru/shared/git/desire/climb.sh")
(defparameter *purge-command* "rm -rf desr")
(defparameter *purge-metastore-command* "rm -rf desr/git/.meta")
(defparameter *update-bootstrapper-command* (format nil "wget ~A -O climb.sh" *bootstrap-script-location*))

(defun invoke-with-status-recording (fn)
  (with-output-to-string (output)
    (multiple-value-bind (condition successp)
        (with-collected-conditions (error)
          (let ((*standard-output* output)
                (*error-output* output))
            (funcall fn)))
      (finish-output output)
      (return-from invoke-with-status-recording
        (list* :status successp :output (string-right-trim '(#\Newline) (get-output-stream-string output))
               (when condition `(:condition ,(format nil "\"~A\"" condition))))))))

(defmacro with-recorded-status (() &body body)
  `(invoke-with-status-recording (lambda () ,@body)))

(defun module-test-fetchability (m)
  (let ((*fetch-errors-serious* t))
    (with-recorded-status ()
      (desr::update-module m)
      t)))

(defun module-test-loadability (m)
  (with-recorded-status ()
    (not (null (asdf:oos 'asdf:load-op (name m))))))

(defvar *remote-output-marker* :beginning-of-test-results-marker)

(defun test-converted-modules ()
  (let ((converted-modules (mapcar #'module (desire::distributor-converted-modules (distributor *default-buildslave-master*)))))
    (syncformat t "~&~S~%" *remote-output-marker*)
    (iter (for m in converted-modules)
          (syncformat t "(:name ~S :mode :fetch~%" (name m))
          (syncformat t " ~{ ~S~})~%" (module-test-fetchability m)))
    (iter (for m in converted-modules)
          (syncformat t "(:name ~S :mode :load~%" (name m))
          (syncformat t " ~{ ~S~})~%" (module-test-loadability m)))
    #+(or)
    (iter (for m in converted-modules)
          (syncformat t "(:name ~S :mode :test" (name m))
          (when (module-has-tests-p m)
            (syncformat t "~{ ~S~})~%" (module-run-tests m))))))

(defun run-build-tests (&key purge purge-metastore (hostname *default-buildslave-host*) (username *default-buildslave-username*)
                        branch metastore-branch
                        disable-debugger
                        verbose)
  (find-executable 'ssh)
  (multiple-value-bind (status output condition)
      (watch-remote-commands hostname username (remove nil
                                                       (list
                                                        (when purge
                                                          *purge-command*)
                                                        (when purge-metastore
                                                          *purge-metastore-command*)
                                                        *update-bootstrapper-command*
                                                        (format nil "bash climb.sh ~:[~;-v ~]~
                                                                      ~:[~;-b ~:*~(~A~) ~] ~:[~;-t ~:*~(~A~) ~]~
                                                                      ~:[~;-g ~]~
                                                                      -x '(progn (desr:ensure-module-systems-loadable :desire) ~
                                                                                 (require :desire-buildbot) ~
                                                                                 (funcall (find-symbol \"TEST-CONVERTED-MODULES\" :desire-buildbot)) ~
                                                                                 (sb-ext:quit))' ~
                                                                      ~~/desr"
                                                                verbose
                                                                branch metastore-branch
                                                                disable-debugger))))
    (if-let ((marker-posn (search (prin1-to-string *remote-output-marker*) output)))
      (with-input-from-string (s output :start (+ marker-posn 2 (length (symbol-name *remote-output-marker*))))
        (iter (for form = (read s nil nil))
              (while form)
              (destructuring-bind (&key name mode status output condition) form
                (declare (ignore output))
                (format t "module ~A, ~A ~A~:[~; encountered condition: ~:*~A~]~%" name mode status condition))))
      (build-test-error "~<@Marker ~S wasn't found in remote output.~:@>" *remote-output-marker*))
    status))