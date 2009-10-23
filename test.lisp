;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE-TESTS; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2008 by
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

(defpackage desire-tests
  (:use :common-lisp :iterate :alexandria :desire :pergamum :executor)
  (:export
   #:*definitions-path*
   #:run-tests))

(in-package :desire-tests)

(define-executable ssh)

(defvar *test-user-account* "empty@betelheise")
(defvar *bootstrap-script-location* "http://www.feelingofgreen.ru/shared/git/desire/climb.sh")

(defvar *clean-command* "rm -rf climb.sh desr /tmp/empty-*")
(defvar *download-bootstrapper-command* (format nil "wget ~A" *bootstrap-script-location*))
(defvar *disable-debugger* "export DISABLE_DEBUGGER=t")
(defvar *climacs-load-command* "bash climb.sh /home/empty/desr/ climacs")
(defvar *xcvb-bootstrap-command* "bash climb.sh /home/empty/desr/ xcvb")

(defvar *tests-common* `(,*clean-command*
                         ,*download-bootstrapper-command*
                         ,*disable-debugger*))

(defvar *climacs-test* `(,@*tests-common*
                         ,*climacs-load-command*))

(defvar *xcvb-bootstrap-test* `(,@*tests-common*
                                ,*xcvb-bootstrap-command*))

(defparameter *test-output-separator* ";; ------------------------  8<  ------------------------------")
(defvar *test-outputs* nil)

(defun compile-shell-command (commands &aux (output ""))
  (iter (for command in (butlast commands))
        (setf output (concatenate 'string output command " &&" #(#\Newline)))
        (finally
         (return (concatenate 'string output (lastcar commands))))))

(defun run-remote-test (name commands &optional verbose (account *test-user-account*))
  (lret (condition
         output
         successp)
    (desire::report t ";; Running test ~A: " name)
    (with-output-to-string (capture)
      (let ((*standard-output-direction* capture))
        (handler-case
            (setf successp
                  (with-input-from-string (stream (compile-shell-command commands))
                    (with-executable-input-stream stream
                      (ssh account "bash" "-s"))))
          (serious-condition (c) (setf condition c)))
        (when (open-stream-p capture)
          (finish-output capture))
        (setf output (get-output-stream-string capture))
        (push output *test-outputs*)))
    (if (or verbose condition)
        (desire::report t "~:[failure~;success~].~%;;~%;; The output was:~%~A~%~A~&~A~%~:[~;~:*~@<;; ~@;The condition met was: ~A~:@>~%~]"
                        successp *test-output-separator* output *test-output-separator* condition)
        (desire::report t "success.~%"))))

(defun run-tests (&key verbose (account *test-user-account*))
  (lret ((success t))
    (setf success (and (run-remote-test :climacs *climacs-test* verbose account) success))
    (setf success (and (run-remote-test :xcvb-bootstrap *xcvb-bootstrap-test* verbose account) success))))