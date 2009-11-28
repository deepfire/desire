;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :desire)


(defparameter *buildslave-remote-output-marker* :beginning-of-test-results-marker)
(defparameter *buildslave-remote-end-of-output-marker* :end-of-test-results-marker)
(defparameter *buildslave-remote-test-output-marker* :beginning-of-test-result-marker)
(defparameter *buildslave-remote-end-of-test-output-marker* :end-of-test-result-marker)

(defun module-test-reachability (m &key capture-output)
  (with-recorded-status (:record-backtrace t :record-output capture-output)
    (touch-module m)
    t))

(defun module-test-fetchability (m &key capture-output)
  (let ((*fetch-errors-serious* t))
    (with-recorded-status (:record-backtrace t :record-output capture-output)
      (unwind-protect (update m)
        (finish-output *standard-output*)
        (finish-output *error-output*))
      t)))

(defun module-test-recursion (m &key capture-output)
  (with-recorded-status (:record-backtrace t :record-output capture-output)
    (unwind-protect (desire (list (name m)) :skip-present t :seal nil)
      (finish-output *standard-output*)
      (finish-output *error-output*))
    t))

(defun module-test-loadability (m &key capture-output)
  (with-recorded-status (:record-backtrace t :record-output capture-output)
    (unwind-protect (asdf:oos 'asdf:load-op (name m))
      (finish-output *standard-output*)
      (finish-output *error-output*))
    t))

(defun module-test-internal (m &key capture-output)
  (declare (ignore m))
  (with-recorded-status (:record-backtrace t :record-output capture-output)
    (unwind-protect (not-implemented 'module-test-internal)
      (finish-output *standard-output*)
      (finish-output *error-output*))))

(defun buildslave (module-names phases)
  (let ((modules (mapcar #'module module-names)))
    (syncformat t "~%~S~%" *buildslave-remote-output-marker*)
    (when (member "SLAVE-FETCH-PHASE" phases :test #'string-equal)
      (iter (for m in modules)
            (syncformat t "(:name ~S :mode :fetch~%~S~%" (name m) *buildslave-remote-test-output-marker*)
            (destructuring-bind (&key return-value condition backtrace) (module-test-fetchability m)
              (syncformat t "~:[~*~;~%>>> A condition of type ~A was encountered during execution.~]~%~S~%:status ~S :condition ~S :backtrace ~S)~%"
                          condition (type-of condition) *buildslave-remote-end-of-test-output-marker* return-value (format nil "~A" condition) (format nil "~A" backtrace)))))
    (when (member "SLAVE-RECURSE-PHASE" phases :test #'string-equal)
      (iter (for m in modules)
            (syncformat t "(:name ~S :mode :recurse~%~S~%" (name m) *buildslave-remote-test-output-marker*)
            (destructuring-bind (&key return-value condition backtrace) (module-test-recursion m)
              (syncformat t "~:[~*~;~%>>> A condition of type ~A was encountered during execution.~]~%~S~%:status ~S :condition ~S :backtrace ~S)~%"
                          condition (type-of condition) *buildslave-remote-end-of-test-output-marker* return-value (format nil "~A" condition) (format nil "~A" backtrace)))))
    (when (member "SLAVE-LOAD-PHASE" phases :test #'string-equal)
      (iter (for m in modules)
            (syncformat t "(:name ~S :mode :load~%~S~%" (name m) *buildslave-remote-test-output-marker*)
            (destructuring-bind (&key return-value condition backtrace) (module-test-loadability m)
              (syncformat t "~:[~*~;~%>>> A condition of type ~A was encountered during execution.~]~%~S~%:status ~S :condition ~S :backtrace ~S)~%"
                          condition (type-of condition) *buildslave-remote-end-of-test-output-marker* return-value (format nil "~A" condition) (format nil "~A" backtrace)))))
    (when (member "SLAVE-TEST-PHASE" phases :test #'string-equal)
      (iter (for m in modules)
            (destructuring-bind (&key return-value condition backtrace) (module-test-internal m)
              (syncformat t "~:[~*~;~%>>> A condition of type ~A was encountered during execution.~]~%~S~%:status ~S :condition ~S :backtrace ~S)~%"
                          condition (type-of condition) *buildslave-remote-end-of-test-output-marker* return-value (format nil "~A" condition) (format nil "~A" backtrace)))))
    (syncformat t "~%~S~%" *buildslave-remote-end-of-output-marker*)))
