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

(defgeneric run-module-test (type module-name &optional verbose-internals record-output)
  (:method :around (type module-name &optional verbose-internals record-output)
    (declare (ignore verbose-internals))
    (pergamum::invoke-with-maybe-prepended-output
     record-output
     (lambda ()
       (multiple-value-bind (condition backtrace return-value)
           (block block
             (flet ((record-condition (c)
                      (return-from block
                        (values c (with-output-to-string (string-stream)
                                    (backtrace most-positive-fixnum string-stream))))))
               (handler-bind ((serious-condition #'record-condition)
                              #+sbcl
                              (sb-c:compiler-error #'record-condition)
                              #+sbcl
                              (sb-c:fatal-compiler-error #'record-condition))
                 (values nil nil
                         (unwind-protect (call-next-method)
                           (finish-output *standard-output*)
                           (finish-output *error-output*))))))
         (list* :return-value return-value (when condition
                                             (list :condition condition :backtrace backtrace))))))
           ;; (with-recorded-status (:record-backtrace t :record-output record-output)
           ;;   (unwind-protect (call-next-method)
           ;;     (finish-output *standard-output*)
           ;;     (finish-output *error-output*)))
           )
  (:method ((o (eql :master-reachability-phase)) mn &optional verbose-internals record-output)
    (declare (ignore verbose-internals record-output))
    (touch-module mn))
  (:method ((o (eql :master-update-phase)) mn &optional verbose-internals record-output)
    (declare (ignore verbose-internals record-output))
    (let ((*fetch-errors-serious* t))
      (update mn)
      t))
  (:method ((o (eql :master-recurse-phase)) mn &optional verbose-internals record-output)
    (declare (ignore record-output))
    (desire (list mn) :skip-present t :skip-missing t :seal nil :verbose verbose-internals)
    t)
  (:method ((o (eql :slave-fetch-phase)) mn &optional verbose-internals record-output)
    (declare (ignore verbose-internals record-output))
    (let ((*fetch-errors-serious* t))
      (update mn)
      t))
  (:method ((o (eql :slave-recurse-phase)) mn &optional verbose-internals record-output)
    (declare (ignore record-output))
    (desire (list mn) :skip-present t :seal nil :verbose verbose-internals)
    t)
  (:method ((o (eql :slave-load-phase)) mn &optional verbose-internals record-output)
    (declare (ignore verbose-internals record-output))
    (asdf:oos 'asdf:load-op mn)
    t)
  (:method ((o (eql :slave-test-phase)) mn &optional verbose-internals record-output)
    (declare (ignore verbose-internals record-output))
    (not-implemented :slave-test-phase)))

(defun invoke-with-slave-output-markers (stream fn)
  (syncformat stream "~%~S~%" *buildslave-remote-output-marker*)
  (funcall fn)
  (syncformat stream "~%~S~%" *buildslave-remote-end-of-output-marker*))

(defmacro with-slave-output-markers (() &body body)
  `(invoke-with-slave-output-markers t (lambda () ,@body)))

(defun buildslave (modules phases &optional verbose-internals)
  (let ((module-names (mapcar #'canonicalise-name modules))
        (phases (mapcar (compose #'make-keyword #'string-upcase #'string) phases)))
    (iter (for p in phases)
          (iter (for mn in module-names)
                (syncformat t "(:name ~S :mode ~(~S~)~%~S~%" mn p *buildslave-remote-test-output-marker*)
                (destructuring-bind (&key return-value condition backtrace) (run-module-test p mn verbose-internals)
                  (syncformat t "~:[~*~;~%>>> A condition of type ~A was encountered during execution.~]~%~S~%:status ~S :condition ~S :backtrace ~S)~%"
                              condition (type-of condition) *buildslave-remote-end-of-test-output-marker* return-value (format nil "~A" condition) backtrace))))))
