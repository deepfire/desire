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

(defgeneric run-module-test (type module &optional verbose-internals)
  (:method :around (type module &optional verbose-internals)
    (declare (ignore verbose-internals))
    (with-recorded-status (:record-backtrace t)
      (unwind-protect (call-next-method)
        (finish-output *standard-output*)
        (finish-output *error-output*))))
  (:method ((o (eql :master-reachability-phase)) m &optional verbose-internals)
    (declare (ignore verbose-internals))
    (touch-module m))
  (:method ((o (eql :master-update-phase)) m &optional verbose-internals)
    (declare (ignore verbose-internals))
    (let ((*fetch-errors-serious* t))
      (update m)
      t))
  (:method ((o (eql :slave-fetch-phase)) m &optional verbose-internals)
    (declare (ignore verbose-internals))
    (let ((*fetch-errors-serious* t)
          (*break-on-signals* 'error))
      (update m)
      t))
  (:method ((o (eql :slave-recurse-phase)) m &optional verbose-internals)
    (desire (list (name m)) :skip-present t :seal nil :verbose verbose-internals)
    t)
  (:method ((o (eql :slave-load-phase)) m &optional verbose-internals)
    (declare (ignore verbose-internals))
    (asdf:oos 'asdf:load-op (name m))
    t)
  (:method ((o (eql :slave-test-phase)) m &optional verbose-internals)
    (declare (ignore verbose-internals))
    (not-implemented :slave-test-phase)))

(defun buildslave (modules phases &optional verbose-internals)
  (let ((modules (mapcar #'coerce-to-module modules))
        (phases (mapcar (compose #'make-keyword #'string-upcase #'string) phases)))
    (syncformat t "~%~S~%" *buildslave-remote-output-marker*)
    (iter (for p in phases)
          (iter (for m in modules)
                (syncformat t "(:name ~S :mode ~(~S~)~%~S~%" (name m) p *buildslave-remote-test-output-marker*)
                (destructuring-bind (&key return-value condition backtrace) (run-module-test p m verbose-internals)
                  (syncformat t "~:[~*~;~%>>> A condition of type ~A was encountered during execution.~]~%~S~%:status ~S :condition ~S :backtrace ~S)~%"
                              condition (type-of condition) *buildslave-remote-end-of-test-output-marker* return-value (format nil "~A" condition) backtrace))))
    (syncformat t "~%~S~%" *buildslave-remote-end-of-output-marker*)))
