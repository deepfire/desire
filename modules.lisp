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


(defvar *verbose-repository-maintenance* nil)

(define-reported-condition module-systems-unloadable-error (module-error)
  ((systems :reader condition-systems :initarg :systems))
  (:report (module systems)
           "~@<Following ~S's systems couldn't be made loadable:~{ ~S~}~:@>" module systems))

(defun ensure-module-systems-loadable (module &optional (locality (gate *self*)) &aux
                                       (module (coerce-to-module module)))
  "Try making MODULE's systems loadable, defaulting to LOCALITY.
Raise an error of type MODULE-SYSTEMS-UNLOADABLE-ERROR upon failure."
  (dolist (s (module-systems module))
    (when *verbose-repository-maintenance*
      (format t "~@<;;; ~@;Ensuring loadability of ~A ~A~:@>~%" (type-of s) (name s)))
    (ensure-system-loadable s nil (not (system-hidden-p s)) locality)))

(defun module-central-system-name (module)
  "How stupid is that?"
  (name module))

(defgeneric notice-module-repository (module &optional locality)
  (:documentation
   "Ensure that the repository corresponding to MODULE within LOCALITY
meets desire's operational requirements.")
  (:method ((o module) &optional (locality (gate *self*)))
    (let ((repo-dir (module-pathname o locality)))
      (when *verbose-repository-maintenance*
        (format t "~@<;; ~@;Processing ~A's repository at ~S~:@>~%" (name o) repo-dir))
      (ensure-tracker-branch repo-dir (ref-value "master" repo-dir))
      (ensure-module-systems-loadable o locality))))
