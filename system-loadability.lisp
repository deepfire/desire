;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :desire)

(defun system-definition-path (system &optional (locality (master 'git)))
  (let* ((system (coerce-to-system system))
         (name (down-case-name system)))
    (values
     (subfile (locality-path locality) (append (list (down-case-name (system-module system))) (system-relativity system) (list name)) :type "asd")
     (subfile (git-locality-asdf-registry-path locality) (list name) :type "asd"))))

(defun system-loadable-p (system)
  "See whether SYSTEM is loadable by the means of ASDF."
  (handler-case (multiple-value-bind (system-asd asd-symlink) (system-definition-path system)
                  (and (equal (symlink-target-file-present-p asd-symlink) system-asd)
                       (asdf:find-system (coerce-to-name system) nil)))
    (asdf:missing-dependency () ;; CXML...
      (warn "~@<~S misbehaves: ASDF:MISSING-DEPENDENCY during ASDF:FIND-SYSTEM~:@>" 'system)
      t)))

(define-reported-condition module-systems-unloadable-error (desire-error)
  ((module :accessor module-system-unloadable-error-module :initarg :module)
   (systems :accessor module-system-unloadable-error-systems :initarg :systems))
  (:report (module systems)
           "~@<Following ~S's systems couldn't be made loadable:~{ ~S~}~:@>"
           module systems))

(defun ensure-system-loadable (system &optional (locality (master 'git)))
  (multiple-value-call (order ensure-symlink 1 0) (system-definition-path system locality)))

(defun ensure-module-systems-loadable (module &optional (locality (master 'git)) &aux (module (coerce-to-module module)))
  "Try making MODULE's systems loadable, defaulting to LOCALITY.
 
   Raise an error of type MODULE-SYSTEMS-UNLOADABLE-ERROR upon failure."
  (mapc (rcurry #'ensure-system-loadable locality) (module-systems module))
  (unless (every #'system-loadable-p (module-systems module))
    (with-ignore-restart (continue () :report "Accept system unloadability.")
      (error 'module-systems-unloadable-error :module module :systems (remove-if #'system-loadable-p (module-systems module))))))
