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

(defun discover-and-register-module-systems (module &optional verbose (system-type *default-system-type*) (locality (gate *self*)) &aux
                                             (module (coerce-to-module module)))
  (let* ((sysfiles (module-system-definitions module system-type locality))
         (sysnames (mapcar (curry #'system-name-from-definition system-type) sysfiles)))
    (labels ((check-present-system-sanity (system)
               (unless (typep system system-type)
                 (system-error system "~@<During system discovery in module ~A: asked for a system ~S of type ~S, got one of type ~S~:@>"
                               system-type (name system) (type-of system)))
               (unless (eq module (system-module system))
                 (system-error system "~@<During system discovery in module ~A: found a definition for system ~A from module ~A.~:@>"
                               (name module) (name system) (name (system-module system)))))
             (register-new-system (name path)
               (syncformat t "~@<;; ~;Registering a previously unknown system ~A at ~S~:@>~%" name path)
               (setf *unsaved-definition-changes-p* t)
               (make-instance system-type :name name :module module
                              :definition-pathname-name (when-let* ((pathname-name (pathname-name path))
                                                                    (hidden-p (not (equal pathname-name (downstring name)))))
                                                          pathname-name)))
             (ensure-system (name path)
               (lret ((system (or (lret ((present-system (system name :if-does-not-exist :continue)))
                                    (when present-system
                                      (check-present-system-sanity present-system)))
                                  (register-new-system (intern (string-upcase name)) path))))
                 (setf (system-pathname system) path)
                 (ensure-system-loadable system path nil locality))))
      (iter (for path in sysfiles)
            (for name in sysnames)
            (let ((type (system-type-from-definition path)))
              (unless (eq type system-type)
                (recursor-error "~@<While operating in ~A mode, encountered an ~A at ~S.~:@>" system-type type path))
              (when verbose
                (format t "~@<;;;; ~@;Adding visible system ~A at ~S, and its hidden systems.~:@>~%" name path))
              (let ((system (ensure-system name path)))
                (collect (list* system
                                ;; A hidden system is a system definition residing in a file named differently from main system's name.
                                ;; Find them.
                                (when (typep system 'asdf-system)
                                  (let* ((raw-hidden-system-names (asdf-hidden-system-names path))
                                         ;; this is only useful for the LOAD-based legacy method of hidden system name discovery
                                         (raw-hidden-system-names-minus-known (set-difference raw-hidden-system-names sysnames :test #'equal)))
                                    (iter (for hidden-system-name in raw-hidden-system-names-minus-known)
                                          (when verbose
                                            (format t "~@<;;;; ~@;Processing hidden system ~A at ~S.~:@>~%" hidden-system-name path))
                                          (collect (ensure-system hidden-system-name path)))))))))))))

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
