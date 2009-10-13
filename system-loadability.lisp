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

(defvar *system-pathname-typemap* '(("asd" . asdf-system) ("mb" . mudball-system) ("xcvb" . xcvb-system))
  "The mapping between SYSTEM subclasses and definition pathname types.")

;;;;
;;;; Backends
;;;;
(defgeneric system-dependencies (s)
  (:method ((s xcvb-system)))
  (:method ((s mudball-system)))
  (:method ((s asdf-system))
    (iter (for depname in (cdr (assoc 'asdf:load-op (asdf:component-depends-on 'asdf:load-op (asdf:find-system (down-case-name s))))))
          (for sanitised-depname = (string-upcase (string (xform-if #'consp #'second depname)))) ;; Drop version on the floor.
          (if-let ((depsystem (system sanitised-depname :if-does-not-exist :continue)))
            (collect depsystem into known-systems)
            (collect sanitised-depname into unknown-names))
          (finally (return (values known-systems unknown-names))))))

(defgeneric system-definition-registry-symlink-path (system &optional locality)
  (:method :around (system &optional locality)
    (subfile (call-next-method system locality) (list (down-case-name system)) :type (system-pathname-type system)))
  (:method ((o asdf-system) &optional (locality (gate *self*)))
    (locality-asdf-registry-path locality)))

(defgeneric system-loadable-p (system-or-name &optional locality)
  (:method ((o symbol) &optional (locality (gate *self*)))
    (call-next-method (coerce-to-system o) locality))
  (:method ((o asdf-system) &optional (locality (gate *self*)))
    (handler-case (and (equal (symlink-target-file (system-definition-registry-symlink-path o locality))
                              (system-definition o (module-pathname (system-module o) locality) :if-does-not-exist :continue))
                       (asdf:find-system (down-case-name o) nil))
      (asdf:missing-dependency () ;; CXML...
        (warn "~@<~S misbehaves: ASDF:MISSING-DEPENDENCY during ASDF:FIND-SYSTEM~:@>" 'system)
        t))))

(defun asdf-hidden-system-names (system &aux (name (name system)))
  "Find out names of ASDF systems hiding in SYSTEM, which mustn't have been 
seen yet.
A hidden system is a system with a definition residing in a file named
differently from that system's name."
  (let ((pre (hash-table-values asdf::*defined-systems*)))
    (asdf:find-system name)
    (mapcar (compose #'string-upcase #'asdf:component-name)
            (remove (string name) (mapcar #'cdr (set-difference (hash-table-values asdf::*defined-systems*) pre))
                    :test #'string= :key (compose #'string-upcase #'asdf:component-name)))))

;;;;
;;;; Conditions
;;;;
(define-condition system-condition (desire-condition)
  ((systems :accessor condition-systems :initarg :systems)))

(define-reported-condition module-systems-unloadable-error (desire-error system-condition)
  ((module :accessor module-system-unloadable-error-module :initarg :module))
  (:report (module systems)
           "~@<Following ~S's systems couldn't be made loadable:~{ ~S~}~:@>" module systems))

(define-reported-condition system-definition-missing-error (desire-error system-condition)
  ((path :accessor system-definition-missing-error-path :initarg :path))
  (:report (systems path)
           "~@<Couldn't find the definition for ~S in ~S~:@>" (first systems) path))

;;;;
;;;; Generic
;;;;
(defun system-pathname-name (pathname)
  "Return the canonical name for a system residing in PATHNAME."
  (string-upcase (pathname-name pathname)))

(defun interpret-system-pathname-type (pathname)
  "Return the canonical type for a system residing in PATHNAME."
  (or (cdr (assoc (pathname-type pathname) *system-pathname-typemap* :test #'string=))
      (error "~@<Bad pathname type ~S: want one of ~S~:@>" (pathname-type pathname) (mapcar #'car *system-pathname-typemap*))))

(defun apply-repo-system-filter (repository system-pathnames)
  (remove-if (rcurry #'pathname-match-p (subwild repository '("_darcs"))) system-pathnames))

(defun system-definition (system repository &key (if-does-not-exist :error))
  "Return the pathname of a SYSTEM's definition within REPOSITORY."
  (let ((name (or (system-definition-pathname-name system) (system-definition-canonical-pathname-name system)))
        (type (system-pathname-type system)))
    (or (file-exists-p (subfile repository (list name) :type type)) ;; Try the fast path first.
        (first (apply-repo-system-filter
                repository (directory (subwild repository (or (system-search-restriction system) '("_darcs")) :name name :type type))))
        (ecase if-does-not-exist
          (:error (error 'system-definition-missing-error :systems (list system) :path repository))
          (:continue nil)))))

(defun system-definitions (repository type)
  "Return a list of all system definition pathnames of TYPE within REPOSITORY."
  (apply-repo-system-filter
   repository (directory (subwild repository nil :name :wild :type (car (rassoc type *system-pathname-typemap* :test #'eq))))))

(defun ensure-system-loadable (system &optional path (locality (gate *self*)))
  "Ensure that SYSTEM is loadable at PATH, which defaults to SYSTEM's 
   definition path within its module within LOCALITY."
  (when-let ((definition-pathname (or path (system-definition system (module-pathname (system-module system) locality) :if-does-not-exist :continue))))
    (ensure-symlink (system-definition-registry-symlink-path system locality)
                    definition-pathname)))

(defun ensure-module-systems-loadable (module &optional (locality (gate *self*)) &aux (module (coerce-to-module module)))
  "Try making MODULE's systems loadable, defaulting to LOCALITY.
 
   Raise an error of type MODULE-SYSTEMS-UNLOADABLE-ERROR upon failure."
  (mapc (rcurry #'ensure-system-loadable nil locality) (module-systems module)))
