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

(defun system-type-to-file-type (type)
  (or (car (rassoc type *system-pathname-typemap* :test #'eq))
      (desire-error "~@<Unknown system type ~S.~:@>" type)))

(defparameter *asdf-system-blacklist* '("cffi-tests" "trivial-features-tests"))

(defun asdf-system-name-blacklisted-p (name)
  (member name *asdf-system-blacklist* :test #'equal))

;;;;
;;;; Backends
;;;;
(defun system-type-from-definition (pathname)
  "Detect the type of system definition residing at PATHNAME."
  (or (cdr (assoc (pathname-type pathname) *system-pathname-typemap* :test #'string=))
      (system-error pathname "~@<Couldn't detect type of system in alleged definition at ~S.~:@>" pathname)))

(defgeneric system-name-from-definition (system-type system-definition-pathname)
  (:documentation
   "Return the canonical name of the system defined through SYSTEM-DEFINITION-PATHNAME.
For case-insensitive systems the name is in upper case.")
  (:method ((type (eql 'asdf-system)) pathname)
    (string-upcase (pathname-name pathname))))

(defgeneric system-canonical-definition-name (system)
  (:documentation
   "Return the canonical definition patname name for SYSTEM.")
  (:method ((o asdf-system))
    (downstring (name o))))

(defgeneric register-locality-with-system-backend (type locality)
  (:documentation
   "Do the module-agnostic part of the rituals needed for making systems
within LOCALITY loadable using BACKEND-TYPE.  The other, module-specific
part is done by ENSURE-MODULE-SYSTEMS-LOADABLE.")
  (:method ((type (eql 'asdf-system)) locality)
    (pushnew (ensure-directories-exist (locality-asdf-registry-path locality)) asdf:*central-registry* :test #'equal)))

(defgeneric system-definition-registry-symlink-path (system &optional locality)
  (:documentation
   "This is functionality badly abstracted.")
  (:method :around (system &optional locality)
    (subfile (call-next-method system locality) (list (down-case-name system)) :type (system-pathname-type system)))
  (:method ((o asdf-system) &optional (locality (gate *self*)))
    (subdirectory* (locality-pathname locality) ".asdf-registry")))

(defgeneric system-dependencies (system)
  (:documentation
   "Inspect SYSTEM for systems it depends upon, and return a list of
known system objects as the primary value, and a list of names of unknown
systems as the secondary value.")
  (:method :around ((s system))
           (handler-case (let ((*break-on-signals* nil))
                           (call-next-method))
             (error (c)
               (format t "~@<; ~@;WARNING: error while querying backend of system ~S about its dependencies: ~A~:@>~%" (name s) c))))
  (:method ((s asdf-system) &aux (name (down-case-name s)))
    (unless (asdf-system-name-blacklisted-p name)
      (iter (for depname in (cdr (assoc 'asdf:load-op (asdf:component-depends-on 'asdf:load-op (asdf:find-system name)))))
            (for sanitised-depname = (string-upcase (string (xform-if #'consp #'second depname)))) ;; Drop version on the floor.
            (if-let ((depsystem (system sanitised-depname :if-does-not-exist :continue)))
              (collect depsystem into known-systems)
              (collect sanitised-depname into unknown-names))
            (finally (return (values known-systems unknown-names)))))))

(defgeneric system-loadable-p (system &optional locality)
  (:documentation
   "Determine whether SYSTEM is loadable within LOCALITY.")
  (:method :around ((s system) &optional (locality (gate *self*)))
    (handler-case (let ((*break-on-signals* nil))
                    (call-next-method s locality))
      (error (c)
        (format t "~@<; ~@;WARNING: error while querying backend of system ~S about its loadability: ~A~:@>~%" (name s) c))))
  (:method ((o symbol) &optional (locality (gate *self*)))
    (system-loadable-p (system o) locality))
  (:method ((o asdf-system) &optional (locality (gate *self*)))
    (handler-case (and (equal (symlink-target-file (system-definition-registry-symlink-path o locality))
                              (system-definition-pathname o (module-pathname (system-module o) locality) :if-does-not-exist :continue))
                       (let ((name (down-case-name o)))
                         (or (asdf-system-name-blacklisted-p name)
                             (asdf:find-system name nil))))
      (asdf:missing-dependency () ;; CXML...
        (warn "~@<~S misbehaves: ASDF:MISSING-DEPENDENCY signalled during ASDF:FIND-SYSTEM~:@>" 'system)
        t))))

(defun asdf-hidden-system-names (system &aux (name (name system)))
  "Find out names of ASDF systems hiding in SYSTEM.
A hidden system is a system with a definition residing in a file named
differently from that system's name."
  (let ((orig asdf::*defined-systems*)
        (test (make-hash-table :test 'equalp)))
    (unwind-protect
         (progn
           (setf asdf::*defined-systems* test)
           (handler-case (let ((*break-on-signals* nil)
                               (name (downstring name)))
                           (unless (asdf-system-name-blacklisted-p name)
                             (asdf:find-system name)))
             (error (c)
               (format t "~@<; ~@;WARNING: error while querying ASDF about hidden names of system ~S: ~A~:@>~%" (name system) c)))
           (mapcar (compose #'string-upcase #'asdf:component-name)
                   (remove (string name) (mapcar #'cdr (hash-table-values test))
                           :test #'string= :key (compose #'string-upcase #'asdf:component-name))))
      (setf asdf::*defined-systems* orig))))

;;;;
;;;; Conditions
;;;;
(define-reported-condition module-systems-unloadable-error (module-error)
  ((systems :reader condition-systems :initarg :systems))
  (:report (module systems)
           "~@<Following ~S's systems couldn't be made loadable:~{ ~S~}~:@>" module systems))

(define-reported-condition system-definition-missing-error (system-error)
  ((path :reader condition-path :initarg :path))
  (:report (system path)
           "~@<Couldn't find the definition for ~S in ~S~:@>" system path))

;;;;
;;;; Generic
;;;;
(defun apply-repo-system-filter (repository system-pathnames)
  (remove-if (rcurry #'pathname-match-p (subwild repository '("_darcs"))) system-pathnames))

(defun system-definition-pathname (system repository &key (if-does-not-exist :error))
  "Return the pathname of a SYSTEM's definition within REPOSITORY."
  (let ((name (or (system-definition-pathname-name system) (system-canonical-definition-name system)))
        (type (system-pathname-type system)))
    (or (file-exists-p (subfile repository (list name) :type type)) ;; Try the fast path first.
        (first (apply-repo-system-filter
                repository (directory (subwild repository (or (system-search-restriction system) '("_darcs")) :name name :type type))))
        (ecase if-does-not-exist
          (:error (error 'system-definition-missing-error :system system :path repository))
          (:continue nil)))))

(defun module-system-definitions (module &optional (type *default-system-type*) (locality (gate *self*)))
  "Return a list of all MODULE's system definition pathnames corresponding to
system TYPE within LOCALITY."
  (let* ((module (coerce-to-module module))
         (path (truename (module-pathname module locality)))
         (pass1-pattern (subwild path (module-system-path-whitelist module) :name :wild :type (system-type-to-file-type type)))
         (pass1 (directory pass1-pattern))
         (blacklists (list* (subwild path '("test"))
                            (subwild path '("tests"))
                            (when-let ((blacklist (module-system-path-blacklist module)))
                              (list (subwild path blacklist))))))
    (iter (for p in pass1)
          (when (notany (curry #'pathname-match-p p) blacklists)
            (collect p)))))

(defun ensure-system-loadable (system &optional path check-path-sanity (locality (gate *self*)))
  "Ensure that SYSTEM is loadable at PATH, which defaults to SYSTEM's 
   definition path within its module within LOCALITY."
  (when-let ((definition-pathname (or path (system-definition-pathname system (module-pathname (system-module system) locality) :if-does-not-exist :continue))))
    (unless (string= (down-case-name system) (pathname-name definition-pathname))
      (when check-path-sanity
        (system-error system "~@<Asked to ensure loadability of system ~A at non-conforming path ~S. Hidden system?~:@>" (name system) definition-pathname)))
    (ensure-symlink (system-definition-registry-symlink-path system locality)
                    definition-pathname)))

(defun ensure-module-systems-loadable (module &optional (locality (gate *self*)) &aux (module (coerce-to-module module)))
  "Try making MODULE's systems loadable, defaulting to LOCALITY.
 
   Raise an error of type MODULE-SYSTEMS-UNLOADABLE-ERROR upon failure."
  (dolist (s (module-systems module))
    (ensure-system-loadable s nil (not (system-hidden-p s)) locality)))

(defun module-central-system-name (module)
  "How stupid is that?"
  (name module))
