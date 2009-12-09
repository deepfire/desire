;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2009 by
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
(defparameter *asdf-system-blacklist* '("cffi-tests" "trivial-features-tests"))

(defun system-type-to-file-type (type)
  (or (car (rassoc type *system-pathname-typemap* :test #'eq))
      (desire-error "~@<Unknown system type ~S.~:@>" type)))

(defun asdf-system-name-blacklisted-p (name)
  (member name *asdf-system-blacklist* :test #'equal))

;;;;
;;;; Conditions
;;;;
(define-reported-condition system-definition-missing-error (system-error)
  ((path :reader condition-path :initarg :path))
  (:report (system path)
           "~@<Couldn't find the definition for ~S in ~S~:@>" system path))

;;;;
;;;; Generic
;;;;
(defun invoke-with-module-system-definitions-and-blacklists (module repo-dir system-pathname-type fn)
  (let* ((path (truename repo-dir))
         (pass1-pattern (subwild path (module-system-path-whitelist module) :name :wild :type system-pathname-type))
         (pass1 (directory pass1-pattern))
         (blacklist-patterns (list* (subwild path '("test"))
                                    (subwild path '("tests"))
                                    (subwild path '("_darcs"))
                                    (when-let ((blacklist (module-system-path-blacklist module)))
                                      (list (subwild path blacklist))))))
    (funcall fn pass1 blacklist-patterns)))

(defmacro do-module-system-definitions ((pathname module repo-dir system-pathname-type) &body body)
  (with-gensyms (pathnames blacklists)
    `(invoke-with-module-system-definitions-and-blacklists ,module ,repo-dir ,system-pathname-type
                                                           (lambda (,pathnames ,blacklists)
                                                             (flet ((blacklisted-p (x)
                                                                      (some (curry #'pathname-match-p x) ,blacklists)))
                                                               (iter (for ,pathname in ,pathnames)
                                                                     ,@body))))))

(defun system-definition-pathname (system repository &key (if-does-not-exist :error))
  "Return the pathname of a SYSTEM's definition within REPOSITORY."
  (let ((name (or (system-definition-pathname-name system) (system-canonical-definition-name system)))
        (type (system-pathname-type system)))
    (or (file-exists-p (subfile repository (list name) :type type)) ;; Try the fast path first.
        (do-module-system-definitions (path (system-module system) repository type)
          (finding path such-that (and (string= name (pathname-name path))
                                       (not (blacklisted-p path)))))
        (ecase if-does-not-exist
          (:error (error 'system-definition-missing-error :system system :path repository))
          (:continue nil)))))

(defun module-system-definitions (module &optional (type *default-system-type*) (locality (gate *self*)))
  "Return a list of all MODULE's system definition pathnames corresponding to
system TYPE within LOCALITY."
  (do-module-system-definitions (path module (module-pathname module locality) (system-type-to-file-type type))
    (unless (blacklisted-p path)
      (collect path))))

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

;; This is /just/ for two functions below.
;; Use SYSTEM-DEFINITION-REGISTRY-SYMLINK-PATH for your needs.
(defun locality-asdf-registry-path (locality)
  (subdirectory* (locality-pathname locality) ".asdf-registry"))

(defgeneric register-locality-with-system-backend (type locality)
  (:documentation
   "Do the module-agnostic part of the rituals needed for making systems
within LOCALITY loadable using BACKEND-TYPE.  The other, module-specific
part is done by ENSURE-MODULE-SYSTEMS-LOADABLE.")
  (:method ((type (eql 'asdf-system)) locality)
    (pushnew (ensure-directories-exist (locality-asdf-registry-path locality)) asdf:*central-registry* :test #'equal)))

(defun ensure-system-loadable (system &optional path check-path-sanity (locality (gate *self*)))
  "Ensure that SYSTEM is loadable at PATH, which defaults to SYSTEM's 
   definition path within its module within LOCALITY."
  (when-let ((definition-pathname (or path (system-definition-pathname system (module-pathname (system-module system) locality) :if-does-not-exist :continue))))
    (unless (string= (down-case-name system) (pathname-name definition-pathname))
      (when check-path-sanity
        (system-error system "~@<Asked to ensure loadability of system ~A at non-conforming path ~S. Hidden system?~:@>" (name system) definition-pathname)))
    (ensure-symlink (system-definition-registry-symlink-path system locality)
                    definition-pathname)))

(defgeneric system-definition-registry-symlink-path (system &optional locality)
  (:documentation
   "This is functionality badly abstracted.")
  (:method :around (system &optional locality)
    (subfile (call-next-method system locality) (list (down-case-name system)) :type (system-pathname-type system)))
  (:method ((o asdf-system) &optional (locality (gate *self*)))
    (locality-asdf-registry-path locality)))

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

;;;;
;;;; ASDF
;;;;
(defun asdf-hidden-system-names (pathname)
  "Find out names of ASDF systems hiding in .asd in PATHNAME.
A hidden system is a system with a definition residing in a file named
differently from that system's name."
  (let ((primary-system-name (string-upcase (pathname-name pathname)))
        (*read-eval* nil)
        (*readtable* (copy-readtable)))
    (set-dispatch-macro-character #\# #\. (lambda (stream &optional char sharp)
                                            (declare (ignore char sharp))
                                            (read stream)
                                            nil))
    (with-open-file (s pathname)
      (flet ((form-defsystem-p (f)
               (and (consp f)
                    (string= "DEFSYSTEM" (symbol-name (first f)))
                    (or (stringp (second f))
                        (symbolp (second f))))))
        (iter (for form = (read s nil 'das-eof))
              (until (eq 'das-eof form))
              (when (form-defsystem-p form)
                (let ((system-name (string-upcase (string (second form)))))
                  (when (not (string= primary-system-name system-name))
                    (collect system-name)))))))))

;;;
;;; The historic, painful version of the above.
;;;
#+(or)
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
