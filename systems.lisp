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

(defgeneric system-canonical-definition-pathname-name (system)
  (:documentation
   "Return the canonical definition patname name for SYSTEM.
Only used in COMPUTE-SYSTEM-DEFINITION-PATHNAME.")
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

(defgeneric ensure-system-loadable (system &optional path locality)
  (:documentation
   "Ensure that SYSTEM is loadable at PATH, which defaults to SYSTEM's 
definition path within its module within LOCALITY.")
  (:method ((o asdf-system) &optional path (locality (gate *self*)))
    (when-let ((definition-pathname (or path (system-definition-pathname o))))
      (ensure-symlink (system-definition-registry-symlink-path o locality)
                      definition-pathname))))

(defgeneric system-definition-registry-symlink-path (system &optional locality)
  (:documentation
   "This is functionality badly abstracted.")
  (:method :around (system &optional locality)
    (subfile (call-next-method system locality) (list (down-case-name system)) :type (system-pathname-type system)))
  (:method ((o asdf-system) &optional (locality (gate *self*)))
    (locality-asdf-registry-path locality)))

(defgeneric compute-direct-system-dependencies (system)
  (:documentation
   "Return a list of names of systems SYSTEM depends upon.")
  (:method :around ((s system))
           (handler-case (let ((*break-on-signals* nil))
                           (call-next-method))
             (error (c)
               (format t "~@<; ~@;WARNING: error while querying backend of system ~S about its dependencies: ~A~:@>~%" (name s) c)))) ;
  (:method ((s asdf-system) &aux (name (down-case-name s)))
    (iter (for depname in (cdr (assoc 'asdf:load-op (asdf:component-depends-on 'asdf:load-op (asdf:find-system name)))))
          (destructuring-bind (name &optional version) (if (consp depname)
                                                           (list (second depname) (first depname))
                                                           (list depname))
            (declare (ignore version))
            (collect (canonicalise-name name))))))

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
    (and (equal (symlink-target-file (system-definition-registry-symlink-path o locality))
                (system-definition-pathname o))
         (let ((name (down-case-name o)))
           (or (asdf-system-name-blacklisted-p name)
               (asdf:find-system name nil))))))

;;;;
;;;; Dependencies
;;;;
(defgeneric recompute-direct-system-dependencies-one (system)
  (:method ((o system))
    (setf (slot-value o 'direct-dependency-names) (compute-direct-system-dependencies o))))

(defgeneric system-dependencies-up-to-date-p (system)
  (:method ((o system))
    (= (file-write-date (system-definition-pathname o)) (system-definition-write-date o))))

(defgeneric direct-system-dependencies (system)
  (:method ((o system))
    (if (system-dependencies-up-to-date-p o)
        (system-direct-dependency-names o)
        (recompute-direct-system-dependencies-one o))))

(defun recompute-direct-system-dependencies ()
  (do-present-systems (s)
    (recompute-direct-system-dependencies-one s)))

(defun recompute-full-system-dependencies-set (systems)
  (let ((present-systems (do-present-systems (s) (collect s)))
        (sys (make-hash-table :test 'eq))
        (removed-links (make-hash-table :test 'eq)))
    (with-container sys (sys :type system)
      (with-container removed-links (removed-links :type list :iterator do-removed-links :iterator-bind-key t)
        (dolist (s present-systems)
          (setf (sys (name s)) s))
        (labels ((do-calc-sysdeps (depstack s)
                   ;; XXX: ensure-slot-value special form ?
                   (cond ((slot-boundp s 'dependencies)
                          (slot-value s 'dependencies))
                         ((member s depstack) ; dependency loop?
                          (push (name s) (removed-links (first depstack)))
                          (deletef (slot-value (first depstack) 'direct-dependency-names) (name s))
                          ;; not re-adding dependencies
                          nil)
                         (t
                          (setf (slot-value s 'dependencies)
                                (apply #'append (curry #'do-calc-sysdeps (cons s depstack))
                                       (mapcar (rcurry #'sys) (system-direct-dependency-names s)))))))
                 (sysdeps (s)
                   (unwind-protect (do-calc-sysdeps nil s)
                     (do-removed-links (from to-names)
                       (nconcf (slot-value from 'direct-dependency-names) to-names))
                     (clrhash removed-links))))
          (dolist (s (if (eq systems t)
                         present-systems
                         systems))
            (sysdeps s))
          (dolist (s present-systems)
            (let ((known-deps (mapcar (rcurry #'system :if-does-not-exist :continue) (system-dependencies s))))
              (setf (slot-value s 'definition-complete-p) (not (find nil known-deps))))))))))

(defun recompute-full-system-dependencies ()
  (recompute-full-system-dependencies-set t))

;;;;
;;;; o/~ Below zero, below my need for words o/~
;;;; o/~ Feel you lifeform, not human. o/~
;;;; ...
;;;; o/~ Of all the big mistakes I've done o/~
;;;; o/~ The small ones will remain... o/~
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
        (iter (for form = (handler-case (read s nil 'das-eof)
                            (serious-condition ())))
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
