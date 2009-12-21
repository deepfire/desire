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

(defgeneric drop-system-backend-definition-cache (type)
  (:method ((o (eql 'asdf-system)))
    (clrhash asdf::*defined-systems*)))

;; This is /just/ for two functions below.
;; Use SYSTEM-DEFINITION-REGISTRY-SYMLINK-PATH for your needs.
(defun locality-asdf-registry-path (locality)
  (subdirectory* (locality-pathname locality) ".asdf-registry"))

(defgeneric register-locality-with-system-backend (type locality)
  (:documentation
   "Do the module-agnostic part of the rituals needed for making systems
within LOCALITY loadable using BACKEND-TYPE.  The other, module-specific
part is done by ENSURE-SYSTEM-LOADABLE.")
  (:method ((type (eql 'asdf-system)) locality)
    (pushnew (ensure-directories-exist (locality-asdf-registry-path locality)) asdf:*central-registry* :test #'equal)))

(defgeneric ensure-system-loadable (system &optional locality path)
  (:documentation
   "Ensure that SYSTEM is loadable at PATH, which defaults to SYSTEM's 
definition path within its module within LOCALITY.")
  (:method :before ((o system) &optional locality path)
    (declare (ignore path locality))
    (unless (system-locally-present-p o)
      (system-error o "~@<Cannot ensure loadability of a non-locally-present system ~A.~:@>"
                    (name o))))
  (:method ((o asdf-system) &optional (locality (gate *self*)) path)
    (let ((definition-pathname (or path (system-definition-pathname o))))
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
  (:method ((o host-system)))
  (:method ((s asdf-system))
    (asdf-system-dependencies s)))

(defgeneric system-loadable-p (system &optional locality)
  (:documentation
   "Determine whether SYSTEM is loadable within LOCALITY.")
  (:method :around ((s system) &optional (locality (gate *self*)))
     (or (system-host-p s)
         (and (system-locally-present-p s)
              (call-next-method s locality))))
  (:method ((o symbol) &optional (locality (gate *self*)))
    (system-loadable-p (system o) locality))
  (:method ((o host-system) &optional locality)
    (declare (ignore locality))
    t)
  (:method ((o asdf-system) &optional (locality (gate *self*)))
    (equal (symlink-target-file (system-definition-registry-symlink-path o locality))
           (system-definition-pathname o))))

;;;;
;;;; Dependencies
;;;;
(defun drop-system-caches (&optional (system-type *default-system-type*))
  "Clear system presence and dependency caches, as well as that of
the system definition backend for SYSTEM-TYPE."
  (do-systems (s)
    (when (and (system-known-p s)
               (not (system-host-p s)))
      (system-makunpresent s)))
  (drop-system-backend-definition-cache system-type))

(defun ensure-host-system (name)
  (if-let ((present (system name :if-does-not-exist :continue)))
    (unless (system-host-p present)
      (system-error present "~@<Asked to facilitate ~A as a host system, while it's already an ~A.~:@>"
                    name (type-of present)))
    (make-instance 'host-system :name (canonicalise-name name))))

(defgeneric notice-system-definition (system pathname)
  (:method ((o known-system) (p pathname))
    (setf (slot-value o 'definition-pathname) p
          (slot-value o 'definition-write-date) (file-write-date p))))

(defgeneric system-dependencies-up-to-date-p (system)
  (:method ((o host-system)) t)
  (:method ((o known-system))
    (unless (system-locally-present-p o)
      (system-error o "~@<Asked to determine up-to-date-ness of a non-locally-present system ~A.~:@>"
                    (name o)))
    (= (file-write-date (system-definition-pathname o)) (system-definition-write-date o))))

(defgeneric direct-system-dependencies (system &key force-recompute)
  (:method ((o known-system) &key force-recompute)
    (if (and (slot-boundp o 'direct-dependency-names)
             (not force-recompute)
             (system-dependencies-up-to-date-p o))
        (system-direct-dependency-names o)
        (setf (slot-value o 'direct-dependency-names) (compute-direct-system-dependencies o)))))

(defun update-system-set-dependencies (systems &key force-recompute verbose)
  "Complete full system dependency caches for every system in SYSTEMS.
The value returned is a boolean, which is true if all systems' dependencies
are complete.
When FORCE-RECOMPUTE is non-NIL full system dependency caches are recomputed."
  (let ((removed-links (make-hash-table :test 'eq)))
    (with-container removed-links (removed-links :type list :iterator do-removed-links :iterator-bind-key t)
      (labels ((do-calc-sysdeps (depstack s)
                 ;; XXX: ensure-slot-value special form ?
                 (with-slots (name direct-dependency-names dependencies definition-complete-p) s
                   (when verbose
                     (format t "computing dependencies of ~A~%" (name s)))
                   (cond ((member s depstack) ; dependency loop?
                          (when verbose
                            (format t "...dependency loop: ~A~%" (mapcar #'name depstack)))
                          (push name (removed-links (first depstack)))
                          (deletef (slot-value (first depstack) 'direct-dependency-names) name)
                          ;; not re-adding dependencies
                          ;; NOTE: what about incompleteness propagation?
                          (values t nil))
                         ((and (slot-boundp s 'dependencies) ; cached?
                               definition-complete-p
                               (or (system-host-p s) ; it's constant
                                   (not force-recompute)))
                          (when verbose
                            (format t "...already computed, returning ~:[in~;~]complete, ~A~%"
                                    definition-complete-p (mapcar #'name dependencies)))
                          (values definition-complete-p
                                  dependencies))
                         ((system-locally-present-p s) ; available?
                          (when verbose
                            (format t "...computing~%"))
                          (setf dependencies
                                (delete-duplicates
                                 (iter (for depname in direct-dependency-names)
                                       (for depsys = (system depname :if-does-not-exist :continue))
                                       (cond
                                         ((not (and depsys (system-known-p depsys)))
                                          ;; There are three reasons for system's definitions to be incomplete.
                                          ;; Martian (i.e. not mentioned in DEFINITIONS) dependencies is the first one.
                                          (setf definition-complete-p nil)
                                          (unless depsys
                                            (make-instance 'unknown-system :name depname))
                                          (nconcing (list depsys)))
                                         (t
                                          (multiple-value-bind (complete-p deps) (do-calc-sysdeps (cons s depstack) depsys)
                                            ;; Incomplete dependencies being contagious are the second reason.
                                            (setf definition-complete-p complete-p)
                                            (nconcing (cons depsys (copy-list deps)))))))))
                          (unless direct-dependency-names ; no deps, no problems
                            (setf definition-complete-p t))
                          (when verbose
                            (format t "computed dependencies of ~A: ~:[in~;~]complete, ~A~%"
                                    (name s) definition-complete-p (mapcar #'name dependencies)))
                          (values definition-complete-p
                                  dependencies))
                         (t
                          (when verbose
                            (format t "...not locally present, skipping~%"))
                          ;; System not being locally present is the third reason.
                          (values (setf definition-complete-p nil)
                                  nil)))))
               (sysdeps (s)
                 (unwind-protect (do-calc-sysdeps nil s)
                   ;; reinstate loops..
                   (do-removed-links (from to-names)
                     (nconcf (slot-value from 'direct-dependency-names) to-names))
                   (clrhash removed-links))))
        (lret ((set (if (eq systems t)
                        (do-present-systems (s)
                          (unless (system-host-p s)
                            (collect s)))
                        systems))
               (complete-p t)) ; the empty set has complete definitions :-)
          (dolist (s set)
            (andf complete-p (sysdeps s))))))))

(defun compute-full-system-dependencies (system &key force-recompute verbose &aux
                                         (system (coerce-to-system system)))
  (let ((complete-p (update-system-set-dependencies (list system)
                                                    :force-recompute force-recompute
                                                    :verbose verbose)))
    (values (when complete-p
              (system-dependencies system))
            complete-p)))

;;;;
;;;; o/~ Below zero, below my need for words o/~
;;;; o/~ Feel you lifeform, not human. o/~
;;;; ...
;;;; o/~ Of all the big mistakes I've done o/~
;;;; o/~ The small ones will remain... o/~
;;;;
(defun map-asd-defsystems (stream fn)
  (with-safe-reader-context ()
    (flet ((form-defsystem-p (f)
             (and (consp f)
                  (string= "DEFSYSTEM" (symbol-name (first f)))
                  (or (stringp (second f))
                      (symbolp (second f))))))
      (iter (for form = (handler-case (read stream nil 'das-eof)
                          (serious-condition ())))
            (until (eq 'das-eof form))
            (when (form-defsystem-p form)
              (collect (funcall fn form)))))))

(defmacro do-asd-defsystems ((form stream) &body body)
  `(map-asd-defsystems ,stream (lambda (,form) ,@body)))

(defun normalise-asdf-sysdep (dep)
  "Given an ASDF system dependency, normalise it by returning the name depended upon
as the primary value, and the required version, whenever present as the secondary value."
  (if (consp dep)
      (values (second dep) (first dep))
      dep))

(defun asdf-system-dependencies (system)
  "Parse an .asd as if it were declarative."
  (with-open-file (s (system-definition-pathname system))
    (apply #'nconc
           (do-asd-defsystems (form s)
             (destructuring-bind (defsystem name &key depends-on &allow-other-keys) form
               (declare (ignore defsystem))
               (when (string-equal name (name system))
                 (mapcar (compose #'canonicalise-name #'normalise-asdf-sysdep) depends-on)))))))

(defun asdf-hidden-system-names (pathname)
  "Find out names of ASDF systems hiding in .asd in PATHNAME.
A hidden system is a system with a definition residing in a file named
differently from that system's name."
  (let ((primary-system-name (string-upcase (pathname-name pathname))))
    (with-open-file (s pathname)
      (remove nil
              (do-asd-defsystems (form s)
                (let ((system-name (string-upcase (string (second form)))))
                  (when (not (string= primary-system-name system-name))
                    system-name)))))))

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
