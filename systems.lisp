;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
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

(define-reported-condition undeclared-system-dependency (system-error)
  ((guilty-set :reader condition-guilty-set :initarg :guilty-set))
  (:report (system guilty-set)
           "~@<Undeclared dependency for system ~A somewhere among systems ~A.~:@>"
           (name system) (mapcar #'name guilty-set)))

;;;;
;;;; Generic
;;;;
(defun invoke-with-source-registry-change (type fn)
  (unwind-protect (let ((*source-registry-update-pending* t))
                    (funcall fn))
    (unless *source-registry-update-pending*
      (reread-system-backend-source-registry type))))

(defmacro with-source-registry-change (type &body body)
  `(invoke-with-source-registry-change ,type (lambda () ,@body)))

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

(defgeneric reread-system-backend-source-registry (type)
  (:documentation
   "Ensure that the runtime configuration of TYPE backend corresponds
with its on-disc configuration.")
  (:method ((type (eql 'asdf-system)))
    (asdf:initialize-source-registry)))

(defgeneric drop-system-backend-definition-cache (type)
  (:method ((o (eql 'asdf-system)))
    (clrhash asdf::*defined-systems*)))

;; This is /just/ for two functions below.
(defun locality-asdf-registry-path (locality)
  (merge-pathnames ".asdf-registry/" (locality-pathname locality)))

(defgeneric register-locality-with-system-backend (type locality)
  (:documentation
   "Do the module-agnostic part of the rituals needed for making systems
within LOCALITY loadable using BACKEND-TYPE.  The other, module-specific
part is done by ENSURE-MODULE-SYSTEMS-LOADABLE.")
  (:method :after (type locality)
    (reread-system-backend-source-registry type))
  (:method ((type (eql 'asdf-system)) locality &aux
            (asdf-user-config-directory (merge-pathnames ".config/common-lisp/source-registry.conf.d/"
                                                         (user-homedir-pathname)))
            (desire-asdf-source-location-registry (merge-pathnames "50-desire.conf" asdf-user-config-directory))
            (system-registry-path (locality-asdf-registry-path locality)))
    (flet ((takeover-config ()
             (with-output-to-file (f desire-asdf-source-location-registry)
               (write `(:include ,system-registry-path) :stream f))))
      (ensure-directories-exist asdf-user-config-directory)
      (ensure-directories-exist system-registry-path)
      ;; Preserve the existing file, whenever its first form is what we want.
      (if-let ((form (and (file-exists-p desire-asdf-source-location-registry)
                          (with-open-file (f desire-asdf-source-location-registry :if-does-not-exist nil)
                            (when f (let ((*read-eval* nil)) (read f)))))))
        (destructuring-bind (option maybe-stored-system-registry-path) form
          (if (eq option :include)
              (unless (equal maybe-stored-system-registry-path system-registry-path)
                (syncformat "~@<;; ~@;Taking over source registry entry ~S.~_Redirecting source registry from ~S to ~S.~:@>~%"
                            desire-asdf-source-location-registry maybe-stored-system-registry-path system-registry-path))
              (takeover-config)))
        (takeover-config)))))

(defgeneric ensure-module-systems-loadable-using-backend-of-system (module system)
  (:documentation
   "Ensure that MODULE's systems are loadable via backend of SYSTEM.")
  (:method :around (o s)
    (with-source-registry-change (type-of s)
      (call-next-method)))
  (:method ((o module) (s asdf-system) &aux
            (systems (module-systems o)))
    (let* ((directories (mapcar (lambda (x) (make-pathname :name nil :type nil :defaults x))
                                (sort (remove-duplicates (mapcar #'system-definition-pathname systems)
                                                         :test (lambda (x y) (equal (pathname-directory x)
                                                                                    (pathname-directory y))))
                                      #'pathname<)))
           (locality (gate *self*))
           (registry-path (locality-asdf-registry-path locality)))
      (ensure-directories-exist registry-path)
      (with-output-to-file (f (merge-pathnames (strconcat* "50-" (down-case-name o) ".conf") registry-path))
        (dolist (d directories)
          (format f "~S~%" `(:directory ,d)))))))

(defgeneric mark-system-loaded (system)
  (:documentation
   "Ensure that SYSTEM is registered as loaded within its backend.")
  (:method ((o asdf-system))
    (let ((system (asdf:find-system (name o))))
      (asdf::register-system (name o) system)
      (setf (gethash 'asdf:load-op (asdf::component-operation-times system))
            (get-universal-time)))))

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
    (not (not (ignore-errors (asdf:find-system (name o)))))))

(defgeneric load-system (system &optional missing-dependency-handler)
  (:method ((o asdf-system) &optional missing-dependency-handler)
    (let (last-missing-system)
      (tagbody :retry
         (flet ((try-recover-system (name &aux
                                          (name (canonicalise-name name)))
                  (let ((missing-system (ensure-system name)))
                    (format t "~@<;; ~@;Attempting to recover an unloadable system ~A.~:@>~%" name)
                    (unless (system-known-p missing-system)
                      (system-error missing-system "~@<Failed to recover system ~A: not known.~:@>" name))
                    (when (eq missing-system last-missing-system)
                      (system-error missing-system "~@<Failed to recover system ~A: missing dependency handler failed to improve situation.~:@>" name))
                    (setf last-missing-system missing-system)
                    (funcall missing-dependency-handler missing-system)
                    (go :retry))))
           (handler-case (asdf:operate 'asdf:load-op (name o))
             (asdf:load-system-definition-error (c)
               (try-recover-system (asdf:error-name c)))
             (asdf:missing-dependency (c)
               (try-recover-system (asdf::missing-requires c)))))))))

;;;;
;;;; Dependencies
;;;;
(defun ensure-system (name)
  (or (system name :if-does-not-exist :continue)
      (make-instance 'unknown-system :name name)))

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
  (let ((removed-links (make-hash-table :test 'eq))
        (recomputedp (make-hash-table :test 'eq)))
    (with-container removed-links (removed-links :type list :iterator do-removed-links :iterator-bind-key t)
      (with-container recomputedp (recomputedp :type boolean :if-does-not-exist :continue)
        (labels ((do-calc-sysdeps (depstack s)
                   ;; XXX: ensure-slot-value special form ?
                   (with-slots (name direct-dependency-names dependencies definition-complete-p) s
                     (when verbose
                       (format t ";;;; computing deps of ~A for~{ ~A~}~%" name (mapcar #'name depstack)))
                     (cond ((member s depstack) ; dependency loop?
                            (when verbose
                              (format t "~@<;;;; ~@;...dependency loop: ~A~:@>~%" (mapcar #'name depstack)))
                            (push name (removed-links (first depstack)))
                            (deletef (slot-value (first depstack) 'direct-dependency-names) name)
                            ;; not re-adding dependencies
                            ;; NOTE: what about incompleteness propagation?
                            (values t nil))
                           ((and (slot-boundp s 'dependencies) ; cached?
                                 definition-complete-p
                                 (or (system-host-p s) ; it's constant
                                     (not force-recompute)
                                     (recomputedp s)))
                            (when verbose
                              (format t "~@<;;;;;; ~@;...already computed, returning ~:[in~;~]complete,~
                                                      ~:[ leaf~;~%~10T~:*~A~]~:@>~%"
                                      definition-complete-p (mapcar #'name dependencies)))
                            (values definition-complete-p
                                    dependencies))
                           ((system-locally-present-p s) ; available?
                            (when verbose
                              (format t ";;;; ...computing, directs: ~A~%" direct-dependency-names))
                            (setf dependencies
                                  (delete-duplicates
                                   (iter (for depname in direct-dependency-names)
                                         (for depsys = (ensure-system depname))
                                         (when verbose
                                           (format t ";;;;;; processing direct ~A, ~
                                                             ~:[missing~;present, ~:[un~;~]known~]~%"
                                                   depname depsys (and depsys (system-known-p depsys))))
                                         (nconcing
                                          (cond
                                            ((not (system-known-p depsys))
                                             ;; There are three reasons for system's definitions to be incomplete.
                                             ;; Martian (i.e. not mentioned in DEFINITIONS) dependencies is the first one.
                                             (setf definition-complete-p nil)
                                             (list depsys))
                                            (t
                                             (multiple-value-bind (complete-p deps) (do-calc-sysdeps (cons s depstack) depsys)
                                               ;; Incomplete dependencies being contagious are the second reason.
                                               (setf definition-complete-p complete-p)
                                               (cons depsys (copy-list deps))))))))
                                  (recomputedp s) t)
                            (unless direct-dependency-names ; no deps, no problems
                              (setf definition-complete-p t))
                            (when verbose
                              (format t "~@<;;;; ~@;...computed: ~A~:@>~%"
                                      (mapcar (curry #'xform-if #'identity #'name) dependencies))
                              (format t "~@<;;;;;; ~@;computed dependencies of ~A: ~:[in~;~]complete,~
                                                      ~:[ leaf~;~%~10T~:*~A~]~:@>~%"
                                      name definition-complete-p (mapcar #'name dependencies)))
                            (values definition-complete-p
                                    dependencies))
                           (t
                            (when verbose
                              (format t ";;;; ...not locally present, skipping~%"))
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
            (when verbose
              (format t "~@<;;; ~@;computing dependencies of ~A~:@>~%"
                      (mapcar #'name set)))
            (dolist (s set)
              (when verbose
                (format t "~@<;;; ~@;processing ~A~:@>~%" (name s)))
              (setf complete-p (and (sysdeps s) complete-p))
              (when verbose
                (format t "~@<;;; ~@;done with ~A~:@>~%" (name s))))))))))

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
  (flet ((form-defsystem-p (f)
           (and (consp f)
                (string= "DEFSYSTEM" (symbol-name (first f)))
                (or (stringp (second f))
                    (symbolp (second f))))))
    (iter (for pre-read-posn = (file-position stream))
          (for form-nr from 0)
          (for form = (block nil
                        (handler-bind
                            ((cl-reader:symbol-in-missing-package-error
                              (lambda (cond)
                                (declare (ignore cond))
                                (invoke-restart (find-restart 'cl-reader:return-uninterned))))
                             (cl-reader:symbol-missing-in-package-error
                              (lambda (cond)
                                (declare (ignore cond))
                                (invoke-restart (find-restart 'make-symbol))))
                             (serious-condition
                              (lambda (cond)
                                (format t "~&~@<;;; ~@;got ~S, while parsing AS definition from ~S:~_~A~@:>~%"
                                        cond stream cond)
                                (let ((cl-reader:*readtable* (cl-reader:copy-readtable)))
                                  ;; (cl-reader:set-syntax-from-char #\: #\Space)
                                  (cl-reader:set-dispatch-macro-character #\# #\? (lambda (s c i) (declare (ignore c i)) (cl-reader:read s nil 'das-eof t) nil))
                                  (cl-reader:set-dispatch-macro-character #\# #\. (lambda (s c i) (declare (ignore c i)) (cl-reader:read s nil 'das-eof t) nil))
                                  (file-position stream pre-read-posn)
                                  (handler-bind
                                      ((cl-reader:symbol-in-missing-package-error
                                        (lambda (cond)
                                          (declare (ignore cond))
                                          (invoke-restart (find-restart 'cl-reader:return-uninterned))))
                                       (cl-reader:symbol-missing-in-package-error
                                        (lambda (cond)
                                          (declare (ignore cond))
                                          (invoke-restart (find-restart 'make-symbol)))))
                                    (return (cl-reader:read stream nil 'das-eof)))))))
                          (cl-reader:read stream nil 'das-eof)
                          ;; (let ((cl-reader:*readtable* (cl-reader:copy-readtable)))
                          ;;         ;; (cl-reader:set-syntax-from-char #\: #\Space)
                          ;;         ;; (cl-reader:set-dispatch-macro-character #\# #\? (lambda (s c i) (declare (ignore s c i)) (cl-reader:read s nil 'das-eof t)))
                          ;;         (cl-reader:set-dispatch-macro-character #\# #\. (lambda (s c i) (declare (ignore c i)) (cl-reader:read s nil 'das-eof t) nil))
                          ;;         (cl-reader:read stream nil 'das-eof))
                          )))
          (until (eq 'das-eof form))
          (when (form-defsystem-p form)
            (collect (funcall fn form))))))

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
