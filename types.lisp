;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLING; Base: 10 -*-
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

(in-package :cling)

;;;
;;; Globals
;;;
(defvar *distributors*   (make-hash-table :test #'equal) "Map distributor names to remotes.")
(defvar *remotes*        (make-hash-table :test #'equal) "Map remote names to remotes.")
(defvar *localities*     (make-hash-table :test #'equal) "Map paths to localities.")
(defvar *modules*        (make-hash-table :test #'equal) "Map module names to modules.")
(defvar *leaves*         (make-hash-table :test #'equal) "Map module names to leaf modules.")
(defvar *nonleaves*      (make-hash-table :test #'equal) "Map module names to nonleaf modules.")
(defvar *systems*        (make-hash-table :test #'equal) "Map system names to remotes.")
(defvar *apps*           (make-hash-table :test #'equal) "Map application names to remotes.")
(defvar *master-mirrors* (make-hash-table :test #'equal) "Map RCS type to localities.")

;;;
;;; Knobs
;;;
(defvar *desires*                 nil "List of import descriptions.")
(defvar *default-world-readable*  t   "Whether to publish GIT repositories by default.")
(defvar *force-modules-essential* nil "Force all modules to be essential.")

(defclass named ()
  ((name :accessor name :initarg :name)))

(defclass registered (named)
  ((registrator :accessor registered-registrator :type function :initarg :registrator)))

(defgeneric fully-qualified-name (o)
  (:documentation "A name which is supposed to be unique within the use domain.")
  (:method ((o registered)) (name o)))

(defmethod initialize-instance :after ((o registered) &key registrator &allow-other-keys)
  "Designed for SETF."
  (funcall registrator o (fully-qualified-name o)))

(defun downstring (x)
  (string-downcase (string x)))

(defun down-case-name (x)
  (string-downcase (string (name x))))

;;;
;;; Distributor name is its hostname.
;;;
(defclass distributor (registered)
  ((remotes :accessor distributor-remotes :initarg :remotes :documentation "Specified.")
   (url-forms :accessor distributor-url-forms :initarg :url-forms :documentation "Transitory.")
   (url-fns :accessor distributor-url-fns :initarg :url-fns :documentation "Transitory."))
  (:default-initargs
   :registrator #'(setf distributor) :url-forms nil :url-fns nil
   :remotes nil))

(defun distributor-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key remotes &allow-other-keys) (read stream nil nil t)
    `(or (distributor ',name :if-does-not-exist :continue)
         (make-instance 'distributor :name ',name :remotes (list ,@remotes) ,@(remove-from-plist initargs :remotes)))))

(defun distributor-remote (distributor value &key (key #'name))
  (find value (distributor-remotes distributor) :key key))

(defun distributor-remote-if (distributor fn)
  (find-if fn (distributor-remotes distributor)))

(defun compute-distributor-modules (distributor)
  "Compute the set of module names published by DISTRIBUTOR."
  (remove-duplicates (mappend #'location-modules (distributor-remotes distributor))))

(defclass rcs-type-mixin () ((type :reader rcs-type :initarg :type)))

;;; exhaustive partition of RCS-TYPE-MIXIN
(defclass git (rcs-type-mixin)   () (:default-initargs :type 'git))
(defclass darcs (rcs-type-mixin) () (:default-initargs :type 'darcs))
(defclass cvs (rcs-type-mixin)   () (:default-initargs :type 'cvs))
(defclass svn (rcs-type-mixin)   () (:default-initargs :type 'svn))

(defclass location (named)
  ((modules :accessor location-modules :initarg :modules :documentation "Specified or maybe cached, for LOCALITYs."))
  (:default-initargs :modules nil))

;;; exhaustive partition of LOCATION
(defclass locality (location)
  ((path :accessor locality-path :initarg :path :documentation "Specified.")
   (scan-p :accessor locality-scan-p :initarg :scan-p :documentation "Specified.")))
(defclass remote (location registered)
  ((distributor :accessor remote-distributor :initarg :distributor :documentation "Specified.")
   (path-form :accessor remote-path-form :initarg :path-form :documentation "Specified.")
   (path-fn :accessor remote-path-fn :initarg :path-fn :documentation "Cache."))
  (:default-initargs :registrator #'(setf remote)))

;;; exhaustive partition of REMOTE
(defclass native-remote (remote) ())
(defclass http-remote (remote) ())
(defclass rsync-remote (remote) ())

;;; most specific, exhaustive partition of REMOTE
(defclass git-native-remote (git native-remote) ())
(defclass git-http-remote (git http-remote) ())
(defclass darcs-http-remote (darcs http-remote) ())
(defclass svn-rsync-remote (svn rsync-remote) ())
(defclass cvs-rsync-remote (cvs rsync-remote) ())

(defgeneric schema (location)
  (:method ((o git-native-remote)) "git")
  (:method ((o http-remote)) "http")
  (:method ((o rsync-remote)) "rsync"))

(defun url (remote name)
  (declare (type remote remote) (type symbol name))
  (concatenate 'simple-base-string
               (schema remote) "://" (down-case-name (remote-distributor remote))
               (funcall (remote-path-fn remote) name)))

;;; most specific, exhaustive partition of LOCALITY
(defclass git-locality (git locality) ())
(defclass darcs-locality (darcs locality) ())
(defclass cvs-locality (cvs locality)
  ((lockdir :accessor cvs-locality-lockdir :initarg :lockdir)))
(defclass svn-locality (svn locality) ())

(defmethod print-object ((o distributor) stream)
  (format stream "~@<#D(~;~A~{ ~<~S ~S~:@>~}~;)~:@>" (symbol-name (name o)) (list :remotes (distributor-remotes o))))

(defmethod initialize-instance :after ((o distributor) &rest rest)
  (declare (ignore rest))
  (dolist (loc (distributor-remotes o))
    (setf (remote-distributor loc) o)))

(defun init-time-collate-remote-name (remote distributor-name &optional specified-name &aux (rcs-type (rcs-type remote)))
  "Provide a mechanism for init-time name collation for REMOTE with DISTRIBUTOR-NAME,
   and optional SPECIFIED-NAME.

   Collation rules are considered in order, as follows:
      - SPECIFIED-NAME wins,
      - if REMOTE is the only GIT remote in DISTRIBUTOR-NAME, provide a default of DISTRIBUTOR-NAME,
      - if REMOTE is the only remote of its RCS type in DISTRIBUTOR-NAME, provide a default of DISTRIBUTOR-NAME-RCS-TYPE."
  (let ((distributor (distributor distributor-name)))
    (cond (specified-name (if (null (distributor-remote distributor specified-name))
                              specified-name
                              (error "~@<Specified remote name ~A conflicts in distributor ~A.~:@>" specified-name distributor-name)))
          ((and (typep remote 'git) (null (distributor-remote-if (rcurry #'typep 'git) distributor))) distributor-name)
          (t (if-let* ((default-name (format-symbol (symbol-package distributor-name) "~A-~A" distributor-name rcs-type))
                       (non-conflicting-p (null (find default-name (distributor-remotes distributor) :key #'name))))
                      default-name
                      (error "~@<Cannot choose an unambiguous name for a ~A remote in distributor ~A, provide one explicitly.~:@>"
                             rcs-type distributor-name))))))

(defmethod initialize-instance :before ((o remote) &key distributor name &allow-other-keys)
  (setf (name o) (init-time-collate-remote-name o distributor name)))

(defmethod print-object ((o remote) stream)
  (destructuring-bind ((form-binding) . form-body) (remote-path-form o)
   (format stream "~@<#R(~;~A ~A ~@<(~;(~A)~{ ~S~}~;)~:@>~{ ~<~S ~S~:@>~}~;)~:@>"
           (symbol-name (type-of o)) (symbol-name (xform-if-not #'symbolp #'name (remote-distributor o))) form-binding form-body
           (list :modules (mapcar #'down-case-name (location-modules o))))))

(defun remote-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (type distributor ((repovar) &rest path-form) &rest initargs &key modules &allow-other-keys) (read stream nil nil)
    `(make-instance ',type :distributor ',distributor :type ',type :path-form ',(list* 'url (list repovar) path-form) :modules ',modules
                    :path-fn (lambda (,repovar) (list ,@path-form))
                    ,@(remove-from-plist initargs :distributor :type :modules :path-form :path-fn))))

(defmethod initialize-instance :after ((o locality) &key path master-mirror-p &allow-other-keys)
  (unless path
    (error "~@<Useless locality definition: no PATH specified.~:@>"))
  (when master-mirror-p
    (setf (master-mirror (rcs-type o)) o)))

(defun define-locality (rcs-type &rest keys &key path &allow-other-keys)
  "Define locality of RCS-TYPE at PATH, if one doesn't exist already, in which case an error is signalled.
   Additionally, register it as a master mirror if MASTER-MIRROR-P is specified."
  (setf (locality path) (apply #'make-instance (format-symbol (symbol-package rcs-type) "~A-LOCALITY" rcs-type) keys)))

(defun locality-master-mirror-p (o)
  (eq o (master-mirror (rcs-type o))))

(defmethod print-object ((o locality) stream)
  (format stream "~@<#L(~;~A ~A~<~{ ~S ~S~}~:@>~;)~:@>"
          (symbol-name (type-of o)) (locality-path o)
          (append (when (locality-master-mirror-p o) (list :master-mirror-p t))
                  (when (locality-scan-p o) (list :scan-p t)))))

(defun locality-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (type path &rest initargs &key &allow-other-keys) (read stream nil nil)
    `(make-instance ',type :path ,path ,@(remove-from-plist initargs :path :modules))))

(defclass module (registered depobj)
  ((depends-on :accessor module-depends-on :initarg :depends-on :documentation "Specified.")
   (essential-p :accessor module-essential-p :initarg :essential-p :documentation "Specified.")
   (master-mirror-locality :accessor module-master-mirror-locality :initarg :master-mirror-locality :documentation "Policy-decided.")
   (remotes :accessor module-remotes :initarg :remotes :documentation "Cache.")
   (localities :accessor module-localities :initarg :localities :documentation "Cache.")
   (systems :accessor module-systems :initarg :systems :documentation "Cache."))
  (:default-initargs
   :registrator #'(setf module)    
   :remotes nil :master-mirror-locality nil :localities nil
   :depends-on nil :systems nil :essential-p nil))

;;; most specific, exhaustive partition of MODULE
(defclass origin-module (module) ())
(defclass imported-module (module)
  ((preferred-remote :accessor module-preferred-remote :initarg :preferred-remote :documentation "Policy-decided."))
  (:default-initargs :preferred-remote nil))

(defmethod print-object ((o module) stream)
  (declare (special *force-modules-essential*))
  (format stream "~@<#M(~;~A~{ ~<~S ~S~:@>~}~;)~:@>" (symbol-name (name o))
          (remove nil (list (when-let (deps (remove-duplicates (append (module-depends-on o) (map-dependencies #'name o))))
                              (list :depends-on deps))
                            (when-let (systems (module-systems o))
                              (list :systems (and (module-systems o) (mapcar #'name systems))))
                            (when (or (module-essential-p o) *force-modules-essential*)
                              (list :essential-p t))))))

(defun module-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key distributor depends-on &allow-other-keys) (read stream nil nil t)
    `(or (module ',name :if-does-not-exist :continue)
         (make-instance 'module :name ',name :distributor (distributor ',distributor)
                        ,@(when depends-on `(:depends-on '(,@depends-on)))
                        ,@(remove-from-plist initargs :distributor :systems :depends-on)))))

(defclass system (registered)
  ((module :accessor system-module :initarg :module :documentation "Specified.")
   (relativity :accessor system-relativity :initarg :relativity :documentation "Specified.")
   (applications :accessor system-applications :initarg :applications :documentation "Cache."))
  (:default-initargs
   :registrator #'(setf system)
   :module nil :applications nil :relativity nil))

(defmethod print-object ((o system) stream)
  (format stream "~@<#S(~;~A~{ ~S~}~;)~:@>" (symbol-name (name o))
          (append (list :module (name (system-module o)))
                  (and (system-relativity o) (list :relativity (system-relativity o)))
                  (and (system-applications o) (list :applications (mapcar #'name (system-applications o)))))))

(defun system-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key module &allow-other-keys) (read stream nil nil t)
    `(or (system ',name :if-does-not-exist :continue)
         (make-instance 'system :name ',name :module (module ',module) ,@(remove-from-plist initargs :module :applications)))))

(defmethod initialize-instance :after ((o system) &key module &allow-other-keys)
  (push o (module-systems module)))

(defclass application (registered)
  ((system :accessor app-system :initarg :system :documentation "Specified.")
   (package :accessor app-package :initarg :package :documentation "Specified.")
   (function :accessor app-function :initarg :function :documentation "Specified.")
   (default-parameters :accessor app-default-parameters :initarg :default-parameters :documentation "Specified."))
  (:default-initargs
   :registrator #'(setf app)))

(defmethod print-object ((o application) stream)
  (format stream "~@<#A(~;~A~{ ~A~}~;)~:@>" (symbol-name (name o))
          (list :system (name (app-system o)) :package (string (app-package o)) :function (string (app-function o))
                :default-parameters (app-default-parameters o))))

(defun application-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key system &allow-other-keys) (read stream nil nil t)
    `(or (application ',name :if-does-not-exist :continue)
         (make-instance 'application :name ',name :system (system ',system) ,@(remove-from-plist initargs :system)))))

(defmethod initialize-instance :after ((o application) &key system &allow-other-keys)
  (push o (system-applications system)))

;;;
;;; Accessors, mappers and coercers.
;;;

(defun coerce-to-name (name)
  (declare (type (or symbol string) name))
  (typecase name
    (symbol (string-upcase (symbol-name name)))
    (string (string-upcase name))))

(define-container-hash-accessor *distributors*   distributor   :name-transform-fn coerce-to-name :coercer t :mapper map-distributors)
(define-container-hash-accessor *modules*        module        :name-transform-fn coerce-to-name :coercer t :mapper map-modules)
(define-container-hash-accessor *leaves*         leaf          :name-transform-fn coerce-to-name :type module :mapper map-leaves)
(define-container-hash-accessor *nonleaves*      nonleaf       :name-transform-fn coerce-to-name :type module :mapper map-nonleaves)
(define-container-hash-accessor *systems*        system        :name-transform-fn coerce-to-name :coercer t :mapper map-systems)
(define-container-hash-accessor *apps*           app           :name-transform-fn coerce-to-name :coercer t :mapper map-app :type application)
(define-container-hash-accessor *remotes*        remote        :name-transform-fn coerce-to-name :coercer t :mapper map-remotes :type remote :when-exists :error)
(define-container-hash-accessor *localities*     locality      :type locality :mapper map-localities :when-exists :error)
(define-container-hash-accessor *master-mirrors* master-mirror :type locality)

(defun reestablish-module-dependencies (module)
  "Rebuild MODULE's part of the dependency graph, and clear
   the transitory DEPENDS-ON slot representation."
  (iter (for depname = (pop (module-depends-on module))) (while depname)
        (for dep = (module depname))
        (setf (nonleaf depname) dep
              (leaf (name module)) module) ;; might be reversed later
        (depend module dep)))

(defun module-dependencies (module)
  "Produce a minimal list of MODULE dependency names."
  (map-dependencies #'name module))

(defun minimise-dependencies (leaves &aux (loops (make-hash-table :test #'equal)))
  "Minimise the dependency graph specified by potential leaf set LEAVES."
  (labels ((maybe-remove-nonleaf (name leaf)
             (unless (satisfied-p leaf)
               (remhash name leaves)))
           (minimise (current &optional acc-deps)
             (cond ((member current acc-deps) ;; is there a dependency loop?
                    (push (first acc-deps) (gethash current loops))
                    (undepend current (first acc-deps)))
                   (t
                    (dolist (overdep (intersection (cdr acc-deps) (mapcar #'cadr (hash-table-alist (depsolver::%depobj-dep# current)))))
                      (undepend current overdep))
                    (mapc (rcurry #'minimise (cons current acc-deps)) (mapcar #'cdr (hash-table-alist (depsolver::%depobj-rdep# current))))))))
    (maphash #'maybe-remove-nonleaf leaves)
    (maphash-values #'minimise leaves)
    (iter (for (dependent deplist) in-hashtable loops)
          (mapc (curry #'depend dependent) deplist))))

(defun load-definitions (stream)
  "Unserialise global definitions from STREAM."
  (let ((*readtable* (copy-readtable))
        (*read-eval* nil)
        (*package* #.*package*))
    (set-dispatch-macro-character #\# #\D 'distributor-reader *readtable*)
    (set-dispatch-macro-character #\# #\M 'module-reader *readtable*)
    (set-dispatch-macro-character #\# #\S 'system-reader *readtable*)
    (set-dispatch-macro-character #\# #\A 'application-reader *readtable*)
    (set-dispatch-macro-character #\# #\R 'remote-reader *readtable*)
    (set-dispatch-macro-character #\# #\L 'locality-reader *readtable*)
    (load stream)
    (mapc #'reestablish-module-dependencies *modules*)
    (minimise-dependencies *leaves*)))

(defun identify-with-distributor (distributor-name locality)
  "Recognize DISTRIBUTOR-NAME's modules as locally hosted in LOCALITY."
  (iter (for module-name in (compute-distributor-modules (distributor distributor-name)))
        (change-class (module module-name) 'origin-module :master-mirror-locality locality)))

(defun serialize-definitions (&optional stream)
  "Serialise global definitions to STREAM."
  (let ((*print-case* :downcase)
        (*package* #.*package*))
    (format stream "~&;;; -*- Mode: Lisp -*-~%;;;~%;;; Distributors~%;;;")
    (iter (for (nil d) in-hashtable *distributors*) (print d stream))
    (format stream "~%~%;;;~%;;; Modules~%;;;")
    (iter (for (nil m) in-hashtable *modules*) (print m stream))
    (format stream "~%~%;;;~%;;; Systems~%;;;")
    (iter (for (nil s) in-hashtable *systems*) (print s stream))
    (format stream "~%~%;;;~%;;; Applications~%;;;")
    (iter (for (nil a) in-hashtable *apps*) (print a stream))
    (format stream "~%~%;;;~%;;; Desires~%;;;")
    (print *desires* stream)))

(defun test-core-1 (&optional (path-from "/mnt/little/git/cling/definitions.lisp") (path-to "/tmp/essential") (force-essential nil))
  (load path-from)
  (let ((*force-modules-essential* force-essential))
    (with-output-to-file (f path-to)
      (serialize-definitions f))))

(defun test-core-2 (&optional (path-from "/tmp/essential") (path-to "/tmp/essential2") (force-essential nil))
  (load-definitions path-from)
  (let ((*force-modules-essential* force-essential))
    (with-output-to-file (f path-to)
      (serialize-definitions f))))