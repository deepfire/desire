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

(defun reinit-definitions ()
  "Empty all global definitions."
  (dolist (var '(*distributors* *remotes* *localities* *modules* *leaves* *nonleaves* *systems* *apps* *master-mirrors*))
    (setf (symbol-value var) (make-hash-table :test #'equal))))

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
  ((remotes :accessor distributor-remotes :initarg :remotes :documentation "Specified."))
  (:default-initargs
   :registrator #'(setf distributor) :remotes nil))

(defmethod print-object ((o distributor) stream)
  (format stream "~@<#D(~;~A ~@<~S ~S~:@>~;)~:@>" (symbol-name (name o)) :remotes (distributor-remotes o)))

(defun distributor-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key remotes &allow-other-keys) (read stream nil nil t)
    `(or (distributor ',name :if-does-not-exist :continue)
         (prog1 (make-instance 'distributor :name ',name ,@(remove-from-plist initargs :remotes))
           ,@remotes))))

(defun distributor-remote (distributor value &key (key #'name))
  (find value (distributor-remotes (coerce-to-distributor distributor)) :key key))

(defun distributor-remote-if (fn distributor)
  (find-if fn (distributor-remotes (coerce-to-distributor distributor))))

(defun compute-distributor-modules (distributor)
  "Compute the set of module names published by DISTRIBUTOR."
  (remove-duplicates (mappend #'location-modules (distributor-remotes (coerce-to-distributor distributor)))))

(defclass rcs-type-mixin () ((rcs-type :reader rcs-type :initarg :rcs-type)))

;;; exhaustive partition of RCS-TYPE-MIXIN
(defclass git (rcs-type-mixin)   () (:default-initargs :rcs-type 'git))
(defclass hg (rcs-type-mixin)    () (:default-initargs :rcs-type 'hg))
(defclass darcs (rcs-type-mixin) () (:default-initargs :rcs-type 'darcs))
(defclass cvs (rcs-type-mixin)   () (:default-initargs :rcs-type 'cvs))
(defclass svn (rcs-type-mixin)   () (:default-initargs :rcs-type 'svn))

(defclass transport-mixin () ((transport :reader transport :initarg :transport)))

;;; non-exhaustive partition of TRANSPORT-MIXIN
(defclass git-native-transport (transport-mixin) () (:default-initargs :transport 'git))
(defclass http (transport-mixin) () (:default-initargs :transport 'http))
(defclass rsync (transport-mixin) () (:default-initargs :transport 'rsync :rcs-type 'rsync))

;;; exhaustive partition of type product of RCS-TYPE and TRANSPORT-MIXIN
(defclass git-native (git git-native-transport) ())
(defclass git-http (git http) ())
(defclass hg-http (hg http) ())
(defclass darcs-http (darcs http) ())
(defclass cvs-rsync (cvs rsync) ())
(defclass svn-rsync (svn rsync) ())

(defclass location (named)
  ((modules :accessor location-modules :initarg :modules :documentation "Specified or maybe cached, for LOCALITYs."))
  (:default-initargs :modules nil))

;;; exhaustive partition of LOCATION
(defclass locality (location)
  ((path :accessor locality-path :initarg :path :documentation "Specified.")
   (scan-p :accessor locality-scan-p :initarg :scan-p :documentation "Specified."))
  (:default-initargs
   :scan-p nil))
(defclass remote (location registered)
  ((distributor :accessor remote-distributor :initarg :distributor :documentation "Specified.")
   (distributor-port :accessor remote-distributor-port :type (or null (integer 0)) :initarg :distributor-port :documentation "Specified, rarely.")
   (path-form :accessor remote-path-form :initarg :path-form :documentation "Specified.")
   (path-fn :accessor remote-path-fn :initarg :path-fn :documentation "Cache."))
  (:default-initargs
   :registrator #'(setf remote)
   :distributor-port nil))

;;; intermediate types
(defclass git-remote (git remote) ())

;;; most specific, exhaustive partition of REMOTE
(defclass git-native-remote (git-native git-remote) ())
(defclass git-http-remote (git-http remote) ())
(defclass hg-http-remote (hg-http remote) ())
(defclass darcs-http-remote (darcs-http remote) ())
(defclass rsync-remote (rsync remote) ())

(defmethod rcs-type ((o transport-mixin))
  "Blanket for undecided ones. Painful ambiguity, also. Must be documented to ward off the hurt."
  (transport o))

(defun url (remote module-spec &aux (module (coerce-to-module module-spec)))
  (declare (type remote remote) (type (or symbol module) module-spec))
  (concatenate 'simple-base-string
               (downstring (transport remote)) "://" (down-case-name (remote-distributor remote))
               (when-let ((port (remote-distributor-port remote)))
                 (format nil ":~D" port))
               (flatten-path-list (funcall (remote-path-fn remote) module) t)))

;;; most specific, exhaustive partition of LOCALITY
(defclass git-locality (git locality) ())
(defclass hg-locality (hg locality) ())
(defclass darcs-locality (darcs locality) ())
(defclass cvs-locality (cvs locality)
  ((lockdir :accessor cvs-locality-lockdir :initarg :lockdir)))
(defclass svn-locality (svn locality) ())

(defun default-remote-name (distributor-name rcs-type)
  "Compute a default name for remote with RCS-TYPE in DISTRIBUTOR-NAME."
  (if (eq rcs-type 'git)
      distributor-name
      (format-symbol (symbol-package distributor-name) "~A~:[-~A~;~]" distributor-name (eq rcs-type 'git) rcs-type)))

(defmethod print-object ((o remote) stream &aux (default-remote-name (with-standard-io-syntax (default-remote-name (name (remote-distributor o)) (rcs-type o)))) )
  (destructuring-bind ((form-binding) . form-body) (remote-path-form o)
    (let ((*print-case* :downcase))
      (format stream "~@<#R(~;~A ~A ~:<(~A)~{ ~S~}~:@>~{ ~<~S ~A~:@>~}~;)~:@>"
              (symbol-name (type-of o)) (symbol-name (name (remote-distributor o))) (list form-binding form-body)
              (append (unless (equal default-remote-name (name o))
                        (list `(:name ,(name o))))
                      (when-let ((port (remote-distributor-port o)))
                        (list `(:distributor-port ,port)))
                      (list `(:modules ,(mapcar #'downstring (location-modules o)))))))))

(defun init-time-collate-remote-name (distributor rcs-type &optional specified-name)
  "Provide a mechanism for init-time name collation for REMOTE with DISTRIBUTOR-NAME,
   and optional SPECIFIED-NAME.

   XXX: not so true anymore, with introduction of RSYNC-REMOTE
   Collation rules are considered in order, as follows:
      - SPECIFIED-NAME wins,
      - if REMOTE is the only GIT remote in DISTRIBUTOR-NAME, provide a default of DISTRIBUTOR-NAME,
      - if REMOTE is the only remote of its RCS type in DISTRIBUTOR-NAME, provide a default of DISTRIBUTOR-NAME-RCS-TYPE."
  (let ((distributor-name (name distributor)))
    (cond (specified-name (if (null (distributor-remote distributor specified-name))
                              specified-name
                              (error "~@<Specified remote name ~A conflicts in distributor ~A.~:@>" specified-name distributor-name)))
          (t (if-let* ((default-name (default-remote-name distributor-name rcs-type))
                       (non-conflicting-p (null (find default-name (distributor-remotes distributor) :key #'name))))
                      default-name
                      (error "~@<Cannot choose an unambiguous name for a ~A remote in distributor ~A, provide one explicitly.~:@>"
                             rcs-type distributor-name))))))

(defmethod initialize-instance :before ((o remote) &key distributor rcs-type name &allow-other-keys)
  (setf (name o) (init-time-collate-remote-name distributor rcs-type name)))

(defmethod initialize-instance :after ((o remote) &key distributor &allow-other-keys)
  (appendf (distributor-remotes distributor) (list o)))

(defun remote-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (type distributor ((repovar) &rest path-form) &rest initargs &key modules name &allow-other-keys) (read stream nil nil)
    `(make-instance ',type :distributor (distributor ',distributor) :modules ',modules
                    :path-form ',(list* (list repovar) path-form) :path-fn (lambda (,repovar) (list ,@path-form))
                    ,@(when name `(:name ',name))
                    ,@(remove-from-plist initargs :name :distributor :type :modules :path-form :path-fn))))

(defmethod initialize-instance :after ((o locality) &key path master-mirror-p &allow-other-keys)
  (unless path
    (error "~@<Useless locality definition: no PATH specified.~:@>"))
  (when master-mirror-p
    (setf (master-mirror (rcs-type o)) o)))

(defun locality-master-mirror-p (o)
  (eq o (master-mirror (rcs-type o) :if-does-not-exist :continue)))

(defmethod print-object ((o locality) stream)
  (format stream "~@<#L(~;~A ~S~{ ~<~S ~S~:@>~}~;)~:@>"
          (symbol-name (type-of o)) (locality-path o)
          (append (when (locality-master-mirror-p o) (list (list :master-mirror-p t)))
                  (when (locality-scan-p o) (list (list :scan-p t))))))

(defun locality-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (type path &rest initargs &key &allow-other-keys) (read stream nil nil)
    `(make-instance ',type :path ,path ,@(remove-from-plist initargs :path :modules))))

(defclass module (registered depobj)
  ((umbrella :accessor module-umbrella :initarg :umbrella :documentation "Transitory?")
   (depends-on :accessor module-depends-on :initarg :depends-on :documentation "Specified.")
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

(defun module-system-implied-p (module &aux (system (first (module-systems module))))
  "Determine if SYSTEM's existence is deducible and omitted from definitions."
  (and system (null (system-relativity system))
       (= 1 (length (module-systems module)))))

(defmethod print-object ((o module) stream)
  (declare (special *force-modules-essential*))
  (format stream "~@<#M(~;~A~{ ~<~S ~S~:@>~}~;)~:@>" (if (eq (name o) (module-umbrella o))
                                                          (symbol-name (name o))
                                                          (list (symbol-name (name o)) (module-umbrella o)))
          (remove nil (list (when-let (deps (module-dependencies o))
                              (list :depends-on deps))
                            (let ((systems (module-systems o)))
                              (unless (module-system-implied-p o)
                               (list :systems (and (module-systems o) (mapcar #'name systems)))))
                            (when (or (module-essential-p o) *force-modules-essential*)
                              (list :essential-p t))))))

(defmethod initialize-instance :around ((o module) &key name &allow-other-keys)
  (when (module name :if-does-not-exist :continue)
    (break))
  (call-next-method))

(defun module-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key depends-on (systems nil systems-specified-p) &allow-other-keys) (read stream nil nil t)
    (declare (ignore systems))
    (destructuring-bind (name umbrella) (if (consp name) name (list name name))
     `(or (module ',name :if-does-not-exist :continue)
          (prog1 (make-instance 'module :name ',name :umbrella ',umbrella ,@(when depends-on `(:depends-on '(,@depends-on)))
                                ,@(remove-from-plist initargs :systems :depends-on))
                 ,@(unless systems-specified-p `((make-instance 'system :name ',name :module (module ',name)))))))))

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
                  (and (system-applications o) (list :applications (system-applications o))))))

(defun system-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key module relativity &allow-other-keys) (read stream nil nil t)
    `(or (system ',name :if-does-not-exist :continue)
         (make-instance 'system :name ',name :module (module ',module) :relativity ',relativity ,@(remove-from-plist initargs :module :applications :relativity)))))

(defmethod initialize-instance :after ((o system) &key module &allow-other-keys)
  (appendf (module-systems module) (list o)))

(defclass application (registered)
  ((system :accessor app-system :initarg :system :documentation "Specified.")
   (package :accessor app-package :initarg :package :documentation "Specified.")
   (function :accessor app-function :initarg :function :documentation "Specified.")
   (default-parameters :accessor app-default-parameters :initarg :default-parameters :documentation "Specified."))
  (:default-initargs
   :registrator #'(setf app) :system nil :package nil :function nil :default-parameters nil))

(defmethod print-object ((o application) stream)
  (format stream "~@<#A(~;~A~{ ~S ~S~}~;)~:@>" (symbol-name (name o))
          (list :system (name (app-system o)) :package (app-package o) :function (app-function o)
                :default-parameters (app-default-parameters o))))

(defun application-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key system package function default-parameters &allow-other-keys) (read stream nil nil t)
    `(or (app ',name :if-does-not-exist :continue)
         (make-instance 'application :name ',name :system (system ',system) :package ',package :function ',function :default-parameters ',default-parameters
                        ,@(remove-from-plist initargs :system :package :function :default-parameters)))))

(defmethod initialize-instance :after ((o application) &key system &allow-other-keys)
  (appendf (system-applications system) (list o)))

;;;
;;; Accessors, mappers and coercers.
;;;

(defun coerce-to-name (name)
  (declare (type (or symbol string) name))
  (typecase name
    (symbol (string-upcase (symbol-name name)))
    (string (string-upcase name))))

(define-container-hash-accessor *distributors*   distributor   :name-transform-fn coerce-to-name :coercer t :mapper map-distributors)
(define-container-hash-accessor *modules*        module        :name-transform-fn coerce-to-name :coercer t :mapper map-modules :when-exists :error)
(define-container-hash-accessor *leaves*         leaf          :name-transform-fn coerce-to-name :type module :mapper map-leaves :when-exists :continue)
(define-container-hash-accessor *nonleaves*      nonleaf       :name-transform-fn coerce-to-name :type module :mapper map-nonleaves :when-exists :continue)
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

(defun module-dependencies (module &aux (module (coerce-to-module module)))
  "Produce a minimal list of MODULE dependency names, which is a subset
   of its direct dependencies."
  (map-dependencies #'name module))

(defun module-full-dependencies (module &optional stack &aux (module (coerce-to-module module)))
  "Compute the complete set of MODULE dependencies."
  (unless (member module stack)
    (cons (name module)
          (iter (for dep in (module-dependencies module))
                (unioning (module-full-dependencies dep (cons module stack)))))))

(defun minimise-dependencies (leaves &aux (loops (make-hash-table :test #'equal)))
  "Minimise the dependency graph specified by potential leaf set LEAVES."
  (labels ((maybe-remove-nonleaf (name leaf)
             (unless (satisfied-p leaf)
               (remhash name leaves)))
           (minimise (current &optional acc-deps)
             (cond ((member current acc-deps) ;; is there a dependency loop?
                    (appendf (gethash current loops) (list (first acc-deps)))
                    (undepend current (first acc-deps)))
                   (t
                    (dolist (overdep (intersection (cdr acc-deps) (mapcar #'cadr (hash-table-alist (depsolver::%depobj-dep# current)))))
                      (undepend current overdep))
                    (mapc (rcurry #'minimise (cons current acc-deps)) (mapcar #'cdr (hash-table-alist (depsolver::%depobj-rdep# current))))))))
    (maphash #'maybe-remove-nonleaf leaves)
    (maphash-values #'minimise leaves)
    (iter (for (dependent deplist) in-hashtable loops)
          (mapc (curry #'depend dependent) deplist))))

(defun define-locality (rcs-type &rest keys &key path &allow-other-keys)
  "Define locality of RCS-TYPE at PATH, if one doesn't exist already, in which case an error is signalled.
   Additionally, register it as a master mirror if MASTER-MIRROR-P is specified."
  (setf (locality path) (apply #'make-instance (format-symbol (symbol-package rcs-type) "~A-LOCALITY" rcs-type) keys)))

(defun read-definitions (stream)
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
    (maphash-values #'reestablish-module-dependencies *modules*)
    (minimise-dependencies *leaves*)))

(defun identify-with-distributor (distributor-name locality)
  "Recognize DISTRIBUTOR-NAME's modules as locally hosted in LOCALITY."
  (iter (for module-name in (compute-distributor-modules (distributor distributor-name)))
        (change-class (module module-name) 'origin-module :master-mirror-locality locality)))

(defun serialize-definitions (&optional stream)
  "Serialise global definitions to STREAM."
  (let ((*print-case* :downcase)
        (*package* #.*package*))
    (flet ((sorted-hash-table-entries (hash-table)
             (sort (hash-table-values hash-table) #'string< :key (compose #'string #'name))))
      (format stream "~&;;; -*- Mode: Lisp -*-~%;;;~%;;; Distributors~%;;;")
      (iter (for d in (sorted-hash-table-entries *distributors*)) (print d stream))
      (format stream "~%~%;;;~%;;; Modules~%;;;")
      (iter (for m in (sorted-hash-table-entries *modules*)) (print m stream))
      (format stream "~%~%;;;~%;;; Systems~%;;;")
      (iter (for s in (sorted-hash-table-entries *systems*)) (unless (module-system-implied-p (system-module s)) (print s stream)))
      (format stream "~%~%;;;~%;;; Applications~%;;;")
      (iter (for a in (sorted-hash-table-entries *apps*)) (print a stream))
      (format stream "~%~%;;;~%;;; Desires~%;;;")
      (print *desires* stream))))

(defun test-core (&optional bail-out-early (pathes-from (list "/mnt/little/git/cling/definitions.lisp"
                                                              "/mnt/little/git/clung/definitions.lisp"))
                  (path-int-0 "/tmp/essential-0") (path-int-1 "/tmp/essential-1") (path-int-2 "/tmp/essential-2") (force-essential nil))
  (let ((*force-modules-essential* force-essential))
    (reinit-definitions)
    (mapcar #'load pathes-from)
    (with-output-to-file (f path-int-0)
      (serialize-definitions f))
    (when bail-out-early
      (return-from test-core))
    (reinit-definitions)
    (read-definitions path-int-0)
    (with-output-to-file (f path-int-1)
      (serialize-definitions f))
    (reinit-definitions)
    (read-definitions path-int-1)
    (with-output-to-file (f path-int-2)
      (serialize-definitions f))))
