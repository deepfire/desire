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

;;;
;;; Knobs
;;;
(defvar *default-wishmaster*      'git.feelingofgreen.ru)
(defvar *desires*                 nil "List of import descriptions.")
(defvar *default-world-readable*  t   "Whether to publish GIT repositories by default.")
(defvar *self*                    nil "The well-known self, if any.")

;;;
;;; Globals
;;;
(defvar *root-of-all-desires* nil
  "The storage location for all source code, binaries and configuration files.")

(defparameter *distributors*       (make-hash-table :test #'equal) "Map distributor names to remotes.")
(defparameter *remotes*            (make-hash-table :test #'equal) "Map remote names to remotes.")
(defparameter *localities*         (make-hash-table :test #'equal) "Map names to localities.")
(defparameter *localities-by-path* (make-hash-table :test #'equal) "Map paths to localities.")
(defparameter *modules*            (make-hash-table :test #'equal) "Map module names to modules.")
(defparameter *leaves*             (make-hash-table :test #'equal) "Map module names to leaf modules.")
(defparameter *nonleaves*          (make-hash-table :test #'equal) "Map module names to nonleaf modules.")
(defparameter *systems*            (make-hash-table :test #'equal) "Map system names to remotes.")
(defparameter *apps*               (make-hash-table :test #'equal) "Map application names to remotes.")
(defparameter *masters*            (make-hash-table :test #'equal) "Map RCS type to master localities.")

(defvar *printing-wishmaster* nil)

(defun clear-definitions ()
  "Empty all global definitions."
  (dolist (var '(*distributors* *remotes* *localities* *localities-by-path* *modules* *leaves* *nonleaves* *systems* *apps* *masters*))
    (setf (symbol-value var) (make-hash-table :test #'equal))))

(defclass named ()
  ((name :accessor name :initarg :name)))

(defun coerce-to-name (o)
  (declare (type (or symbol named) o))
  (if (symbolp o)
      o
      (name o)))

(defun sort-by-name (named-objects)
  "Sort object of class NAMED, lexicographically."
  (sort named-objects #'string< :key (compose #'symbol-name #'name)))

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

(defclass wishmaster (distributor) ())

(defmethod print-object ((o distributor) stream)
  (format stream "~@<#~A(~;~A ~@<~S ~S~:@>~;)~:@>"
          (ecase (type-of o) (distributor #\D) (wishmaster #\W)) (symbol-name (name o))
          :remotes (xform *printing-wishmaster* (curry #'remove-if-not (of-type 'git)) (distributor-remotes o))))

(defun distributor-reader (stream &optional sharp char)
  (declare (ignore sharp))
  (destructuring-bind (name &rest initargs &key remotes &allow-other-keys) (read stream nil nil t)
    `(or (distributor ',name :if-does-not-exist :continue)
         (prog1 (make-instance ,(ecase char (#\D 'distributor) (#\W 'wishmaster)) :name ',name ,@(remove-from-plist initargs :remotes))
           ,@remotes))))

(defmacro do-distributor-remotes ((var distributor) &body body)
  `(iter (for ,var in (distributor-remotes (coerce-to-distributor ,distributor)))
         ,@body))

(defun distributor-remote (distributor value &key (key #'name))
  (find value (distributor-remotes (coerce-to-distributor distributor)) :key key))

(defun distributor-remote-if (fn distributor)
  (find-if fn (distributor-remotes (coerce-to-distributor distributor))))

(defun compute-distributor-modules (distributor)
  "Compute the set of module names published by DISTRIBUTOR."
  (remove-duplicates (mapcan #'location-modules (distributor-remotes (coerce-to-distributor distributor)))))

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
(defclass rsync (transport-mixin) () (:default-initargs :transport 'rsync))

;;; exhaustive partition of type product of RCS-TYPE and TRANSPORT-MIXIN
(defclass git-native (git git-native-transport) ())
(defclass git-http (git http) ())
(defclass hg-http (hg http) ())
(defclass darcs-http (darcs http) ())
(defclass cvs-rsync (cvs rsync) ())
(defclass svn-rsync (svn rsync) ())

(defclass location (registered)
  ((modules :accessor location-modules :initarg :modules :documentation "Specified or maybe cached, for LOCALITYs."))
  (:default-initargs
   :registrator #'(setf locality)
   :modules nil))

;;; exhaustive partition of LOCATION
(defclass locality (location)
  ((path :accessor locality-path :initarg :path :documentation "Specified.")
   (scan-p :accessor locality-scan-p :initarg :scan-p :documentation "Specified."))
  (:default-initargs
   :scan-p nil))
(defclass remote (location registered)
  ((distributor :accessor remote-distributor :initarg :distributor :documentation "Specified.")
   (distributor-port :accessor remote-distributor-port :type (or null (integer 0 65536)) :initarg :distributor-port :documentation "Specified, rarely.")
   (path-form :accessor remote-path-form :initarg :path-form :documentation "Specified.")
   (disabled-p :accessor remote-disabled-p :type boolean :initarg :disabled-p :documentation "Specified.")
   (path-fn :accessor remote-path-fn :initarg :path-fn :documentation "Cache."))
  (:default-initargs
   :registrator #'(setf remote)
   :disabled-p nil
   :distributor-port nil))

;;; intermediate types
(defclass git-remote (git remote)
  ((converted-modules :accessor git-remote-converted-modules :initarg :converted-modules :documentation "Cache."))
  (:default-initargs
   :converted-modules nil))

;;; most specific, exhaustive partition of REMOTE
(defclass git-native-remote (git-native git-remote) ())
(defclass git-http-remote (git-http git-remote) ())
(defclass hg-http-remote (hg-http remote) ())
(defclass darcs-http-remote (darcs-http remote) ())
(defclass cvs-rsync-remote (cvs-rsync remote) ())
(defclass svn-rsync-remote (svn-rsync remote) ())

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
(defclass cvs-locality (cvs locality) ())
(defclass svn-locality (svn locality) ())

(defun default-remote-name (distributor-name rcs-type)
  "Compute a default name for remote with RCS-TYPE in DISTRIBUTOR-NAME."
  (if (eq rcs-type 'git)
      distributor-name
      (format-symbol (symbol-package distributor-name) "~A~:[-~A~;~]" distributor-name (eq rcs-type 'git) rcs-type)))

(defun system-simple-p (system)
  "Determine whether SYSTEM meets the requirements for a simple system."
  (null (system-relativity system)))

(defun module-simple-p (module)
  "Determine whether MODULE meets the requirements for a simple module."
  (and (eq (name module) (module-umbrella module))
       (not (module-essential-p module))
       (or (null (module-systems module))
           (and (endp (rest (module-systems module)))
                (system-simple-p (first (module-systems module)))))))

(defmethod print-object ((o remote) stream &aux (default-remote-name (with-standard-io-syntax (default-remote-name (name (remote-distributor o)) (rcs-type o)))) )
  (destructuring-bind ((form-binding) . form-body) (remote-path-form o)
    (let ((*print-case* :downcase))
      (format stream "~@<#R(~;~A ~A ~:<(~A)~{ ~S~}~:@>~{ ~<~S ~A~:@>~}~;)~:@>"
              (symbol-name (type-of o)) (symbol-name (name (remote-distributor o))) (list form-binding form-body)
              (if *printing-wishmaster*
                  (list :converted-modules (mapcar #'downstring (git-remote-converted-modules o)))
                  (multiple-value-bind (simple complex) (unzip #'module-simple-p (location-modules o) :key #'module)
                    (multiple-value-bind (systemful systemless) (unzip #'module-systems simple :key #'module)
                      (append (unless (equal default-remote-name (name o))
                                (list `(:name ,(name o))))
                              (when-let ((port (remote-distributor-port o)))
                                (list `(:distributor-port ,port)))
                              (when-let ((port (remote-disabled-p o)))
                                (list `(:disabled-p t)))
                              (when complex
                                (list `(:modules ,(mapcar #'downstring complex))))
                              (when systemful
                                (list `(:simple-modules ,(mapcar #'downstring systemful))))
                              (when systemless
                                (list `(:simple-systemless-modules ,(mapcar #'downstring systemless))))))))))))

(defun init-time-collate-remote-name (distributor rcs-type &optional specified-name)
  "Provide a mechanism for init-time name collation for REMOTE with 
   DISTRIBUTOR-NAME, and optionally SPECIFIED-NAME.

   Collation rules are considered in order, as follows:
      - SPECIFIED-NAME wins,
      - if REMOTE is the only GIT remote in DISTRIBUTOR-NAME, provide a default
        of DISTRIBUTOR-NAME,
      - if REMOTE is the only remote of its RCS type in DISTRIBUTOR-NAME, 
        provide a default of DISTRIBUTOR-NAME-RCS-TYPE."
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

(defun system-implied-p (system)
  "See it the definition of SYSTEM is implied, and is therefore subject to omission. "
  (system-simple-p system))

(defun emit-make-simple-module-form (name)
  `(or (module ',name :if-does-not-exist :continue)
       (make-instance 'module :name ',name :umbrella ',name)))

(defun emit-make-simple-system-form (type module-name name)
  `(or (system ',name :if-does-not-exist :continue)
       (make-instance ',type :name ',name :module (module ',module-name))))

(defun path-form-to-path-fn-form (repo-var path-form)
  "Upgrade the abbreviated PATH-FORM into a proper lambda form, with the repository
   variable called REPO-VAR."
  `(lambda (,repo-var) (list ,@path-form)))

(defun remote-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (type distributor ((repovar) &rest path-form) &rest initargs &key modules simple-modules simple-systemless-modules name &allow-other-keys) (read stream nil nil)
    `(prog1
         (make-instance ',type :distributor (distributor ',distributor) :modules ',(append modules simple-modules simple-systemless-modules)
                        :path-form ',(list* (list repovar) path-form) :path-fn ,(path-form-to-path-fn-form repo-var path-form)
                        ,@(when name `(:name ',name))
                        ,@(remove-from-plist initargs :name :distributor :type :modules :simple-modules :simple-systemless-modules :path-form :path-fn))
       ,@(mapcar #'emit-make-simple-module-form (append simple-modules simple-systemless-modules))
       ,@(mapcar (lambda (name) (emit-make-simple-system-form 'asdf-system name name)) simple-modules))))

(defun locality-master-p (o)
  (eq o (master (rcs-type o) :if-does-not-exist :continue)))

(defmethod print-object ((o locality) stream)
  (format stream "~@<#L(~;~A ~A ~S~{ ~<~S ~S~:@>~}~;)~:@>"
          (symbol-name (type-of o)) (name o) (locality-path o)
          (append (when (locality-master-p o) (list (list :master-p t)))
                  (when (locality-scan-p o) (list (list :scan-p t))))))

(defun locality-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (type name path &rest initargs &key &allow-other-keys) (read stream nil nil)
    `(make-instance ',type :name ',name :path ,path ,@(remove-from-plist initargs :path :modules))))

(defmethod initialize-instance :after ((o locality) &key path &allow-other-keys)
  (unless path
    (error "~@<A location without path specified is useless. ~S is one of many.~:@>" o))
  (setf (locality-by-path path) o))

(defun parse-git-remote-namestring (namestring)
  "Given a git remote NAMESTRING, deduce the remote's type, host, port and path,
   and return them as multiple values."
  (let* ((colon-pos (or (position #\: namestring) (error "No colon in git remote namestring ~S." namestring)))
         (typestr (subseq namestring 0 colon-pos))
         (type (switch (typestr :test #'string=) ("http" 'git-http-remote) ("git" 'git-native-remote)
                       (t (error "Bad URI type ~S in git remote namestring ~S." typestr namestring)))))
    (unless (> (length namestring) (+ colon-pos 3))
      (error "Git remote namestring ~S is too short." namestring))
    (unless (and (char= (aref namestring (+ 1 colon-pos)) #\/)
                 (char= (aref namestring (+ 2 colon-pos)) #\/))
      (error "Git remote namestring ~S is malformed." namestring))
    (let* ((maybe-slash3-pos (position #\/ namestring :start (+ 3 colon-pos)))
           (maybe-colon2-pos (position #\: namestring :start (+ 4 colon-pos) :end maybe-slash3-pos))
           (host (intern (string-upcase (subseq namestring (+ 3 colon-pos) (or maybe-colon2-pos maybe-slash3-pos)))))
           (port (when maybe-colon2-pos (parse-integer namestring :start (1+ maybe-colon2-pos) :end maybe-slash3-pos)))
           (path (when-let ((rest (and maybe-slash3-pos (subseq namestring (1+ maybe-slash3-pos)))))
                   (split-sequence #\/ rest :remove-empty-subseqs t))))
      (values type host port path))))

(defun git-remote-namestring (remote)
  "Produce a namestring for a git REMOTE."
  (declare (type git-remote remote))
  (format nil "~A://~A/~{~A/~}"
          (case (type-of remote) (git-native-remote "git") (git-http-remote "http"))
          (down-case-name remote)
          (butlast (rest (remote-path-form remote)))))

(defclass module (registered depobj)
  ((umbrella :accessor module-umbrella :initarg :umbrella :documentation "Transitory?")
   (essential-p :accessor module-essential-p :initarg :essential-p :documentation "Specified.")
   (scan-positive-localities :accessor module-scan-positive-localities :initarg :remotes :documentation "Cache. Locality scans fill this one.")
   (remotes :accessor module-remotes :initarg :remotes :documentation "Cache. COMPUTE-MODULE-CACHES")
   (localities :accessor module-localities :initarg :localities :documentation "Cache. COMPUTE-MODULE-CACHES")
   (systems :accessor module-systems :initarg :systems :documentation "Cache. COMPUTE-MODULE-CACHES"))
  (:default-initargs
   :registrator #'(setf module)    
   :remotes nil :localities nil
   :systems nil :essential-p nil))

;;; most specific, exhaustive partition of MODULE
(defclass origin-module (module) ())

(defmethod print-object ((o module) stream)
  (format stream "~@<#M(~;~A~{ ~<~S ~S~:@>~}~;)~:@>" (if (eq (name o) (module-umbrella o))
                                                          (symbol-name (name o))
                                                          (list (symbol-name (name o)) (module-umbrella o)))
          (remove nil (multiple-value-call #'list
                        (destructuring-bind (first . other-systems) (module-systems o)
                          (unless (and first (null other-systems) (system-simple-p first))
                            (multiple-value-bind (simple complex) (unzip #'system-simple-p (module-systems o))
                              (values (when simple
                                        (list :systems (mapcar #'name simple)))
                                      (when complex
                                        (list :complex-systems (mapcar #'name complex)))))))
                        (when (module-essential-p o)
                          (list :essential-p t))))))

(defmethod initialize-instance :around ((o module) &key name &allow-other-keys)
  (when (module name :if-does-not-exist :continue)
    (break))
  (call-next-method))

(defun module-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key (systems nil systems-specified-p) complex-systems &allow-other-keys) (read stream nil nil t)
    (destructuring-bind (name umbrella) (if (consp name) name (list name name))
      `(or (module ',name :if-does-not-exist :continue)
           (prog1 (make-instance 'module :name ',name :umbrella ',umbrella
                                 ,@(remove-from-plist initargs :systems :complex-systems))
             ;; The simple system -- this case is used only when the module is not simple itself, i.e. when it's got a non-default umbrella name.
             ,@(if systems-specified-p
                   (mapcar (curry #'emit-make-simple-system-form 'asdf-system name) systems)
                   ;; Existence of complex systems implies absence of simple systems, by default.
                   (unless complex-systems
                     `(,(emit-make-simple-system-form 'asdf-system name name)))))))))

(defclass asdf () ())
(defclass mudballs () ())

(defclass system (registered)
  ((module :accessor system-module :initarg :module :documentation "Specified.")
   (relativity :accessor system-relativity :initarg :relativity :documentation "Specified.")
   (applications :accessor system-applications :initarg :applications :documentation "Cache."))
  (:default-initargs
   :registrator #'(setf system)
   :module nil :applications nil :relativity nil))

(defclass asdf-system (asdf system) ())
(defclass mudballs-system (mudballs system) ())

(defmethod print-object ((o system) stream)
  (format stream "~@<#S(~;~A~{ ~S~}~;)~:@>" (symbol-name (name o))
          (append (list :module (name (system-module o)))
                  (and (system-relativity o) (list :relativity (system-relativity o)))
                  (and (system-applications o) (list :applications (system-applications o))))))

(defun system-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key module relativity &allow-other-keys) (read stream nil nil t)
    `(or (system ',name :if-does-not-exist :continue)
         (make-instance 'asdf-system :name ',name :module (module ',module) :relativity ',relativity ,@(remove-from-plist initargs :module :applications :relativity)))))

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

(defun coerce-to-namestring (namespec)
  (declare (type (or symbol string named) namespec))
  (typecase namespec
    (named (string (name namespec)))
    (symbol (string-upcase (symbol-name namespec)))
    (string (string-upcase namespec))))

(define-container-hash-accessor *distributors*   distributor   :name-transform-fn coerce-to-namestring :coercer t :mapper map-distributors :iterator do-distributors)
(define-container-hash-accessor *modules*        module        :name-transform-fn coerce-to-namestring :coercer t :mapper map-modules :if-exists :error :iterator do-modules)
(define-container-hash-accessor *leaves*         leaf          :name-transform-fn coerce-to-namestring :type module :mapper map-leaves :if-exists :continue)
(define-container-hash-accessor *nonleaves*      nonleaf       :name-transform-fn coerce-to-namestring :type module :mapper map-nonleaves :if-exists :continue)
(define-container-hash-accessor *systems*        system        :name-transform-fn coerce-to-namestring :coercer t :mapper map-systems)
(define-container-hash-accessor *apps*           app           :name-transform-fn coerce-to-namestring :coercer t :mapper map-apps :type application)
(define-container-hash-accessor *remotes*        remote        :name-transform-fn coerce-to-namestring :coercer t :mapper map-remotes :type remote :if-exists :error :iterator do-remotes)
(define-container-hash-accessor *localities*     locality      :type locality :mapper map-localities :if-exists :error)
(define-container-hash-accessor *localities-by-path* locality-by-path :type locality :if-exists :error)
(define-container-hash-accessor *masters*        master        :type locality)

(defun cvs-locality-lock-path (cvs-locality)
  "Provide the fixed definition of lock directory for CVS repositories,
   within CVS-LOCALITY."
  (subdirectory* (locality-path cvs-locality) ".cvs-locks"))

(defun git-locality-asdf-registry-path (git-locality)
  "Provide the fixed definition of ASDF registry directory,
   within GIT-LOCALITY."
  (subdirectory* (locality-path git-locality) ".asdf-registry"))

(defun module-path (module &optional (locality (master 'git)))
  "Return MODULE's path in LOCALITY, which defaults to master Git locality."
  (subdirectory* (locality-path locality) (downstring (coerce-to-name module))))

(defun read-definitions (stream)
  "Unserialise global definitions from STREAM."
  (let ((*readtable* (copy-readtable))
        (*read-eval* nil)
        (*package* #.*package*))
    (set-dispatch-macro-character #\# #\D 'distributor-reader *readtable*)
    (set-dispatch-macro-character #\# #\W 'distributor-reader *readtable*)
    (set-dispatch-macro-character #\# #\M 'module-reader *readtable*)
    (set-dispatch-macro-character #\# #\S 'system-reader *readtable*)
    (set-dispatch-macro-character #\# #\A 'application-reader *readtable*)
    (set-dispatch-macro-character #\# #\R 'remote-reader *readtable*)
    (set-dispatch-macro-character #\# #\L 'locality-reader *readtable*)
    (load stream)))

(defun serialize-definitions (&optional stream)
  "Serialise global definitions to STREAM."
  (let ((*print-case* :downcase)
        (*package* #.*package*))
    (flet ((sorted-hash-table-entries (hash-table)
             (sort (hash-table-values hash-table) #'string< :key (compose #'string #'name))))
      (format stream "~&;;; -*- Mode: Lisp -*-~%;;;~%;;; Distributors~%;;;")
      (iter (for d in (sorted-hash-table-entries *distributors*)) (print d stream))
      (format stream "~%~%;;;~%;;; Modules~%;;;")
      (iter (for m in (sorted-hash-table-entries *modules*)) (unless (module-simple-p m) (print m stream)))
      (format stream "~%~%;;;~%;;; Systems~%;;;")
      (iter (for s in (sorted-hash-table-entries *systems*)) (unless (system-implied-p s) (print s stream)))
      (format stream "~%~%;;;~%;;; Applications~%;;;")
      (iter (for a in (sorted-hash-table-entries *apps*)) (print a stream)))))

(defun define-master-localities (git-path hg-path darcs-path cvs-path svn-path)
  "Define the set of master localities."
  (when-let ((bad-paths (remove-if #'directory-exists-p (list git-path hg-path darcs-path cvs-path svn-path))))
    ;; XXX: numeric
    (error "~@<The specified paths ~S are not accessible.~:@>" bad-paths))
  (let ((hostname (string-upcase (machine-instance))))
    (setf (master 'git)   (make-instance 'git-locality   :name hostname :path git-path :scan-p t)
          (master 'hg)    (make-instance 'hg-locality    :name (format-symbol t "~A-HG" hostname) :path hg-path)
          (master 'darcs) (make-instance 'darcs-locality :name (format-symbol t "~A-DARCS" hostname) :path darcs-path)
          (master 'cvs)   (make-instance 'cvs-locality   :name (format-symbol t "~A-CVS" hostname) :path cvs-path)
          (master 'svn)   (make-instance 'svn-locality   :name (format-symbol t "~A-SVN" hostname) :path svn-path))
    (pushnew (ensure-directories-exist (git-locality-asdf-registry-path (master 'git))) asdf:*central-registry* :test #'equal)
    (ensure-directories-exist (cvs-locality-lock-path (master 'cvs)))
    t))

(defun define-master-localities-in (path &key (git-subdir "git") (hg-subdir "hg") (darcs-subdir "darcs") (cvs-subdir "cvs") (svn-subdir "svn"))
  "A shortcut for master locality set definition in PATH."
  (define-master-localities
    (subdirectory* path git-subdir)
    (subdirectory* path hg-subdir)
    (subdirectory* path darcs-subdir)
    (subdirectory* path cvs-subdir)
    (subdirectory* path svn-subdir)))

(defun meta-path ()
  "Return the path to the meta directory."
  (subdirectory* (locality-path (master 'git)) ".meta"))

(defun save-current-definitions (&key (seal-p t) (commit-message "Updated DEFINITIONS") (metastore (meta-path)))
  "Save current model of the world within METASTORE.
   When SEAL-P is non-NIL, the changes are committed."
  (with-output-to-new-metafile (definitions 'definitions metastore :commit-p seal-p :commit-message commit-message)
    (serialize-definitions definitions)
    (terpri definitions)))

(defun load-definitions (&optional (metastore (meta-path)))
  "Load definitions of the world from METASTORE."
  (with-open-metafile (definitions 'definitions metastore)
    (read-definitions definitions)))

(defun update-module-locality-presence-cache (&optional (locality (master 'git)))
  "Scan LOCALITY for known modules and update those modules's idea
   of where they are present.

   Returns the list of found modules."
  (do-modules (module)
    (when (module-present-p module locality t)
      (collect module))))

(defun report (stream format-control &rest args)
  (apply #'format stream format-control args)
  (finish-output stream))

(defun ensure-some-wishmasters (meta-path &optional (wishmasters (list *default-wishmaster*)))
  (when (metafile-empty-p 'wishmasters meta-path)
    (with-output-to-new-metafile (metafile 'wishmasters meta-path :commit-p t)
      (dolist (wishmaster wishmasters)
        (write-string wishmaster metafile) (terpri metafile)))
    (report t ";;; Added wishmasters:~{ ~S~}~%" wishmasters)))

(defun ensure-present-module-systems-loadable (&optional (locality (master 'git)))
  "Ensure that all modules present in LOCALITY have their systems loadable.
   Return the list of present modules."
  (do-present-modules (module locality)
    (ensure-module-systems-loadable module locality)))

(defun set-common-wishes (modules &key seal-p (commit-message "Updated COMMON-WISHES") (meta-path (meta-path)))
  "Set META-PATH's exported name set to the set of names of MODULES."
  (with-output-to-new-metafile (metafile 'common-wishes (meta-path) :commit-p seal-p :commit-message commit-message)
    (iter (for module in modules)
          (print (name module) metafile))))

(defun required-tools-available-for-remote-type-p (type)
  "See if required executables for fetching from remotes of TYPE are present."
  (every #'find-executable
         (case type
           (git '(git))
           (hg '(hg))
           (darcs '(darcs-to-git))
           (cvs '(rsync git-cvsimport))
           (svn '(rsync git-svn)))))

(defun determine-tools-and-update-remote-accessibility ()
  "Find out which and where RCS tools are available and disable correspondingly inaccessible remotes."
  (let ((present (unzip #'required-tools-available-for-remote-type-p '(git hg darcs cvs svn))))
    (unless (member 'git present)
      (warn "~@<The git executable is not present. Desire is UNLIKELY to be of much use.~:@>"))
    (do-remotes (r)
      (setf (remote-disabled-p r) (not (member (rcs-type r) present))))))

(defmacro do-distributor-modules ((module-var distributor) &body body)
  "Execute BODY with MODULE-VAR iteratively bound to DISTRIBUTOR's modules."
  (with-gensyms (remote module-name)
    `(do-distributor-remotes (,remote ,distributor)
       (iter (for ,module-name in (location-modules ,remote))
             (for ,module-var = (module ,module-name))
             ,@body))))

(defun init (path &key as-distributor)
  "Make Desire fully functional, with PATH chosen as storage location.

   AS-DISTRIBUTOR, when specified, will be interpreted as a name of an already
   defined, well known distributor, whose modules will be designated as
   originating locally."
  (setf *root-of-all-desires* (parse-namestring path))
  (clear-definitions)
  (define-master-localities-in path)
  (when (ensure-metastore (meta-path) :required-metafiles '(definitions common-wishes wishmasters))
    (report t ";;; Ensured metastore at ~S~%" (meta-path)))
  (ensure-some-wishmasters (meta-path))
  (report t ";;; Loading definitions from ~S~%" (metafile-path 'definitions (meta-path)))
  (load-definitions (meta-path))
  (report t ";;; Determining available tools and deducing accessible remotes~%")
  (determine-tools-and-update-remote-accessibility)
  (report t ";;; Scanning for modules in ~S~%" (meta-path))
  (let ((present-git-modules (update-module-locality-presence-cache (master 'git)))
        marred-modules)
    (when as-distributor
      (setf *self* (distributor as-distributor)
            marred-modules (compute-distributor-modules *self*))
      (when ((missing (set-difference marred-modules present-git-modules)))
        (error "~@<Modules ~S are missing from *SELF*, who we pretend to be.~:@>" missing)))
    (setf (git-remote-converted-modules (master 'git)) (set-difference present-git-modules marred-modules)))
  (ensure-present-module-systems-loadable (master 'git))
  t)

(defun define-locality (name rcs-type &rest keys &key &allow-other-keys)
  "Define locality of RCS-TYPE at PATH, if one doesn't exist already, 
   in which case an error is signalled."
  (apply #'make-instance (format-symbol (symbol-package rcs-type) "~A-LOCALITY" rcs-type) :name name (remove-from-plist keys :name)))

(defun distributor-related-desires (distributor-spec)
  "Yield the names of modules currently desired from DISTRIBUTOR-SPEC."
  (rest (find (coerce-to-name distributor-spec) *desires*)))

(defun set-distributor-related-desires (new distributor-spec)
  (setf (rest (find (coerce-to-name distributor-spec) *desires*)) new))

(defsetf distributor-related-desires set-distributor-related-desires)

(defun add-desire (distributor &optional (module-spec :everything) &aux (distributor (coerce-to-distributor distributor)))
  "Add MODULE-SPEC (which is either a list of module specifications or an
   :EVERYTHING wildcard) to the list of modules desired from DISTRIBUTOR."
  (check-type module-spec (or (eql :everything) list))
  (unionf (distributor-related-desires distributor)
          (if (eq module-spec :everything)
              (compute-distributor-modules distributor)
              (mapcar #'coerce-to-name module-spec))))

(defun remote-provides-module-p (remote module &aux (remote (coerce-to-remote remote)) (module (coerce-to-module module)))
  "See whether REMOTE provides the MODULE."
  (not (or (null (find (name module) (location-modules remote)))
           (remote-disabled-p remote))))

(defun distributor-provides-module-p (distributor module &aux (module (coerce-to-module module)) (distributor (coerce-to-distributor distributor)))
  "See whether DISTRIBUTOR provides MODULE via any of its remotes, returning
   one, if so."
  (do-distributor-remotes (remote distributor)
    (when (remote-provides-module-p remote module)
      (return remote))))

(defun module-distributors (module)
  "Those distributors providing MODULE, by definition."
  (do-distributors (distributor)
    (when (distributor-provides-module-p distributor module)
      (collect distributor))))

(defun module-desired-p (module &aux (module (coerce-to-module module)))
  "See whether MODULE is desired. Return the desired distributor, if so."
  (iter (for (distributor-name . desires) in *desires*)
        (when (member (name module) desires)
          (return (distributor distributor-name)))))

(defun module-distributor (module)
  "Find out the only distributor providing MODULE, or is specified
   to be a desired distributor for it, if there is ambiguity."
  (if-let* ((distributors (module-distributors module))
            (unambiguous-p (= 1 (length distributors))))
           (first distributors)
           (module-desired-p module)))

(defun module-desired-remote (module)
  "Find the remote providing MODULE among those of its desired distributor, 
   if any."
  (when-let ((distributor (module-desired-p module)))
    (distributor-provides-module-p distributor module)))

(define-condition desire-condition (condition) ())
(define-condition desire-error (desire-condition error) ())
(define-condition repository-error (desire-error)
  ((locality :accessor condition-locality :initarg :locality)
   (module :accessor condition-module :initarg :module)))
(define-condition remote-error (desire-error)
  ((remote :accessor condition-remote :initarg :remote)))

(define-reported-condition insatiable-desire (desire-error)
  ((desire :accessor condition-desire :initarg :desire))
  (:report (desire)
           "~@<It is not known to me how to satisfy the desire for ~S.~:@>" desire))

(define-reported-condition fetch-failure (remote-error)
  ((module :accessor condition-module :initarg :module)
   (execution-error :accessor condition-execution-error :initarg :execution-error))
  (:report (remote module execution-error)
           "~@<An attempt to fetch module ~S from ~S has failed.~@:_~S~:@>" (name module) remote execution-error))

(defun module-remote (module &key (if-does-not-exist :error))
  "Find out the only remote providing MODULE, or is specified
   to be in a desired distributor for MODULE, if there is ambiguity."
  (if-let ((distributor (module-distributor module)))
    (distributor-provides-module-p distributor module)
    (ecase if-does-not-exist
      (:error (error 'insatiable-desire))
      (:continue nil))))

(defun substitute-desires (in with)
  "Substitute some of module->distributor maps in IN with those in WITH.

   IN must be compounded (one specification per distributor), whereas WITH
   is allowed to be spread, with many specifications per distributor."
  (lret ((new-desires (copy-tree in)))
    (iter (for (new-dist . modules) in with)
          (iter (for module in modules)
                (for olddistspec = (find module new-desires :key (compose (curry #'find module) #'rest)))
                (unless (eq new-dist (car olddistspec))
                  (when olddistspec
                    (removef (rest olddistspec) module))
                  (let ((new-home (or (find new-dist new-desires :key #'car)
                                      (car (push (list new-dist) new-desires)))))
                    (push module (rest new-home))))))))

(defun compute-module-caches (module)
  "Regarding MODULE, return remotes providing it, localities storing 
   (or desiring to store) it and systems it provides, as values."
  (values
   (do-remotes (remote)
     (when (remote-provides-module-p remote module)
       (collect remote)))
   (remove-duplicates
    (xform (module-desired-p module)
           (curry #'cons (master 'git))
           (module-scan-positive-localities module)))
   (remove-if #'null (map-systems (compose #'module #'system-module)))))

(defun update-module-caches (module)
  "Update MODULE's locality, remote and system caches.

   The user must never need this."
  (setf (values (module-remotes module) (module-localities module) (module-systems module))
        (compute-module-caches module)))

(defun test-core (&optional bail-out-early (pathes-from (list "/mnt/little/git/dese/definitions.lisp"
                                                              "/mnt/little/git/clung/definitions.lisp"))
                  (path-int-0 "/tmp/essential-0") (path-int-1 "/tmp/essential-1") (path-int-2 "/tmp/essential-2"))
  (clear-definitions)
  (mapcar #'load pathes-from)
  (with-output-to-file (f path-int-0)
    (serialize-definitions f))
  (when bail-out-early
    (return-from test-core))
  (clear-definitions)
  (read-definitions path-int-0)
  (with-output-to-file (f path-int-1)
    (serialize-definitions f))
  (clear-definitions)
  (read-definitions path-int-1)
  (with-output-to-file (f path-int-2)
    (serialize-definitions f)))
