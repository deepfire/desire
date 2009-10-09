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

;;;
;;; Knobs
;;;
(defvar *default-wishmaster*                       "git://git.feelingofgreen.ru")
(defvar *desires*                                  nil "List of import descriptions.")
(defvar *default-world-readable*                   t   "Whether to publish GIT repositories by default.")
(defvar *self*                                     nil "The well-known self, if any.")
(defvar *combined-remotes-prefer-native-over-http* t "Whether multi-protocol Git remotes prefer native git protocol to HTTP.")
(defvar *default-system-type*                      'asdf-system)
(defvar *fetch-errors-serious*                     nil)

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

(defun clear-definitions ()
  "Empty all global definitions."
  (dolist (var '(*distributors* *remotes* *localities* *localities-by-path* *modules* *leaves* *nonleaves* *systems* *apps* *masters*))
    (setf (symbol-value var) (make-hash-table :test #'equal))))

(defclass named ()
  ((name :accessor name :initarg :name)))

(defun coerce-to-name (o)
  (declare (type (or symbol named) o))
  (if (symbolp o) o (name o)))

(defun coerce-to-named (o)
  (declare (type (or symbol named) o))
  (if (symbolp o) (make-instance 'named :name o) o))

(defun sort-by-name (named-objects)
  "Sort object of class NAMED, lexicographically."
  (sort named-objects #'string< :key (compose #'symbol-name #'name)))

(defclass synchronisable ()
  ((mirror :accessor synchronisable-mirror :initarg :mirror)
   (last-sync-time :accessor synchronisable-last-sync-time :initarg :last-sync-time)
   (synchronised-p :accessor synchronised-p :type boolean :initarg :synchronised-p))
  (:default-initargs
   :mirror nil
   :last-sync-time 0
   :synchronised-p nil))

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
  (string-downcase (string (xform-if (of-type 'named) #'name x))))

(defvar *read-universal-time*)
;;;
;;; Distributor name is its hostname.
;;;
(defclass distributor (registered synchronisable)
  ((remotes :accessor distributor-remotes :initarg :remotes :documentation "Specified."))
  (:default-initargs
   :registrator #'(setf distributor) :remotes nil))
(defclass wishmaster () 
  ((gate-remote :accessor wishmaster-gate-remote :initarg :gate-remote)))

(defclass releasing-wishmaster (wishmaster distributor) ())
(defclass pure-wishmaster (wishmaster distributor) ())

(defmethod print-object ((o distributor) stream)
  (format stream "~@<#D(~;~A ~@<~S ~S~:@>~;)~:@>"
          (string (name o)) :remotes (distributor-remotes o)))

(defvar *printing-wishmaster* nil)

(defmethod print-object ((o wishmaster) stream)
  (if (and (typep o 'releasing-wishmaster)
           (null *printing-wishmaster*))
      (call-next-method)
      (format stream "~@<#W(~;~S ~@<~(~S ~A~)~:@>~;)~:@>"
              (git-remote-namestring (wishmaster-gate-remote o))
              :converted-modules (mapcar #'name (git-remote-converted-modules (wishmaster-gate-remote o))))))

(defvar *read-time-enclosing-distributor*)

(defun distributor-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (name &rest initargs &key remotes &allow-other-keys) (read stream nil nil t)
    `(or (distributor ',name :if-does-not-exist :continue)
         (lret ((*read-time-enclosing-distributor* (make-instance 'distributor :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t ,@(remove-from-plist initargs :remotes))))
           ,@remotes))))

(defun emit-make-simple-module-form (name)
  `(or (module ',name :if-does-not-exist :continue)
       (make-instance 'module :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :umbrella ',name)))

(defun emit-make-simple-system-form (type module-name name)
  `(or (system ',name :if-does-not-exist :continue)
       (make-instance ',type :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :module (module ',module-name))))

(defun emit-remote-form (type name modules simple-modules simple-systemless-modules path-components remote-initargs)
  `(prog1
       (make-instance ',type ,@(when name `(:name ',name)) :last-sync-time ,*read-universal-time* :synchronised-p t :distributor *read-time-enclosing-distributor*
                      :path ',path-components :modules ',(append modules simple-modules simple-systemless-modules)
                      ,@remote-initargs)
     ,@(mapcar #'emit-make-simple-module-form (append simple-modules simple-systemless-modules))
     ,@(mapcar (lambda (name) (emit-make-simple-system-form *default-system-type* name name)) simple-modules)))

(defmacro do-distributor-remotes ((var distributor &optional block-name) &body body)
  `(iter ,@(when block-name `(,block-name)) (for ,var in (distributor-remotes (coerce-to-distributor ,distributor)))
         ,@body))

(defmacro do-distributor-modules ((module-var distributor) &body body)
  "Execute BODY with MODULE-VAR iteratively bound to DISTRIBUTOR's modules."
  (with-gensyms (remote module-name block)
    `(do-distributor-remotes (,remote ,distributor ,block)
       (iter (for ,module-name in (location-modules ,remote))
             (for ,module-var = (module ,module-name))
             (in ,block ,@body)))))

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

(defvar *class-slot-store* (make-hash-table :test 'equal))

(define-root-container *class-slot-store* %class-slot :type t :if-exists :continue)

(defun class-slot (&rest class-slot-name)
  (%class-slot class-slot-name))

(defun set-class-slot (class-name slot-name value)
  (setf (%class-slot (list class-name slot-name)) value))

(defsetf class-slot set-class-slot)

(defmacro with-class-slot (classes slot-name &body body)
  `(symbol-macrolet ,(iter (for class in classes) (collect `(,class (class-slot ',class ',slot-name))))
     ,@body))

(defun rcs-enabled-p (type)
  (class-slot type 'enabled-p))

(defun find-and-register-tools-for-remote-type (type)
  "Find and make available executables for fetching from remotes of TYPE.
   Return T when all executables required by TYPE are available, or NIL."
  (with-class-slot (git hg darcs cvs svn) required-executables
    (setf git '(git) hg '(hg)  darcs '(darcs darcs-to-git) cvs '(rsync git) svn '(rsync git)))
  (with-class-slot (git hg darcs cvs svn) enabled-p
    (setf git nil hg nil darcs nil cvs nil svn nil))
  (setf (class-slot type 'enabled-p)
        (every #'find-executable (class-slot type 'required-executables))))

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

(defclass location (registered synchronisable)
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
   (domain-name-takeover :accessor remote-domain-name-takeover :initarg :domain-name-takeover :documentation "Specified.")
   (distributor-port :accessor remote-distributor-port :type (or null (integer 0 65536)) :initarg :distributor-port :documentation "Specified, rarely.")
   (path :accessor remote-path :initarg :path :documentation "Specified.")
   (path-fn :accessor remote-path-fn :initarg :path-fn :documentation "Generated from above.")
   (disabled-p :accessor remote-disabled-p :type boolean :initarg :disabled-p :documentation "Specified."))
  (:default-initargs
   :registrator #'(setf remote)
   :disabled-p nil
   :distributor-port nil
   :domain-name-takeover nil))

;;; intermediate types
(defclass git-remote (git remote)
  ((converted-modules :accessor git-remote-converted-modules :initarg :converted-modules :documentation "Cache."))
  (:default-initargs
   :converted-modules nil))

;;; most specific, exhaustive partition of REMOTE
(defclass git-native-remote (git-native git-remote) ())
(defclass git-http-remote (git-http git-remote) ())
(defclass git-combined-remote (git-native git-http git-remote) ())
(defclass hg-http-remote (hg-http remote) ())
(defclass darcs-http-remote (darcs-http remote) ())
(defclass cvs-rsync-remote (cvs-rsync remote) ())
(defclass svn-rsync-remote (svn-rsync remote) ())

;;; most specific, exhaustive partition of LOCALITY
(defclass git-locality (git locality) ())
(defclass hg-locality (hg locality) ())
(defclass darcs-locality (darcs locality) ())
(defclass cvs-locality (cvs locality) ())
(defclass svn-locality (svn locality) ())

(defmethod rcs-type ((o (eql 'git-native-remote))) 'git)
(defmethod rcs-type ((o (eql 'git-http-remote))) 'git)
(defmethod rcs-type ((o (eql 'git-combined-remote))) 'git)
(defmethod rcs-type ((o (eql 'hg-http-remote))) 'hg)
(defmethod rcs-type ((o (eql 'darcs-http-remote))) 'darcs)
(defmethod rcs-type ((o (eql 'cvs-rsync-remote))) 'cvs)
(defmethod rcs-type ((o (eql 'svn-rsync-remote))) 'svn)

(defmethod transport ((o git-combined-remote))
  (if *combined-remotes-prefer-native-over-http*
      'git 'http))

(defun default-remote-name (distributor-name rcs-type)
  "Compute a default name for remote with RCS-TYPE in DISTRIBUTOR-NAME."
  (if (eq rcs-type 'git)
      distributor-name
      (format-symbol (symbol-package distributor-name) "~A~:[-~A~;~]" distributor-name (eq rcs-type 'git) rcs-type)))

(defun choose-default-remote-name (distributor rcs-type)
  "Try choose a default name for a remote with RCS-TYPE on DISTRIBUTOR.
When there's a name clash NIL is returned."
  (let ((default-name (default-remote-name (name distributor) rcs-type)))
    (when-let ((non-conflicting-p (null (find default-name (distributor-remotes distributor) :key #'name))))
      default-name)))

(defun system-simple-p (system)
  "Determine whether SYSTEM meets the requirements for a simple system."
  (and (null (system-search-restriction system))
       (null (system-definition-pathname-name system))))

(defun module-simple-p (module)
  "Determine whether MODULE meets the requirements for a simple module."
  (and (eq (name module) (module-umbrella module))
       (not (module-essential-p module))
       (or (null (module-systems module))
           (and (endp (rest (module-systems module)))
                (system-simple-p (first (module-systems module)))))))

(defmethod print-object ((o remote) stream &aux (default-remote-name (with-standard-io-syntax (default-remote-name (name (remote-distributor o)) (rcs-type o)))))
  (let ((*print-case* :downcase))
    (format stream "~@<#R(~;~A ~S~{ ~<~S ~A~:@>~}~;)~:@>"
            (symbol-name (type-of o)) (remote-path o)
            (multiple-value-bind (simple complex) (unzip #'module-simple-p (location-modules o) :key #'module)
              (multiple-value-bind (systemful systemless) (unzip #'module-systems simple :key #'module)
                (append (unless (equal default-remote-name (name o))
                          (list `(:name ,(string (name o)))))
                        (when-let ((port (remote-distributor-port o)))
                          (list `(:distributor-port ,port)))
                        (when complex
                          (list `(:modules ,(mapcar #'downstring complex))))
                        (when systemful
                          (list `(:simple-modules ,(mapcar #'downstring systemful))))
                        (when systemless
                          (list `(:simple-systemless-modules ,(mapcar #'downstring systemless))))))))))

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
          (t (or (choose-default-remote-name distributor rcs-type)
                 (error "~@<Cannot choose an unambiguous name for a ~A remote in distributor ~A, provide one explicitly.~:@>"
                        rcs-type distributor-name))))))

(defmethod initialize-instance :before ((o remote) &key distributor rcs-type name &allow-other-keys)
  (setf (name o) (init-time-collate-remote-name distributor rcs-type name)))

(defmethod initialize-instance :after ((o remote) &key distributor path &allow-other-keys)
  (appendf (distributor-remotes distributor) (list o))
  (setf (remote-path-fn o) (compile nil `(lambda ()
                                           (declare (special *module* *umbrella*))
                                           (list ,@path)))))

(defun system-implied-p (system)
  "See it the definition of SYSTEM is implied, and is therefore subject to omission. "
  (system-simple-p system))

(defun remote-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (type path-components &rest initargs &key modules simple-modules simple-systemless-modules name &allow-other-keys) (read stream nil nil)
    (emit-remote-form type name modules simple-modules simple-systemless-modules path-components
                      (remove-from-plist initargs :name :distributor :type :modules :simple-modules :simple-systemless-modules))))

;;;
;;; NOTE: this is the reason why remotes have names
;;;
(defun git-fetch-remote (remote module-name &optional locality-path)
  "Fetch from REMOTE, with working directory optionally changed
to LOCALITY-PATH."
  (maybe-within-directory locality-path
    (let ((module-url (url remote module-name)))
      (ensure-gitremote (name remote) module-url))
    (with-explanation ("fetching from remote ~A in ~S" (name remote) *default-pathname-defaults*)
      (git "fetch" (down-case-name remote)))
    (ensure-master-branch-from-remote :remote-name (name remote))))

(defmacro within-wishmaster-meta ((wishmaster &key (metastore '(meta-path)) update-p) &body body)
  (once-only (wishmaster metastore)
    `(within-directory (,metastore)
       ,@(when update-p `((git-fetch-remote (wishmaster-gate-remote ,wishmaster) :.meta)))
       (within-ref (list "remotes" (down-case-name ,wishmaster) "master")
         ,@body))))

(defun locality-master-p (o)
  (eq o (master (rcs-type o) :if-does-not-exist :continue)))

(defmethod print-object ((o locality) stream)
  (format stream "~@<#L(~;~A ~A ~S~{ ~<~S ~S~:@>~}~;)~:@>"
          (symbol-name (type-of o)) (string (name o)) (locality-path o)
          (append (when (locality-master-p o) (list (list :master-p t)))
                  (when (locality-scan-p o) (list (list :scan-p t))))))

(defun locality-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (type name path &rest initargs &key &allow-other-keys) (read stream nil nil)
    `(make-instance ',type :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :path ,path ,@(remove-from-plist initargs :path :modules))))

(defmethod initialize-instance :after ((o locality) &key path &allow-other-keys)
  (unless path
    (error "~@<A location without path specified is useless. ~S is one of many.~:@>" o))
  (setf (locality-by-path path) o))

(defun uri-type-to-remote-type (uri-type)
  (switch (uri-type :test #'string=)
    ("http" 'git-http-remote)
    ("git" 'git-native-remote)
    ("git+http" 'git-combined-remote)
    ("darcs" 'darcs-http-remote)
    ("cvs" 'cvs-rsync-remote)
    ("svn" 'svn-rsync-remote)))

(defun parse-remote-namestring (namestring)
  "Given a remote NAMESTRING, deduce the remote's type, host, port and path,
and return them as multiple values.
Note that http is interpreted as git-http -type remote.
DARCS/CVS/SVN need darcs://, cvs:// and svn:// schemas, correspondingly."
  (let* ((colon-pos (or (position #\: namestring) (error "No colon in git remote namestring ~S." namestring)))
         (typestr (subseq namestring 0 colon-pos))
         (type (or (uri-type-to-remote-type typestr)
                   (error "Bad URI type ~S in remote namestring ~S." typestr namestring))))
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
  (url remote nil))

(defun make-pure-wishmaster (type hostname port path)
  "Produces a pure wishmaster with Git remote of TYPE, accessible at 
   HOSTNAME, PORT and PATH."
  (lret ((wishmaster (make-instance 'pure-wishmaster :name hostname)))
    (let ((gate-remote (make-instance type :distributor wishmaster :distributor-port port :path path)))
      (push gate-remote (distributor-remotes wishmaster))
      (setf (wishmaster-gate-remote wishmaster) gate-remote))))

(defun ensure-pure-wishmaster (url)
  "Return a non-well-known pure wishmaster specified by URL, which is
   intepreted as pointing at its git remote.
   The wishmaster is either found, or created, if it is not yet known."
  (multiple-value-bind (type hostname port path) (parse-remote-namestring url)
    (let ((path (append path '(*module*))))
      (or (distributor hostname :if-does-not-exist :continue)
          (let* ((wishmaster (make-pure-wishmaster type hostname port path))
                 (remote (wishmaster-gate-remote wishmaster)))
            (within-directory ((meta-path))
              (ensure-gitremote (name remote) (url remote '.meta)))
            wishmaster)))))

(defun wishmaster-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (url &key converted-modules) (read stream nil nil)
    `(lret ((converted-modules ',converted-modules)
            (wishmaster (ensure-pure-wishmaster ,url)))
       (let ((gate-remote (wishmaster-gate-remote wishmaster))
             (modules (list ,@(mapcar #'emit-make-simple-module-form converted-modules))))
         (setf (location-modules gate-remote) (mapcar #'name modules))
         (dolist (m modules)
           (push gate-remote (module-remotes m)))
         ,@(mapcar (lambda (name) (emit-make-simple-system-form *default-system-type* name name)) converted-modules)
         (setf (wishmaster-converted-modules wishmaster) (mapcar #'module converted-modules))))))

(defun ensure-wishmaster (wishmaster-spec)
  "When WISHMASTER-SPEC is a symbol, find the distributor it names,
   and mark it as being a releasing wishmaster.
   When WISHMASTER-SPEC is a string, it is interpreted as an URL to
   a Git remote of a non-well-known pure-wishmaster, and is used
   to ensure its existence.
   In both cases the wishmaster object is returned.
   In other cases, a type error is signalled."
  (lret ((wishmaster (funcall (etypecase wishmaster-spec
                                (symbol (compose (rcurry #'change-class 'releasing-wishmaster) #'distributor))
                                (string #'ensure-pure-wishmaster))
                              wishmaster-spec)))
    (let ((gate-remote (distributor-remote-if (of-type 'git-remote) wishmaster)))
      (unless gate-remote
        (error "~@<Can't promote ~S to a wishmaster, as it's lacking a gate remote.~:@>" wishmaster))
      (unless (slot-boundp wishmaster 'gate-remote)
        (setf (wishmaster-gate-remote wishmaster) gate-remote)))
    (do-distributor-modules (m wishmaster)
      (change-class m 'origin-module))))

(defun distributor-release-git-modules (distributor)
  "Return the list of release modules provided by DISTRIBUTOR via git."
  (mapcar #'module (location-modules (wishmaster-gate-remote distributor))))

(defun wishmaster-converted-modules (wishmaster)
  "Return the list of modules converted by WISHMASTER."
  (git-remote-converted-modules (wishmaster-gate-remote wishmaster)))

(defun set-wishmaster-converted-modules (wishmaster new)
  "Set the list of modules converted by WISHMASTER."
  (setf (git-remote-converted-modules (wishmaster-gate-remote wishmaster)) new))

(defsetf wishmaster-converted-modules set-wishmaster-converted-modules)

(defclass module (registered depobj synchronisable)
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

(defclass origin-module (module) 
  ((status :accessor module-status :initarg :status)
   (public-packages :accessor module-public-packages :initarg :public-packages)
   (hidden-p :accessor module-hidden-p :initarg :hidden-p))
  (:default-initargs
   :status :unknown
   :public-packages nil
   :hidden-p t))

(defmethod print-object ((o module) stream)
  (format stream "~@<#M(~;~A~{ ~<~S ~A~:@>~}~;)~:@>" (if (eq (name o) (module-umbrella o))
                                                          (symbol-name (name o))
                                                          (list (symbol-name (name o)) (module-umbrella o)))
          (remove nil (multiple-value-call #'list
                        (destructuring-bind (&optional first &rest other-systems) (module-systems o)
                          (unless (and first (null other-systems) (system-simple-p first))
                            (multiple-value-bind (simple complex) (unzip #'system-simple-p (module-systems o))
                              (values (when simple
                                        (list :systems (mapcar #'down-case-name simple)))
                                      (when complex
                                        (list :complex-systems (mapcar #'down-case-name complex)))))))
                        (when (module-essential-p o)
                          (list :essential-p (module-essential-p o)))))))

(defmethod initialize-instance :around ((o module) &key name &allow-other-keys)
  (when (module name :if-does-not-exist :continue)
    (break))
  (call-next-method))

(defvar *module*)
(defvar *umbrella*)

(defun url (remote-or-name &optional module-or-name &aux
            (namep (symbolp module-or-name))
            (remote (coerce-to-remote remote-or-name))
            (module (if namep (module module-or-name :if-does-not-exist :continue) module-or-name))
            (module-name (if namep module-or-name (name module))))
  (declare (type (or remote symbol) remote) (type (or module symbol) module-or-name))
  (let* ((remote-path (let ((*module* (when module-name (downstring module-name)))
                            (*umbrella* (when module (downstring (module-umbrella module)))))
                        (declare (special *module* *umbrella*))
                        (funcall (remote-path-fn remote))))
         (remote-string-list (iter (for insn-spec in remote-path)
                                   (cond ((eq insn-spec :no/)
                                          (pop accumulated-path))
                                         (t
                                          (when insn-spec
                                            (collect insn-spec into accumulated-path at beginning)
                                            (collect "/" into accumulated-path at beginning))))
                                   (finally (return (nreverse accumulated-path))))))
    (apply #'concatenate 'simple-base-string
           (downstring (transport remote)) "://"
           (unless (remote-domain-name-takeover remote)
             (down-case-name (remote-distributor remote)))
           (unless (remote-domain-name-takeover remote)
             (when-let ((port (remote-distributor-port remote)))
               (format nil ":~D" port)))
           (unless (remote-domain-name-takeover remote)
             "/")
           remote-string-list)))

(defun module-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (name &rest initargs &key (systems nil systems-specified-p) complex-systems &allow-other-keys) (read stream nil nil t)
    (destructuring-bind (name umbrella) (if (consp name) name (list name name))
      `(or (module ',name :if-does-not-exist :continue)
           (prog1 (make-instance 'module :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :umbrella ',umbrella
                                 ,@(remove-from-plist initargs :systems :complex-systems))
             ;; The simple system -- this case is used only when the module is not simple itself, i.e. when it's got a non-default umbrella name.
             ,@(if systems-specified-p
                   (mapcar (curry #'emit-make-simple-system-form *default-system-type* name) systems)
                   ;; Existence of complex systems implies absence of simple systems, by default.
                   (unless complex-systems
                     `(,(emit-make-simple-system-form *default-system-type* name name)))))))))

(defclass system-type-mixin ()
  ((pathname-type :accessor system-pathname-type :initarg :pathname-type)))

(defclass asdf (system-type-mixin) () (:default-initargs :pathname-type "asd"))
(defclass mudball (system-type-mixin) () (:default-initargs :pathname-type "mb"))
(defclass xcvb (system-type-mixin) () (:default-initargs :pathname-type "xcvb"))

(defclass system (registered synchronisable)
  ((module :accessor system-module :initarg :module :documentation "Specified.")
   (search-restriction :accessor system-search-restriction :initarg :search-restriction :documentation "Specified.")
   (relativity :accessor system-relativity :initarg :relativity :documentation "Specified.")
   (definition-pathname-name :accessor system-definition-pathname-name :initarg :definition-pathname-name :documentation "Specified.")
   (applications :accessor system-applications :initarg :applications :documentation "Cache."))
  (:default-initargs
   :registrator #'(setf system)
   :search-restriction nil :definition-pathname-name nil
   :module nil :applications nil :relativity nil))

(defclass asdf-system (asdf system) ())
(defclass mudball-system (mudball system) ())
(defclass xcvb-system (xcvb system) ())

(defun system-definition-canonical-pathname-name (system)
  "Return the canonical definition patname name for SYSTEM."
  (downstring (name system)))

(defmethod print-object ((o system) stream)
  (format stream "~@<#S(~;~A~{ ~S~}~;)~:@>" (symbol-name (name o))
          (append (list :module (name (system-module o)))
                  (and (system-relativity o) (list :relativity (system-relativity o)))
                  (and (system-search-restriction o) (list :search-restriction (system-search-restriction o)))
                  (and (system-definition-pathname-name o) (list :definition-pathname-name (system-definition-pathname-name o)))
                  (and (system-applications o) (list :applications (system-applications o))))))

(defun system-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (name &rest initargs &key module relativity search-restriction &allow-other-keys) (read stream nil nil t)
    `(or (system ',name :if-does-not-exist :continue)
         (make-instance *default-system-type* :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :module (module ',module) :relativity ',relativity
                        ,@(when search-restriction `(:search-restriction ',search-restriction))
                        ,@(remove-from-plist initargs :module :applications :relativity :search-restriction)))))

(defmethod initialize-instance :after ((o system) &key module &allow-other-keys)
  (appendf (module-systems module) (list o)))

(defclass application (registered synchronisable)
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

(defun application-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (name &rest initargs &key system package function default-parameters &allow-other-keys) (read stream nil nil t)
    `(or (app ',name :if-does-not-exist :continue)
         (make-instance 'application :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t
                        :system (system ',system) :package ',package :function ',function :default-parameters ',default-parameters
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

(define-root-container *distributors*   distributor   :name-transform-fn coerce-to-namestring :remover %remove-distributor :coercer t :mapper map-distributors :iterator do-distributors)
(define-root-container *modules*        module        :name-transform-fn coerce-to-namestring :remover %remove-module :coercer t :mapper map-modules :if-exists :error :iterator do-modules)
(define-root-container *leaves*         leaf          :name-transform-fn coerce-to-namestring :type module :mapper map-leaves :if-exists :continue)
(define-root-container *nonleaves*      nonleaf       :name-transform-fn coerce-to-namestring :type module :mapper map-nonleaves :if-exists :continue)
(define-root-container *systems*        system        :name-transform-fn coerce-to-namestring :remover %remove-system :coercer t :mapper map-systems)
(define-root-container *apps*           app           :name-transform-fn coerce-to-namestring :remover %remove-app :coercer t :mapper map-apps :type application)
(define-root-container *remotes*        remote        :name-transform-fn coerce-to-namestring :remover %remove-remote :coercer t :mapper map-remotes :type remote :if-exists :error :iterator do-remotes)
(define-root-container *localities*     locality      :type locality :mapper map-localities :if-exists :error)
(define-root-container *localities-by-path* locality-by-path :type locality :if-exists :error)
(define-root-container *masters*        master        :type locality :if-exists :error)

(defun remove-distributor (distributor-designator &aux (d (coerce-to-distributor distributor-designator)))
  (do-distributor-remotes (r d)
    (do-remove-remote r))
  (%remove-distributor (name d)))

(defun do-remove-remote (r)
  (dolist (m (location-modules r))
    (when-let ((m (module m :if-does-not-exist :continue)))
      (do-remove-module (module m))))
  (%remove-remote (name r)))

(defun remove-remote (remote-designator &aux (r (coerce-to-remote remote-designator)))
  (removef (distributor-remotes (remote-distributor r)) r)
  (do-remove-remote r))

(defun do-remove-module (m)
  (dolist (s (module-systems m))
    (do-remove-system s))
  (%remove-module (name m)))

(defun remove-module (module-designator &aux (m (coerce-to-module module-designator)))
  (iter (for remote in (compute-module-remotes m))
        (removef (location-modules remote) m))
  (do-remove-module m))

(defun do-remove-system (s)
  (dolist (a (system-applications s))
    (%remove-app a))
  (%remove-system (name s)))

(defun remove-system (system-designator &aux (s (coerce-to-system system-designator)))
  (removef (module-systems (system-module s)) s)
  (do-remove-system s))

(defun remove-app (app-designator &aux (a (coerce-to-application app-designator)))
  (removef (system-applications (app-system a)) a)
  (%remove-app (name a)))

(defun determine-tools-and-update-remote-accessibility ()
  "Find out which and where RCS tools are available and disable correspondingly inaccessible remotes."
  (let ((present (unzip #'find-and-register-tools-for-remote-type '(hg darcs cvs svn))))
    (do-remotes (r)
      (setf (remote-disabled-p r) (not (member (rcs-type r) present))))))

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

(defmacro with-definition-read-context (&body body)
  `(let ((*readtable* (copy-readtable))
         (*read-eval* nil)
         (*read-universal-time* (get-universal-time))
         (*package* #.*package*))
     ,@body))

(defun read-definitions (stream)
  "Unserialise global definitions from STREAM."
  (with-definition-read-context
    (set-dispatch-macro-character #\# #\D 'distributor-reader *readtable*)
    (set-dispatch-macro-character #\# #\W 'wishmaster-reader *readtable*)
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

(defun define-master-locality-in (root-path rcs-type &aux (hostname (format-symbol #.*package* "~(~A~)" (machine-instance))))
  "Define a locality of RCS-TYPE in a subdirectory of ROOT-PATH."
  (lret* ((locality-type (find-class (format-symbol #.*package* "~A-LOCALITY" rcs-type)))
          (path (subdirectory* root-path (string-downcase (string rcs-type))))
          (locality (make-instance locality-type :name (default-remote-name hostname rcs-type) :path path)))
    (unless (directory-exists-p path)
      (ensure-directories-exist path))
    (setf (master rcs-type) locality)
    (case rcs-type
      (git (pushnew (ensure-directories-exist (git-locality-asdf-registry-path locality)) asdf:*central-registry* :test #'equal))
      (cvs (ensure-directories-exist (cvs-locality-lock-path locality) :verbose t)))))

(defun define-master-localities-in (path)
  "A shortcut for master locality set definition in PATH."
  (mapc (curry #'define-master-locality-in path) '(git hg darcs cvs svn)))

(defun meta-path ()
  "Return the path to the meta directory."
  (subdirectory* (locality-path (master 'git)) ".meta"))

(defun save-current-definitions (&key seal-p (commit-message "Updated DEFINITIONS") (metastore (meta-path)))
  "Save current model of the world within METASTORE.
   When SEAL-P is non-NIL, the changes are committed."
  (with-output-to-new-metafile (definitions 'definitions metastore :commit-p seal-p :commit-message commit-message)
    (serialize-definitions definitions)
    (terpri definitions)))

(defun load-definitions (&optional (metastore (meta-path)))
  "Load definitions of the world from METASTORE."
  (with-open-metafile (definitions 'definitions metastore)
    (read-definitions definitions)))

(defun advertise-wishmaster-conversions (wishmaster &optional (metastore (meta-path)))
  "Advertise the set of modules converted by WISHMASTER at METASTORE."
  (with-output-to-new-metafile (common-wishes 'common-wishes metastore :commit-p t)
    (let ((*printing-wishmaster* t))
      (print wishmaster common-wishes))))

(defun module-present-p (module-or-name &optional (locality (master 'git)) check-when-present-p (check-when-missing-p t)
                         &aux (module (coerce-to-module module-or-name)))
  "See if MODULE's presence cache is positive for LOCALITY, failing that check the
   repository, and update the cache, accordingly to the result.

   CHECK-WHEN-PRESENT-P determines if presence check is performed when MODULE's cache
   is positive.
   CHECK-WHEN-MISSING-P defermines if presence check is performed upon negative
   cache results."
  (with-slots (scan-positive-localities) module
    (labels ((update-presence-in (locality)
               (lret ((presence (repository-present-p (module-path module locality))))
                 (if presence
                     (pushnew locality scan-positive-localities)
                     (removef scan-positive-localities locality)))))
      (if-let ((cache-hit (find locality scan-positive-localities)))
        (if check-when-present-p
            (update-presence-in locality)
            t)
        (if check-when-missing-p
            (update-presence-in locality)
            nil)))))

(defmacro do-present-modules ((module &optional (locality '(master 'git))) &body body)
  "Iterate the BODY over all modules cached as present in LOCALITY, with MODULE specifying
   the driven variable binding."
  `(do-modules (,module)
     (when (module-present-p ,module ,locality nil nil)
       ,@body)))

(defun compute-module-locality-presence (&optional (locality (master 'git)))
  "Scan LOCALITY for known modules and update those modules's idea
   of where they are present.

   Returns the list of found modules."
  (do-modules (module)
    (when (module-present-p module locality t)
      (collect module))))

(defun wishmaster-recompute-and-advertise-exports (wishmaster present-set &optional (metastore (meta-path)))
  (when (typep wishmaster 'releasing-wishmaster)
    (let ((release-set (distributor-release-git-modules wishmaster)))
      (when-let ((missing (set-difference release-set present-set)))
        (error "~@<Modules ~S are missing from distributor ~S, which we pretend to be.~:@>" missing (name wishmaster)))))
  (let* ((old-converted-set (wishmaster-converted-modules wishmaster))
         (new-converted-set (set-difference present-set (distributor-release-git-modules wishmaster))))
    (setf (wishmaster-converted-modules wishmaster) new-converted-set)
    (advertise-wishmaster-conversions wishmaster metastore)
    (values (set-difference new-converted-set old-converted-set)
            (set-difference old-converted-set new-converted-set))))

(defun set-up-default-wishmaster-list (wishmaster-urls &optional (metastore (meta-path)))
  (with-output-to-new-metafile (metafile 'wishmasters metastore :commit-p t)
    (dolist (url wishmaster-urls)
      (print url metafile))))

(defun recall-wishmasters (default-wishmaster-urls &optional (metastore (meta-path)))
  "Remember other wishmasters still walking the planes..."
  (when (metafile-empty-p 'wishmasters metastore)
    (set-up-default-wishmaster-list default-wishmaster-urls metastore))
  (with-open-metafile (wishmasters 'wishmasters metastore)
    (dolist (url (all-stream-forms wishmasters))
      (ensure-wishmaster url))))

(defun update-wishmaster (wishmaster &optional (locality (master 'git)) (metastore (meta-path)))
  (cond ((eq wishmaster *self*)
         (wishmaster-recompute-and-advertise-exports wishmaster (compute-module-locality-presence locality) metastore))
        (t
         (within-wishmaster-meta (wishmaster :metastore metastore :update-p t)
           (with-open-metafile (common-wishes 'common-wishes metastore)
             (with-definition-read-context
               (set-dispatch-macro-character #\# #\W 'wishmaster-reader *readtable*)
               (load common-wishes)))))))

(defun update-known-wishmasters (&optional (locality (master 'git)) (metastore (meta-path)))
  (do-distributors (d)
    (when (and (typep d 'wishmaster)
               (not (eq d *self*)))
      (update-wishmaster d locality metastore))))

(defun ensure-present-module-systems-loadable (&optional (locality (master 'git)))
  "Ensure that all modules present in LOCALITY have their systems loadable.
   Return the list of present modules."
  (do-present-modules (module locality)
    (ensure-module-systems-loadable module locality)))

(defun report (stream format-control &rest args)
  (apply #'format stream format-control args)
  (finish-output stream))

(defun pathname-absolute-p (pathname)
  (eq (car (pathname-directory pathname)) :absolute))

(defun init (path &key as (default-wishmasters (list *default-wishmaster*)))
  "Make Desire fully functional, with PATH chosen as storage location.

   AS, when specified, will be interpreted as a distributor specification,
   that is, either a string, for an URL pointing to a Git remote of a 
   non-well-known converting wishmaster, or a symbol, naming a well-known
   distributor. That distributor will be further identified as self.
   DEFAULT-WISHMASTERS specifies a set of wishmaster specifications which
   are seeded into the system iff there is none of these yet."
  (let* ((path (fad:pathname-as-directory path))
         (path (if (pathname-absolute-p path)
                   path
                   (merge-pathnames path))))
    (setf *root-of-all-desires* (parse-namestring path))
    (clear-definitions)
    (define-master-localities-in path)
    (unless (find-and-register-tools-for-remote-type 'git)
      (error "The git executable is missing, and so, DESIRE is of no use."))
    (when (ensure-metastore (meta-path) :required-metafiles '(definitions common-wishes wishmasters))
      (report t ";;; Ensured metastore at ~S~%" (meta-path)))
    (report t ";;; Loading definitions from ~S~%" (metafile-path 'definitions (meta-path)))
    (load-definitions (meta-path))
    (report t ";;; Determining available tools and deducing accessible remotes~%")
    (determine-tools-and-update-remote-accessibility)
    (report t ";;; Remembering other wishmasters still walking the planes...~%")
    (recall-wishmasters default-wishmasters (meta-path))
    (report t ";;; Scanning for modules in ~S...  " *root-of-all-desires*)
    (let* ((master (master 'git))
           (present-git-modules (compute-module-locality-presence master)))
      (report t "~D of them.~%" (length present-git-modules))
      (if-let ((wishmaster (and as (ensure-wishmaster as))))
        (progn
          (report t ";;; Advertising self as a wishmaster~%")
          (setf *self* wishmaster
                (wishmaster-converted-modules wishmaster) present-git-modules)
          (wishmaster-recompute-and-advertise-exports wishmaster present-git-modules))
        (progn
          (report t ";;; Retrieving information about known wishmasters~%")
          (update-known-wishmasters master)))
      (report t ";;; Ensuring that present modules have their defined systems accessible~%")
      (ensure-present-module-systems-loadable master))
    (report t ";;; All done.~%")
    t))

(defun define-locality (name rcs-type &rest keys &key &allow-other-keys)
  "Define locality of RCS-TYPE at PATH, if one doesn't exist already, 
   in which case an error is signalled."
  (apply #'make-instance (format-symbol (symbol-package rcs-type) "~A-LOCALITY" rcs-type) :name name (remove-from-plist keys :name)))

;;;
;;; Conditions.
;;;
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

;;;
;;; Queries.
;;;
(defun remote-defines-module-p (remote module &aux (module (coerce-to-module module)))
  "See whether MODULE is defined for REMOTE."
  (not (null (find (name module) (location-modules remote)))))

(defun module-accessible-via-remote-p (remote module &aux (remote (coerce-to-remote remote)) (module (coerce-to-module module)))
  "See whether REMOTE provides the MODULE, i.e. that MODULE is defined,
   and the remote is not disabled."
  (and (remote-defines-module-p remote module)
       (not (remote-disabled-p remote))))

(defun compute-module-remotes (module)
  "Compute the set of all remotes through which MODULE is available."
  (do-remotes (remote)
    (collecting (remote-defines-module-p remote module))))

(defun module-enabled-remote (module)
  "Return the first non-disabled remote providing MODULE.
   The second value is a boolean, indicating non-emptiness of the set of
   providing remotes, regardless of the enabled-p flag."
  (apply/find-if (complement #'remote-disabled-p) #'compute-module-remotes module))

(defun module-remote (module &key (if-does-not-exist :error))
  "Find the first remote occuring to provide MODULE."
  (let ((module (coerce-to-module module)))
    (or (do-remotes (remote)
          (finding remote such-that (remote-defines-module-p remote module)))
        (ecase if-does-not-exist
          (:error (error 'insatiable-desire :desire module))
          (:continue nil)))))

(defun distributor-module-remotes (distributor module &aux (module (coerce-to-module module)) (distributor (coerce-to-distributor distributor)))
  "Return the set of DISTRIBUTOR's remotes providing MODULE, regardless
   of the current availability."
  (remove-if-not (rcurry #'remote-defines-module-p module) (distributor-remotes distributor)))

(defun distributor-module-enabled-remote (distributor module &aux (module (coerce-to-module module)) (distributor (coerce-to-distributor distributor)))
  "Return the first non-disabled DISTRIBUTOR's remote providing MODULE.
   The second value is a boolean, indicating non-emptiness of the set of
   providing remotes, regardless of the enabled-p flag."
  (apply/find-if (complement #'remote-disabled-p) #'distributor-module-remotes distributor module))

(defun module-distributor (module &key (if-does-not-exist :error))
  "Find the first distributor occuring to provide MODULE."
  (remote-distributor (module-remote module :if-does-not-exist if-does-not-exist)))

;;;
;;; Desires.
;;;
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

(defun module-desired-p (module &aux (module (coerce-to-module module)))
  "See whether MODULE is desired. Return the desired distributor, if so."
  (iter (for (distributor-name . desires) in *desires*)
        (when (member (name module) desires)
          (return (distributor distributor-name)))))

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

;;;
;;; Rudimentary caching.
;;;
(defun compute-module-caches (module)
  "Regarding MODULE, return remotes defining it, localities storing 
   (or desiring to store) it and systems it provides, as values."
  (values
   (do-remotes (remote)
     (when (remote-defines-module-p remote module)
       (collect remote)))
   (remove-duplicates
    (xform (module-desired-p module)
           (curry #'cons (master 'git))
           (module-scan-positive-localities module)))
   (remove-if #'null (map-systems (compose #'module #'system-module)))))

(defun update-module-caches (module)
  "Update MODULE's locality, remote and system caches.

   The user must never need this."
  (with-slots (remotes localities systems) module
    (setf (values remotes localities systems) (compute-module-caches module))))

(defun test-core (&optional bail-out-early (pathes-from (list "/little/git/desire/definitions.lisp"))
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
