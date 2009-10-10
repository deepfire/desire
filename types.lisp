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
(defvar *bootstrap-wishmaster-url*                "git://git.feelingofgreen.ru")
(defvar *desires*                                  nil "List of import descriptions.")
(defvar *default-world-readable*                   t   "Whether to publish GIT repositories by default.")
(defvar *self*                                     nil "Possibly unknown distributor whom we identify as.")
(defvar *combined-remotes-prefer-native-over-http* t   "Whether multi-protocol Git remotes prefer native git protocol to HTTP.")
(defvar *default-system-type*                      'asdf-system)
(defvar *fetch-errors-serious*                     nil)
(defvar *merge-remote-wishmasters*                 t   "Whether to merge definitions from remote wishmasters.")

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

(defun clear-definitions ()
  "Empty all global definitions."
  (dolist (var '(*distributors* *remotes* *localities* *localities-by-path* *modules* *leaves* *nonleaves* *systems* *apps*))
    (setf (symbol-value var) (make-hash-table :test #'equal))))

;;;;
;;;; Distributor
;;;;
;;;
;;; Note: distributor name is its hostname.
;;;
(defclass distributor (registered synchronisable)
  ((remotes :accessor distributor-remotes :initarg :remotes :documentation "Specified.")
   (modules :accessor distributor-modules :documentation "Cache.")
   (wishmaster :accessor wishmasterp :initarg :wishmaster :documentation "Whether it participates in the DESIRE protocol.")
   (git :accessor gate :documentation "Manually managed."))
  (:default-initargs
   :registrator #'(setf distributor) :modules nil :remotes nil))

(defclass local-distributor (distributor)
  ((root   :accessor root        :initarg :root :documentation "Root of all desires.")
   (git    :accessor local-git   :documentation "Transitory git locality of a locally-accessible distributor.")
   (hg     :accessor local-hg    :documentation "Transitory hg locality of a locally-accessible distributor.")
   (darcs  :accessor local-darcs :documentation "Transitory darcs locality of a locally-accessible distributor.")
   (cvs    :accessor local-cvs   :documentation "Transitory cvs locality of a locally-accessible distributor.")
   (svn    :accessor local-svn   :documentation "Transitory svn locality of a locally-accessible distributor."))
  (:default-initargs
   :wishmaster t))

(defmacro do-wishmasters ((var) &body body)
  `(do-distributors (,var)
     (when (wishmasterp ,var)
       ,@body)))

(defmacro do-distributor-remotes ((var distributor &optional block-name) &body body)
  `(iter ,@(when block-name `(,block-name)) (for ,var in (distributor-remotes (coerce-to-distributor ,distributor)))
         ,@body))

(defmacro do-distributor-modules ((module-var distributor) &body body)
  "Execute BODY with MODULE-VAR iteratively bound to DISTRIBUTOR's modules."
  (with-gensyms (remote module-name block)
    `(do-distributor-remotes (,remote ,distributor ,block)
       (iter (for ,module-name in (location-module-names ,remote))
             (for ,module-var = (module ,module-name))
             (in ,block ,@body)))))

(defun compute-distributor-modules (distributor)
  "Compute the set of module names published by DISTRIBUTOR.
This notably excludes converted modules."
  (remove-duplicates (mapcan #'location-module-names (distributor-remotes (coerce-to-distributor distributor)))))

;;;;
;;;; VCS nomenclature
;;;;
(defclass vcs-type-mixin () ((vcs-type :reader vcs-type :initarg :vcs-type)))

(defvar *supported-vcs-types* '(git hg darcs cvs svn))
(defvar *gate-vcs-type* 'git)

;;; exhaustive partition of VCS-TYPE-MIXIN
(defclass git (vcs-type-mixin)   () (:default-initargs :vcs-type 'git))
(defclass hg (vcs-type-mixin)    () (:default-initargs :vcs-type 'hg))
(defclass darcs (vcs-type-mixin) () (:default-initargs :vcs-type 'darcs))
(defclass cvs (vcs-type-mixin)   () (:default-initargs :vcs-type 'cvs))
(defclass svn (vcs-type-mixin)   () (:default-initargs :vcs-type 'svn))

(defun vcs-enabled-p (type)
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

;;; exhaustive partition of type product of VCS-TYPE and TRANSPORT-MIXIN
(defclass git-native (git git-native-transport) ())
(defclass git-http (git http) ())
(defclass hg-http (hg http) ())
(defclass darcs-http (darcs http) ())
(defclass cvs-rsync (cvs rsync) ())
(defclass svn-rsync (svn rsync) ())

;;;;
;;;; Location
;;;;
(defclass location (registered synchronisable)
  ((module-names :accessor location-module-names :initarg :modules :documentation "Specified or maybe cached, for LOCALITYs."))
  (:default-initargs
   :registrator #'(setf locality)
   :modules nil))

(defclass gate (location)
  ((converted-module-names :accessor gate-converted-module-names :initarg :converted-module-names :documentation "Complex computation."))
  (:default-initargs
   :converted-module-names nil)
  (:documentation
   "A location belonging to a WISHMASTER (a distributor participating in
the DESIRE protocol) which holds (and possibly exports) converted modules."))

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
   (disabled-p :accessor remote-disabled-p :type boolean :initarg :disabled-p :documentation "Image-only property: not serialised."))
  (:default-initargs
   :registrator #'(setf remote)
   :disabled-p nil
   :distributor-port nil
   :domain-name-takeover nil))

(defclass gate-locality (gate locality remote) ())

;;;;
;;;; Location * VCS
;;;;
(defclass git-remote (git gate remote) ())

;;; most specific, exhaustive partition of REMOTE
(defclass git-native-remote (git-native git-remote) ())
(defclass git-http-remote (git-http git-remote) ())
(defclass git-combined-remote (git-native git-http git-remote) ())
(defclass hg-http-remote (hg-http remote) ())
(defclass darcs-http-remote (darcs-http remote) ())
(defclass cvs-rsync-remote (cvs-rsync remote) ())
(defclass svn-rsync-remote (svn-rsync remote) ())

(defun uri-type-to-remote-type (uri-type)
  (switch (uri-type :test #'string=)
    ("http" 'git-http-remote)
    ("git" 'git-native-remote)
    ("git+http" 'git-combined-remote)
    ("darcs" 'darcs-http-remote)
    ("cvs" 'cvs-rsync-remote)
    ("svn" 'svn-rsync-remote)))

;;; most specific, exhaustive partition of LOCALITY
(defclass git-locality (git-native-remote gate-locality) ())
(defclass hg-locality (hg locality) ())
(defclass darcs-locality (darcs locality) ())
(defclass cvs-locality (cvs locality) ())
(defclass svn-locality (svn locality) ())

(defmethod vcs-type ((o (eql 'git-native-remote))) 'git)
(defmethod vcs-type ((o (eql 'git-http-remote))) 'git)
(defmethod vcs-type ((o (eql 'git-combined-remote))) 'git)
(defmethod vcs-type ((o (eql 'hg-http-remote))) 'hg)
(defmethod vcs-type ((o (eql 'darcs-http-remote))) 'darcs)
(defmethod vcs-type ((o (eql 'cvs-rsync-remote))) 'cvs)
(defmethod vcs-type ((o (eql 'svn-rsync-remote))) 'svn)

;;;
;;; Locality methods
;;;
(defmethod initialize-instance :after ((o locality) &key path &allow-other-keys)
  (unless path
    (error "~@<A location without path specified is useless. ~S is one of many.~:@>" o))
  (setf (locality-by-path path) o)
  (unless (directory-exists-p path)
    (ensure-directories-exist path)))

(defmethod initialize-instance :after ((o cvs-locality) &key path &allow-other-keys)
  (unless (directory-exists-p path)
    (ensure-directories-exist path))
  (ensure-directories-exist (cvs-locality-lock-path o) :verbose t))

(defmethod shared-initialize :after ((o gate-locality) slot-names &key &allow-other-keys)
  (setf (gate-converted-module-names o) (mapcar #'name (compute-locality-module-presence o))))

(defun locality-asdf-registry-path (locality)
  "Provide the fixed definition of ASDF registry directory,
   within LOCALITY."
  (subdirectory* (locality-path locality) ".asdf-registry"))

(defun locality-register-with-asdf (locality)
  (pushnew (ensure-directories-exist (locality-asdf-registry-path locality)) asdf:*central-registry* :test #'equal))

(defun cvs-locality-lock-path (cvs-locality)
  "Provide the fixed definition of lock directory for CVS repositories,
   within CVS-LOCALITY."
  (subdirectory* (locality-path cvs-locality) ".cvs-locks"))

(defun module-path (module &optional (locality (gate *self*)))
  "Return MODULE's path in LOCALITY, which defaults to master Git locality."
  (subdirectory* (locality-path locality) (downstring (coerce-to-name module))))

;;;
;;; Local distributor methods
;;;
(defgeneric define-local-distributor-locality (local-distributor vcs-type)
  (:documentation "Define a locality of VCS-TYPE in a subdirectory of LOCAL-DISTIBUTOR's root.")
  (:method ((o local-distributor) vcs-type)
    (setf (slot-value o vcs-type)
          (make-instance (find-class (format-symbol #.*package* "~A-LOCALITY" vcs-type))
                         :name (default-remote-name (name o) vcs-type)
                         :path (subdirectory* (root o) (string-downcase (string vcs-type)))))))

(defmethod shared-initialize :after ((o local-distributor) slot-names &key &allow-other-keys)
  ;; The locality typed *gate-vcs-type* need to be produced differently between make-instance/change-class.
  (mapc (curry #'define-local-distributor-locality o) (remove *gate-vcs-type* *supported-vcs-types*)))

(defun wishmaster-release-modules (wishmaster)
  "Return the list of names of modules released by WISHMASTER via its gate.
This function is interesting because wishmasters only ever release via
their gate remote."
  (location-module-names (gate wishmaster)))

(defun distributor-converted-modules (distributor)
  "Return the list of names of converted modules provided by DISTRIBUTOR
via its gate. It's called DISTRIBUTOR-CONVERTED-MODULES because we want to
be able to use it on all distributors, without keeping track of whether
they participate in the desire wishmaster protocol or not."
  (gate-converted-module-names (gate distributor)))

(defmethod update-instance-for-different-class :after ((d distributor) (w local-distributor) &key root &allow-other-keys)
  "Called once, during INIT, if we're pretending to be someone well-known."
  (setf (gate w) (change-class (find-if (of-type *gate-vcs-type*) (distributor-remotes w))
                               'git-locality
                               :path (merge-pathnames #p"git/" root)))
  (let ((locally-present-set (distributor-converted-modules w)) ; was computed by GATE-LOCALITY's :after I-I method
        ;; The logic here is that if we're engaging in this whole game,
        ;; we're only releasing via our gate remote (git), which means that
        ;; WISHMASTER-RELEASE-MODULES returns the complete set of modules.
        (release-set (wishmaster-release-modules w))) 
    (when-let ((missing (set-difference release-set locally-present-set)))
      (error "~@<Modules ~S, which are required to establish identity with distributor ~A, are missing from ~A.~:@>" missing (name w) (root w)))
    (dolist (m-name release-set)
      (change-class (module m-name) 'origin-module))
    (nset-differencef (gate-converted-module-names (gate w)) release-set)))

(defmethod initialize-instance :after ((o local-distributor) &key root &allow-other-keys)
  (define-local-distributor-locality o *gate-vcs-type* :path root))

;;;
;;; Remote methods
;;;
(defmethod transport ((o git-combined-remote))
  (if *combined-remotes-prefer-native-over-http*
      'git 'http))

(defun default-remote-name (distributor-name vcs-type)
  "Compute a default name for remote with VCS-TYPE in DISTRIBUTOR-NAME."
  (if (eq vcs-type *gate-vcs-type*)
      distributor-name
      (format-symbol (symbol-package distributor-name) "~A~:[-~A~;~]" distributor-name (eq vcs-type *gate-vcs-type*) vcs-type)))

(defun choose-default-remote-name (distributor vcs-type)
  "Try choose a default name for a remote with VCS-TYPE on DISTRIBUTOR.
When there's a name clash NIL is returned."
  (let ((default-name (default-remote-name (name distributor) vcs-type)))
    (when-let ((non-conflicting-p (null (find default-name (distributor-remotes distributor) :key #'name))))
      default-name)))

(defun init-time-collate-remote-name (distributor vcs-type &optional specified-name)
  "Provide a mechanism for init-time name collation for REMOTE with 
   DISTRIBUTOR-NAME, and optionally SPECIFIED-NAME.

   Collation rules are considered in order, as follows:
      - SPECIFIED-NAME wins,
      - if REMOTE is the only GIT remote in DISTRIBUTOR-NAME, provide a default
        of DISTRIBUTOR-NAME,
      - if REMOTE is the only remote of its VCS type in DISTRIBUTOR-NAME, 
        provide a default of DISTRIBUTOR-NAME-VCS-TYPE."
  (let ((distributor-name (name distributor)))
    (cond (specified-name (if (null (find specified-name (distributor-remotes distributor) :key #'name))
                              specified-name
                              (error "~@<Specified remote name ~A conflicts in distributor ~A.~:@>" specified-name distributor-name)))
          (t (or (choose-default-remote-name distributor vcs-type)
                 (error "~@<Cannot choose an unambiguous name for a ~A remote in distributor ~A, provide one explicitly.~:@>"
                        vcs-type distributor-name))))))

(defmethod initialize-instance :before ((o remote) &key distributor vcs-type name &allow-other-keys)
  (setf (name o) (init-time-collate-remote-name distributor vcs-type name)))

(defmethod initialize-instance :after ((o remote) &key distributor path &allow-other-keys)
  (appendf (distributor-remotes distributor) (list o))
  (setf (remote-path-fn o) (compile nil `(lambda ()
                                           (declare (special *module* *umbrella*))
                                           (list ,@path)))))

;;;
;;; NOTE: this is the reason why remotes have names
;;;
(defun git-fetch-remote (remote module-name &optional locality-path)
  "Fetch from REMOTE, with working directory optionally changed
to LOCALITY-PATH."
  (maybe-within-directory locality-path
    (let ((module-url (url remote module-name)))
      (ensure-gitremote (name remote) module-url))
    (with-explanation ("fetching module ~A from remote ~A in ~S" module-name (name remote) *default-pathname-defaults*)
      (git "fetch" (down-case-name remote)))
    (ensure-master-branch-from-remote :remote-name (name remote))))

(defun git-clone-remote (remote module-name &optional locality-path)
  "Clone REMOTE, with working directory optionally changed to LOCALITY-PATH."
  (maybe-within-directory locality-path
    (let ((module-url (url remote module-name)))
      (with-explanation ("cloning module ~A from remote ~A in ~S" module-name (name remote) *default-pathname-defaults*)
        (git "clone" "-o" (down-case-name remote) module-url)))))

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

(defun make-distributor (type hostname port path &key gate-p)
  "Make a distributor residing at HOSTNAME, with a remote of TYPE,
accesible at PORT and PATH. 
When GATE-P is true, the remote will be set as distributor's gate
remote, in which case TYPE must be subtype of GIT."
  (lret ((d (make-instance 'distributor :name hostname)))
    (let ((r (make-instance type :distributor d :distributor-port port :path path)))
      (push r (distributor-remotes d))
      (when gate-p
        (unless (typep r *gate-vcs-type*)
          (error "~@<Requested to make ~S a gate remote of ~A, but it is not a remote of gate type, i.e. ~A.~:@>"
                 r hostname *gate-vcs-type*))
        (setf (gate d) r)))))

;;;;
;;;; Modules
;;;;
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

(defun module-present-p (module-or-name &optional (locality (gate *self*)) check-when-present-p (check-when-missing-p t)
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

;;;;
;;;; Systems
;;;;
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

(defmethod initialize-instance :after ((o system) &key module &allow-other-keys)
  (appendf (module-systems module) (list o)))

;;;;
;;;; Applications
;;;;
(defclass application (registered synchronisable)
  ((system :accessor app-system :initarg :system :documentation "Specified.")
   (package :accessor app-package :initarg :package :documentation "Specified.")
   (function :accessor app-function :initarg :function :documentation "Specified.")
   (default-parameters :accessor app-default-parameters :initarg :default-parameters :documentation "Specified."))
  (:default-initargs
   :registrator #'(setf app) :system nil :package nil :function nil :default-parameters nil))

(defmethod initialize-instance :after ((o application) &key system &allow-other-keys)
  (appendf (system-applications system) (list o)))

;;;;
;;;; Globals
;;;;
(define-root-container *distributors*   distributor   :name-transform-fn coerce-to-namestring :remover %remove-distributor :coercer t :mapper map-distributors :iterator do-distributors)
(define-root-container *modules*        module        :name-transform-fn coerce-to-namestring :remover %remove-module :coercer t :mapper map-modules :if-exists :error :iterator do-modules)
(define-root-container *leaves*         leaf          :name-transform-fn coerce-to-namestring :type module :mapper map-leaves :if-exists :continue)
(define-root-container *nonleaves*      nonleaf       :name-transform-fn coerce-to-namestring :type module :mapper map-nonleaves :if-exists :continue)
(define-root-container *systems*        system        :name-transform-fn coerce-to-namestring :remover %remove-system :coercer t :mapper map-systems)
(define-root-container *apps*           app           :name-transform-fn coerce-to-namestring :remover %remove-app :coercer t :mapper map-apps :type application)
(define-root-container *remotes*        remote        :name-transform-fn coerce-to-namestring :remover %remove-remote :coercer t :mapper map-remotes :type remote :if-exists :error :iterator do-remotes)
(define-root-container *localities*     locality      :type locality :mapper map-localities :if-exists :error)
(define-root-container *localities-by-path* locality-by-path :type locality :if-exists :error)

(defun remove-distributor (distributor-designator &aux (d (coerce-to-distributor distributor-designator)))
  (do-distributor-remotes (r d)
    (do-remove-remote r))
  (%remove-distributor (name d)))

(defun do-remove-remote (r)
  (dolist (m (location-module-names r))
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
        (removef (location-module-names remote) m))
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

(defun meta-path (&optional (local-distributor *self*))
  "Return the path to the meta directory."
  (subdirectory* (locality-path (gate local-distributor)) ".meta"))

(defun compute-locality-module-presence (&optional (locality (gate *self*)))
  "Scan LOCALITY for known modules and update its and those modules's idea
about their relationship.
The value returned is the list of found modules."
  (do-modules (module)
    (when (module-present-p module locality t)
      (collect module))))

(defmacro do-present-modules ((module &optional (locality '(gate *self*))) &body body)
  "Iterate the BODY over all modules cached as present in LOCALITY, with MODULE specifying
   the driven variable binding."
  `(do-modules (,module)
     (when (module-present-p ,module ,locality nil nil)
       ,@body)))

(defun clone-metastore (&optional (url *bootstrap-wishmaster-url*) (locality-path (meta-path)))
  "Clone metastore from URL, with working directory optionally changed to
LOCALITY-PATH."
  (maybe-within-directory locality-path
    (multiple-value-bind (type host port path) (parse-remote-namestring url)
      (declare (ignore type port path))
      (with-explanation ("cloning .meta ~A/.meta in ~S" url *default-pathname-defaults*)
        (git "clone" "-o" (string-downcase host) (concatenate 'string url "/.meta"))))))

(defmacro within-wishmaster-meta ((w &key (metastore '(meta-path)) update-p) &body body)
  (once-only (w metastore)
    `(within-directory (,metastore)
       ,@(when update-p `((git-fetch-remote (gate ,w) :.meta)))
       (within-ref (list "remotes" (down-case-name ,w) "master")
         ,@body))))

(defun merge-remote-wishmaster (wishmaster &optional (metastore (meta-path)))
  "Merge definitions from WISHMASTER."
  (within-wishmaster-meta (wishmaster :metastore metastore)
    (load-definitions metastore)))

(defun merge-remote-wishmasters (&optional (metastore (meta-path)))
  (do-wishmasters (w)
    (merge-remote-wishmaster w metastore)))

(defun determine-tools-and-update-remote-accessibility ()
  "Find out which and where VCS tools are available and disable correspondingly inaccessible remotes."
  (let ((present (unzip #'find-and-register-tools-for-remote-type '(hg darcs cvs svn))))
    (do-remotes (r)
      (setf (remote-disabled-p r) (not (member (vcs-type r) present))))))

(defun ensure-present-module-systems-loadable (&optional (locality (gate *self*)))
  "Ensure that all modules present in LOCALITY have their systems loadable.
   Return the list of present modules."
  (do-present-modules (module locality)
    (ensure-module-systems-loadable module locality)))

(defgeneric load-definitions (&optional metastore))
(defgeneric save-current-definitions (&key seal-p commit-message metastore))

(defun init (path &key as (merge-remote-wishmasters *merge-remote-wishmasters*))
  "Make Desire fully functional, with PATH chosen as storage location.

AS, when specified, will be interpreted as a distributor name, whose
definition will be looked up. Consequently, an attempt to establish
an identity relationship with that definition will be performed,
by looking up locally the modules defined for export. The rest of
locally present modules will be marked as converted."
  (let* ((path (fad:pathname-as-directory path))
         (absolute-path (if (pathname-absolute-p path)
                            path
                            (merge-pathnames path)))
         (root (parse-namestring absolute-path))
         (meta-path (merge-pathnames #p"git/.meta/" root)))
    (clear-definitions)
    (unless (find-and-register-tools-for-remote-type *gate-vcs-type*)
      (error "The executable of gate VCS (~A) is missing, and so, DESIRE is of no use." *gate-vcs-type*))
    (unless (metastore-present-p meta-path '(definitions))
      (report t ";;; No metastore found in ~S, bootstrapping from ~S~%" meta-path *bootstrap-wishmaster-url*)
      (clone-metastore *bootstrap-wishmaster-url* meta-path))
    (report t ";;; Loading definitions from ~S~%" (metafile-path 'definitions meta-path))
    (load-definitions meta-path)
    (when merge-remote-wishmasters
      (report t ";;; Merging definitions from remote wishmasters...~%")
      (merge-remote-wishmasters meta-path))
    (report t ";;; Determining available tools and deducing accessible remotes~%")
    (determine-tools-and-update-remote-accessibility)
    (setf *self* (if-let ((d (and as (distributor as))))
                   (progn (report t ";;; Trying to establish self as ~A~%" as)
                          (change-class d 'local-distributor :root root :wishmaster t))
                   (let ((local-name (format-symbol #.*package* "~(~A~)" (machine-instance))))
                     (report t ";;; Establishing self as non-well-known distributor ~A~%" local-name)
                     (make-instance 'local-distributor :name local-name :root root :omit-registration t))))
    (report t ";;; Ensuring that present modules have their defined systems accessible~%")
    ;; TODO: make this a method on NOTICE-MODULE-APPEARED
    (ensure-present-module-systems-loadable (gate *self*))
    (report t ";;; All done.~%")
    (values)))

(defun define-locality (name vcs-type &rest keys &key &allow-other-keys)
  "Define locality of VCS-TYPE at PATH, if one doesn't exist already, 
   in which case an error is signalled."
  (apply #'make-instance (format-symbol (symbol-package vcs-type) "~A-LOCALITY" vcs-type) :name name (remove-from-plist keys :name)))

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
  (not (null (find (name module) (location-module-names remote)))))

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
           (curry #'cons (gate *self*))
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
