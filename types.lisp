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
(defvar *distributors*       (make-hash-table :test #'equal) "Map distributor names to remotes.")
(defvar *remotes*            (make-hash-table :test #'equal) "Map remote names to remotes.")
(defvar *localities*         (make-hash-table :test #'equal) "Map names to localities.")
(defvar *localities-by-path* (make-hash-table :test #'equal) "Map paths to localities.")
(defvar *modules*            (make-hash-table :test #'equal) "Map module names to modules.")
(defvar *leaves*             (make-hash-table :test #'equal) "Map module names to leaf modules.")
(defvar *nonleaves*          (make-hash-table :test #'equal) "Map module names to nonleaf modules.")
(defvar *systems*            (make-hash-table :test #'equal) "Map system names to remotes.")
(defvar *apps*               (make-hash-table :test #'equal) "Map application names to remotes.")

(defvar *desire-root* nil "Absolute pathname of a directory containing master localities and installed tools.")

(defvar *unsaved-definition-changes-p* nil
  "Whether the idea about the world changed, since INIT was performed, or
SAVE-CURRENT-DEFINITIONS was called.")

(defun clear-definitions ()
  "Empty all global definitions."
  (dolist (var '(*distributors* *remotes* *localities* *localities-by-path* *modules* *leaves* *nonleaves* *systems* *apps*))
    (setf (symbol-value var) (make-hash-table :test #'equal)))
  (values))

(defvar *original-self-gate-class-name* nil)

;;;;
;;;; Distributor
;;;;
;;;
;;; Note: distributor name is its hostname.
;;;
(defclass distributor (registered synchronisable)
  ((remotes :accessor distributor-remotes :initarg :remotes :documentation "Specified.")
   (modules :accessor distributor-modules :documentation "Cache.")
   (git :accessor gate :documentation "Manually managed.")
   (relationships :accessor distributor-relationships :initarg :relationships))
  (:default-initargs
   :registrator #'(setf distributor) :modules nil :remotes nil
   :relationships nil))

(defclass local-distributor (distributor)
  ((root    :accessor root          :initarg :root :documentation "Root of all desires.")
   (git     :accessor local-git     :documentation "Transitory git locality of a locally-accessible distributor.")
   (hg      :accessor local-hg      :documentation "Transitory hg locality of a locally-accessible distributor.")
   (darcs   :accessor local-darcs   :documentation "Transitory darcs locality of a locally-accessible distributor.")
   (cvs     :accessor local-cvs     :documentation "Transitory cvs locality of a locally-accessible distributor.")
   (svn     :accessor local-svn     :documentation "Transitory svn locality of a locally-accessible distributor.")
   (tarball :accessor local-tarball :documentation "Transitory tarball locality of a locally-accessible distributor.")))

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

(defclass relationship ()
  ((from :accessor rel-from :initarg :from)
   (to :accessor rel-to :initarg :to)))

(defclass definition-subscription (relationship)
  ((branch :accessor rel-branch :initarg :branch)))

(defun rel (distributor to)
  (cdr (assoc to (distributor-relationships distributor) :test #'eq)))

(defun set-rel (distributor to value &aux (rships (distributor-relationships distributor)))
  (if-let ((cell (assoc to rships :test #'eq)))
    (setf (cdr cell) value)
    (setf (distributor-relationships distributor) (acons to value rships))))

(defsetf rel set-rel)

;;;;
;;;; VCS nomenclature
;;;;
(defclass vcs-type-mixin () ((vcs-type :reader vcs-type :initarg :vcs-type)))

(defvar *supported-vcs-types* '(git hg darcs cvs svn tarball))
(defvar *gate-vcs-type* 'git)

(defclass wrinkle-mixin ()
  ((wrinkles :accessor wrinkles :initarg :wrinkles))
  (:default-initargs :wrinkles nil))

;;; exhaustive partition of VCS-TYPE-MIXIN
(defclass git (vcs-type-mixin)               () (:default-initargs :vcs-type 'git))
(defclass hg (vcs-type-mixin)                () (:default-initargs :vcs-type 'hg))
(defclass darcs (vcs-type-mixin)             () (:default-initargs :vcs-type 'darcs))
(defclass cvs (vcs-type-mixin wrinkle-mixin) () (:default-initargs :vcs-type 'cvs))
(defclass svn (vcs-type-mixin wrinkle-mixin) () (:default-initargs :vcs-type 'svn))
(defclass tarball (vcs-type-mixin)           () (:default-initargs :vcs-type 'tarball))

(defun vcs-enabled-p (type)
  (class-slot type 'enabled-p))

(defgeneric remote-module-wrinkle (remote module-name)
  (:method ((r wrinkle-mixin) module-name)
    (cadr (assoc module-name (wrinkles r) :test #'string=))))

(defun find-and-register-tools-for-remote-type (type)
  "Find and make available executables for fetching from remotes of TYPE.
   Return T when all executables required by TYPE are available, or NIL."
  (setf (class-slot type 'enabled-p)
        (every #'find-executable (class-slot type 'required-executables))))

(defclass schema-mixin () ((schema :reader schema :initarg :schema)))
(defclass transport-mixin (schema-mixin) ((transport :reader transport :initarg :transport)))

;;; non-exhaustive partition of TRANSPORT-MIXIN
(defclass native (transport-mixin) () (:default-initargs :transport 'native))
(defclass http (transport-mixin) () (:default-initargs :transport 'http :schema 'http))
(defclass rsync (transport-mixin) () (:default-initargs :transport 'rsync :schema 'rsync))

;;; exhaustive partition of type product of VCS-TYPE and TRANSPORT-MIXIN
(defclass git-native (git native) () (:default-initargs :schema 'git))
(defclass git-http (git http) ())
(defclass hg-http (hg http) ())
(defclass darcs-http (darcs http) ())
(defclass cvs-rsync (cvs rsync) ())
(defclass cvs-native (cvs native) () (:default-initargs :schema '|:PSERVER|))
(defclass svn-rsync (svn rsync) ())
(defclass svn-http (svn http) ())
(defclass svn-native (svn native) () (:default-initargs :schema 'svn))
(defclass tarball-http (tarball http)
  ((initial-version :accessor initial-tarball-version :initarg :initial-version)))

;;;;
;;;; Location
;;;;
(defclass location (registered synchronisable)
  ((module-names :accessor location-module-names :initarg :module-names :documentation "Specified or maybe cached, for LOCALITYs."))
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
  ((pathname :accessor locality-pathname :initarg :pathname :documentation "Specified.")
   (scan-p :accessor locality-scan-p :initarg :scan-p :documentation "Specified."))
  (:default-initargs
   :scan-p nil))
(defclass remote (location registered)
  ((distributor :accessor remote-distributor :initarg :distributor :documentation "Specified.")
   (domain-name-takeover :accessor remote-domain-name-takeover :initarg :domain-name-takeover :documentation "Specified.")
   (distributor-port :accessor remote-distributor-port :type (or null (integer 0 65536)) :initarg :distributor-port :documentation "Specified, rarely.")
   (module-credentials :accessor remote-module-credentials :type list :initarg :credentials :documentation "Specified, rarely.")
   (path :accessor remote-path :initarg :path :documentation "Specified.")
   (path-fn :accessor remote-path-fn :initarg :path-fn :documentation "Generated from above.")
   (disabled-p :accessor remote-disabled-p :type boolean :initarg :disabled-p :documentation "Image-only property: not serialised."))
  (:default-initargs
   :registrator #'(setf remote)
   :disabled-p nil
   :distributor-port nil
   :domain-name-takeover nil
   :credentials nil))

(defstruct (credentials (:conc-name cred-) (:constructor make-cred (name &key username password)))
  (name (error "~@<Won't create an unnamed credential.~:@>") :type symbol)
  (username (error "~@<Won't create a credential without a username.~:@>") :type string)
  (password (error "~@<Won't create a credential without a password.~:@>") :type (or null string)))

(defvar *credentials* (alist-hash-table `((anonymous-anonymous . ,(make-cred 'anonymous-anonymous :username "anonymous" :password "anonymous"))
                                          (anonymous-empty     . ,(make-cred 'anonymous-empty :username "anonymous" :password nil)))
                                        :test #'equal)
  "Credentials, by name. Not intended to be secure.")

(defun credentials-match-p (credentials username password)
  "Determine whether supplied USERNAME and PASSWORD match CREDENTIALS."
  (and (equal username (cred-username credentials))
       (equal password (cred-password credentials))))

(defun module-credentials (remote module-name)
  (cadr (assoc module-name (remote-module-credentials remote) :test #'string=)))

(defun wishmasterp (distributor)
  "See whether DISTRIBUTOR participates in the DESIRE protocol,
that is, whether it has a gate remote -- a git remote containing
a special module called '.meta'."
  (find-if (of-type 'gate) (distributor-remotes distributor)))

(defclass gate-remote (gate remote) ())
(defclass gate-locality (locality gate-remote) ())

;;;;
;;;; Location * VCS
;;;;
(defclass git-remote (git remote) ())
(defclass darcs-remote (darcs remote) ())
(defclass hg-remote (hg remote) ())
(defclass cvs-remote (cvs remote) ())
(defclass svn-remote (svn remote) ())
(defclass tarball-remote (tarball remote) ())

;;; almost most specific (due to GATE mixin), exhaustive partition of REMOTE
(defclass git-native-remote (git-native git-remote) ())
(defclass git-http-remote (git-http git-remote) ())
(defclass git-combined-remote (git-native git-http git-remote) ())
(defclass hg-http-remote (hg-http hg-remote) ())
(defclass darcs-http-remote (darcs-http darcs-remote) ())
(defclass cvs-rsync-remote (cvs-rsync cvs-remote) ())
(defclass cvs-native-remote (cvs-native cvs-remote) ())
(defclass svn-rsync-remote (svn-rsync svn-remote) ())
(defclass svn-http-remote (svn-http svn-remote) ())
(defclass svn-native-remote (svn-native svn-remote) ())
(defclass tarball-http-remote (tarball-http tarball-remote) ())

;;; A special case location*vcs*role extension which is /going/ to be
;;; troublesome, as it violates simplicity.
(defclass gate-native-remote (gate-remote git-native-remote) ())
(defclass gate-http-remote (gate-remote git-http-remote) ())

;; Handle remote localisation, for printing purposes.
(defun remote-canonical-class-name (remote)
  (or (when (eq (remote-distributor remote) *self*)
        *original-self-gate-class-name*)
      (type-of remote)))

(defun remote-type-promote-to-gate (type)
  (ecase type
    (git-native-remote 'gate-native-remote)
    (git-http-remote 'gate-http-remote)))

(defun uri-type-to-remote-type (uri-type &key gate-p hint)
  (xform gate-p #'remote-type-promote-to-gate
         (switch (uri-type :test #'string=)
           ("git" 'git-native-remote)
           ("git+http" 'git-http-remote)
           ("git-and-http" 'git-combined-remote)
           ("darcs" 'darcs-http-remote)
           ("http" (ecase hint
                     (git 'git-http-remote)
                     (darcs 'darcs-http-remote)
                     (svn 'svn-http-remote)
                     ((nil) (error "~@<The 'http' uri type is ambiguous, and there was no hint given.~:@>"))))
           ("cvs+rsync" 'cvs-rsync-remote)
           ("cvs" 'cvs-native-remote)
           (":pserver" 'cvs-native-remote)
           ("svn" 'svn-native-remote)
           ("svn+http" 'svn-http-remote)
           ("svn+rsync" 'svn-rsync-remote))))

;; Used for validation of user input in add-module
(defun remote-types-compatible-p (x y)
  "See if X and Y denote compatible remote types, that is that they
differ in only slight detail -- gate property, for example."
  (and (eq (vcs-type x) (vcs-type y))
       (eq (transport x) (transport y))))

;;; most specific, exhaustive partition of LOCALITY
(defclass git-locality (gate-locality gate-native-remote) ())
(defclass hg-locality (hg locality) ())
(defclass darcs-locality (darcs locality) ())
(defclass cvs-locality (cvs locality) ())
(defclass svn-locality (svn locality) ())
(defclass tarball-locality (tarball locality) ())

(defmethod vcs-type ((o (eql 'gate-native-remote))) *gate-vcs-type*)
(defmethod vcs-type ((o (eql 'gate-http-remote))) *gate-vcs-type*)
(defmethod vcs-type ((o (eql 'git-native-remote))) 'git)
(defmethod vcs-type ((o (eql 'git-http-remote))) 'git)
(defmethod vcs-type ((o (eql 'git-combined-remote))) 'git)
(defmethod vcs-type ((o (eql 'hg-http-remote))) 'hg)
(defmethod vcs-type ((o (eql 'darcs-http-remote))) 'darcs)
(defmethod vcs-type ((o (eql 'cvs-native-remote))) 'cvs)
(defmethod vcs-type ((o (eql 'cvs-rsync-remote))) 'cvs)
(defmethod vcs-type ((o (eql 'svn-rsync-remote))) 'svn)
(defmethod vcs-type ((o (eql 'svn-http-remote))) 'svn)
(defmethod vcs-type ((o (eql 'svn-native-remote))) 'svn)
(defmethod vcs-type ((o (eql 'tarball-http-remote))) 'tarball)

(defmethod transport ((o (eql 'gate-native-remote))) 'native)
(defmethod transport ((o (eql 'gate-http-remote))) 'http)
(defmethod transport ((o (eql 'git-native-remote))) 'native)
(defmethod transport ((o (eql 'git-http-remote))) 'http)
(defmethod transport ((o (eql 'git-combined-remote))) (if *combined-remotes-prefer-native-over-http* 'native 'http))
(defmethod transport ((o (eql 'hg-http-remote))) 'native)
(defmethod transport ((o (eql 'darcs-http-remote))) 'http)
(defmethod transport ((o (eql 'cvs-native-remote))) 'native)
(defmethod transport ((o (eql 'cvs-rsync-remote))) 'rsync)
(defmethod transport ((o (eql 'svn-rsync-remote))) 'rsync)
(defmethod transport ((o (eql 'svn-http-remote))) 'http)
(defmethod transport ((o (eql 'svn-native-remote))) 'native)
(defmethod transport ((o (eql 'tarball-http-remote))) 'http)

;;;
;;; Locality methods
;;;
(defmethod initialize-instance :after ((o locality) &key pathname &allow-other-keys)
  (unless pathname
    (error "~@<A location without path specified is useless. ~S is one of many.~:@>" o))
  (setf (locality-by-path pathname) o)
  (unless (directory-exists-p pathname)
    (ensure-directories-exist pathname)))

(defmethod initialize-instance :after ((o cvs-locality) &key pathname &allow-other-keys)
  (unless (directory-exists-p pathname)
    (ensure-directories-exist pathname))
  (ensure-directories-exist (cvs-locality-lock-path o) :verbose t))

(defgeneric update-gate-conversions (locality)
  (:method ((o gate-locality))
    (setf (gate-converted-module-names o) (mapcar #'name (compute-locality-module-presence o)))))

(defmethod shared-initialize :after ((o gate-locality) slot-names &key &allow-other-keys)
  (update-gate-conversions o))

(defun locality-asdf-registry-path (locality)
  "Provide the fixed definition of ASDF registry directory,
   within LOCALITY."
  (subdirectory* (locality-pathname locality) ".asdf-registry"))

(defun locality-register-with-asdf (locality)
  (pushnew (ensure-directories-exist (locality-asdf-registry-path locality)) asdf:*central-registry* :test #'equal))

(defun cvs-locality-lock-path (cvs-locality)
  "Provide the fixed definition of lock directory for CVS repositories,
   within CVS-LOCALITY."
  (subdirectory* (locality-pathname cvs-locality) ".cvs-locks"))

(defun module-pathname (module &optional (locality (gate *self*)))
  "Return MODULE's path in LOCALITY, which defaults to master Git locality."
  (subdirectory* (locality-pathname locality) (downstring (coerce-to-name module))))

;;;
;;; Local distributor methods
;;;
(defgeneric define-local-distributor-locality (local-distributor vcs-type &rest arguments)
  (:documentation "Define a locality of VCS-TYPE in a subdirectory of LOCAL-DISTIBUTOR's root.")
  (:method ((o local-distributor) vcs-type &rest arguments)
    (setf (slot-value o vcs-type)
          (apply #'make-instance (find-class (format-symbol #.*package* "~A-LOCALITY" vcs-type))
                 :name (default-remote-name (name o) vcs-type 'native) :distributor o
                 :pathname (subdirectory* (root o) (string-downcase (string vcs-type)))
                 arguments))))

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

(defgeneric update-local-distributor-conversions (distributor &optional scan-gate)
  (:method ((o local-distributor) &optional scan-gate)
    (let ((gate (gate o)))
      (when scan-gate
        (update-gate-conversions gate))
      (let ((locally-present-set (gate-converted-module-names gate))
            ;; The logic here is that if we're engaging in this whole game,
            ;; we're only releasing via our gate remote, which means that
            ;; WISHMASTER-RELEASE-MODULES returns the complete set of modules.
            (release-set (wishmaster-release-modules o))) 
        (when-let ((missing (set-difference release-set locally-present-set)))
          (error "~@<Modules ~S, which are required to establish identity with distributor ~A, are missing from ~A.~:@>" missing (name o) (root o)))
        (dolist (m-name release-set)
          (let ((m (module m-name)))
            (unless (typep m 'origin-module)
              (change-class m 'origin-module))))
        (nset-differencef (gate-converted-module-names gate) release-set)))))

(defmethod update-instance-for-different-class :after ((d distributor) (w local-distributor) &key root &allow-other-keys)
  "Called once, during INIT, if we're pretending to be someone well-known."
  (let ((gate (find-if (of-type *gate-vcs-type*) (distributor-remotes w))))
    (setf *original-self-gate-class-name* (class-name (class-of gate))
          (gate w) (change-class gate 'git-locality :pathname (merge-pathnames #p"git/" root))))
  (update-local-distributor-conversions w))

(defmethod initialize-instance :after ((o local-distributor) &key &allow-other-keys)
  (define-local-distributor-locality o *gate-vcs-type* :registrator #'(setf locality)))

;;;
;;; Remote methods
;;;
(defmethod transport ((o git-combined-remote))
  (if *combined-remotes-prefer-native-over-http*
      'git 'http))

(defun default-remote-name (distributor-name vcs-type transport)
  "Compute a default name for remote with VCS-TYPE and TRANSPORT in
DISTRIBUTOR-NAME."
  (if (eq vcs-type *gate-vcs-type*)
      distributor-name
      (format-symbol (symbol-package distributor-name) "~A~:[-~A-~A~;~]" distributor-name (eq vcs-type *gate-vcs-type*) vcs-type transport)))

(defun choose-default-remote-name (distributor vcs-type transport)
  "Try choose a default name for a remote with VCS-TYPE and TRANSPORT
on DISTRIBUTOR.  When there's a name clash NIL is returned as the primary
value, and the computed default name is returned as the secondary value
instead."
  (let* ((default-name (default-remote-name (name distributor) vcs-type transport))
         (non-conflicting-p (null (find default-name (distributor-remotes distributor) :key #'name))))
    (values (when non-conflicting-p default-name)
            (unless non-conflicting-p default-name))))

(defun init-time-select-remote-name (distributor vcs-type transport &optional specified-name)
  "Provide a mechanism for init-time name selection for REMOTE with 
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
          (t (or (choose-default-remote-name distributor vcs-type transport)
                 (error "~@<Cannot choose an unambiguous name for a ~A remote in distributor ~A, provide one explicitly.~:@>"
                        vcs-type distributor-name))))))

(defmethod initialize-instance :before ((o remote) &key distributor vcs-type transport name &allow-other-keys)
  (setf (name o) (init-time-select-remote-name distributor vcs-type transport name)))

(defmethod initialize-instance :after ((o remote) &key distributor path &allow-other-keys)
  (appendf (distributor-remotes distributor) (list o))
  (setf (remote-path-fn o) (compile nil `(lambda ()
                                           (declare (special *module* *umbrella*))
                                           (list ,@path)))))

;;;
;;; NOTE: this is the reason why remotes have names
;;;
(defun git-fetch-remote (remote module-name &optional directory)
  "Fetch from REMOTE, with working directory optionally changed
to DIRECTORY."
  (maybe-within-directory directory
    (let ((module-url (url remote module-name)))
      (ensure-gitremote (name remote) module-url))
    (fetch-gitremote (name remote))))

(defun git-clone-remote (remote module-name &optional locality-pathname)
  "Clone REMOTE, with working directory optionally changed to
LOCALITY-PATHNAME."
  (maybe-within-directory locality-pathname
    (let ((module-url (url remote module-name)))
      (with-explanation ("cloning module ~A from remote ~A in ~S" module-name (name remote) *default-pathname-defaults*)
        (git "clone" "-o" (down-case-name remote) module-url)))))

(defun parse-remote-namestring (namestring &key gate-p slashless type-hint)
  "Given a remote NAMESTRING, deduce the remote's type, host, port and path,
and return them as multiple values.
Note that http is interpreted as git-http -type remote.
DARCS/CVS/SVN need darcs://, cvs:// and svn:// schemas, correspondingly."
  (multiple-value-bind (schema user password hostname port path directoryp) (parse-uri namestring :slashless-header slashless)
    (let ((cred (when user
                  (or (match-credentials user password)
                      (error "~@<Credentials were provided in an URI namestring ~S, but were not recognised.~:@>" namestring)))))
      (values (or (uri-type-to-remote-type schema :gate-p gate-p :hint type-hint)
                  (error "Bad URI type ~S in remote namestring ~S." schema namestring))
              cred hostname port path directoryp))))

;;;;
;;;; Modules
;;;;
(defclass module (registered synchronisable)
  ((umbrella :accessor module-umbrella :initarg :umbrella :documentation "Transitory?")
   (essential-p :accessor module-essential-p :initarg :essential-p :documentation "Specified.")
   (system-path-whitelist :accessor module-system-path-whitelist :initarg :path-whitelist :documentation "Specified.")
   (system-path-blacklist :accessor module-system-path-blacklist :initarg :path-blacklist :documentation "Specified.")
   (scan-positive-localities :accessor module-scan-positive-localities :initarg :remotes :documentation "Cache. Locality scans should fill this one.")
   (remotes :accessor module-remotes :initarg :remotes :documentation "Cache. COMPUTE-MODULE-CACHES")
   (localities :accessor module-localities :initarg :localities :documentation "Cache. COMPUTE-MODULE-CACHES")
   (systems :accessor module-systems :initarg :systems :documentation "Cache. COMPUTE-MODULE-CACHES"))
  (:default-initargs
   :registrator #'(setf module)
   :path-whitelist nil :path-blacklist nil
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

(defgeneric remote-link-module (remote module &key &allow-other-keys)
  (:documentation
   "Establish a 'provides' relationship between REMOTE and MODULE.")
  (:method ((r remote) (m module) &key &allow-other-keys)
    (pushnew (name m) (location-module-names r))
    (pushnew r (module-remotes m)))
  (:method :around ((r wrinkle-mixin) (m module) &key wrinkle)
    (when (and wrinkle
               (not (string= wrinkle (down-case-name m))))
      (push (cons (name m) wrinkle) (wrinkles r)))
    (call-next-method)))

(defgeneric remote-unlink-module (remote module)
  (:documentation
   "Undo a 'provides' relationship between REMOTE and MODULE.")
  (:method ((r remote) (m module))
    (removef (location-module-names r) (name m))
    (removef (module-remotes m) r))
  (:method ((r cvs-remote) (m module))
    (removef (wrinkles r) (name m) :key #'car)
    (call-next-method)))

(defun remote-defines-module-p (remote module)
  "See whether MODULE is defined for REMOTE."
  (or (not (null (find (name module) (location-module-names remote))))
      (when (typep remote 'gate)
        (not (null (find (name module) (gate-converted-module-names remote)))))))

(defun module-locally-present-p (module-or-name &optional (locality (gate *self*)) check-when-present-p (check-when-missing-p t)
                         &aux (module (coerce-to-module module-or-name)))
  "See if MODULE's presence cache is positive for LOCALITY, failing that check the
   repository, and update the cache, accordingly to the result.

   CHECK-WHEN-PRESENT-P determines if presence check is performed when MODULE's cache
   is positive.
   CHECK-WHEN-MISSING-P defermines if presence check is performed upon negative
   cache results."
  (with-slots (scan-positive-localities) module
    (labels ((update-presence-in (locality)
               (lret ((presence (git-repository-present-p (module-pathname module locality))))
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

(defvar *module*)
(defvar *umbrella*)

(defun compile-remote-module-path-strings (remote module module-name)
  (let ((*module* (when module-name (downstring module-name)))
        (*umbrella* (when module (downstring (module-umbrella module)))))
    (declare (special *module* *umbrella*))
    (iter (for insn-spec in (funcall (remote-path-fn remote)))
          (cond ((eq insn-spec :no/)
                 (pop accumulated-path))
                (t
                 (when insn-spec
                   (collect insn-spec into accumulated-path at beginning)
                   (collect "/" into accumulated-path at beginning))))
          (finally (return (nreverse accumulated-path))))))

(defgeneric url-using-module (remote module)
  (:method :around ((r remote) module)
           ;; This module/name weirdness is because we try to support :<module-name>,
           ;; without supplying an actual module.
           (let ((path (compile-remote-module-path-strings r
                                                           (when (typep module 'module) module)
                                                           (coerce-to-name module))))
             (apply #'concatenate 'simple-base-string
                    (downstring (schema r))
                    (call-next-method)
                    (unless (remote-domain-name-takeover r)
                      (down-case-name (remote-distributor r)))
                    (unless (remote-domain-name-takeover r)
                      (when-let ((port (remote-distributor-port r)))
                        (format nil ":~D" port)))
                    (unless (remote-domain-name-takeover r)
                      "/")
                    path)))
  (:method :around ((r wrinkle-mixin) module)
           (values (call-next-method)
                   (remote-module-wrinkle r (when module (coerce-to-name module)))))
  (:method (r m) "://")
  (:method ((r cvs-native-remote) m)
    (let ((c (module-credentials r (coerce-to-name m))))
      (if c
          (let ((cred (cred c)))
            (format nil ":~A~:[~;~:*:~A~]@" (cred-username cred) (cred-password cred)))
          ":anonymous@"))))

(defun url (remote-or-name &optional module-or-name)
  (declare (type (or remote symbol) remote-or-name) (type (or module symbol) module-or-name))
  (url-using-module (coerce-to-remote remote-or-name)
                    (or (when (symbolp module-or-name)
                          (module module-or-name :if-does-not-exist :continue))
                        module-or-name)))

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
   (definition-pathname-name :accessor system-definition-pathname-name :initarg :definition-pathname-name
                             :documentation "Specified, see documentation for SYSTEM-HIDDEN-P.")
   (applications :accessor system-applications :initarg :applications :documentation "Cache."))
  (:default-initargs
   :registrator #'(setf system)
   :hidden-p nil :search-restriction nil :definition-pathname-name nil
   :module nil :applications nil :relativity nil))

(defun system-hidden-p (system)
  "A hidden system is a system whose definition resides in a file
named differently from system's name. We have to store the name of
the definition file for such systems.
Find out whether SYSTEM is hidden."
  (not (null (system-definition-pathname-name system))))

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
(define-root-container *credentials*    cred          :type credentials :iterator do-credentials :if-exists :error)
(define-root-container *localities-by-path* locality-by-path :type locality :if-exists :error)

(defun match-credentials (username password)
  "Find a named credentials entry with matching USERNAME and PASSWORD."
  (do-credentials (c)
    (when (credentials-match-p c username password)
      (return c))))

(defun canonicalise-module-name (name)
  "Given a module's NAME, whether in form of a string, keyword or a symbol
in any other package, return the canonical module name, as a symbol 
in the 'DESIRE' package.."
  (name (module name)))

(defun remove-distributor (distributor-designator &aux (d (coerce-to-distributor distributor-designator)))
  (do-distributor-remotes (r d)
    (do-remove-remote r))
  (%remove-distributor (name d)))

(defun do-remove-remote (r &key keep-modules)
  (dolist (m (location-module-names r))
    (when-let ((m (module m :if-does-not-exist :continue)))
      (if keep-modules
          (remote-unlink-module r m)
          (do-remove-module (module m)))))
  (%remove-remote (name r)))

(defun remove-remote (remote-designator &key keep-modules &aux (r (coerce-to-remote remote-designator)))
  (removef (distributor-remotes (remote-distributor r)) r)
  (do-remove-remote r :keep-modules keep-modules))

(defun do-remove-module (m)
  (dolist (s (module-systems m))
    (do-remove-system s))
  (%remove-module (name m)))

(defun remove-module (module-designator &key keep-localities &aux (m (coerce-to-module module-designator)))
  (unless keep-localities
    (do-remotes (r)
      (when (remote-defines-module-p r m)
        (removef (location-module-names r) (name m)))))
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

(defun choose-gate-or-else (remotes)
  (or (find-if (of-type 'gate) remotes)
      (first remotes)))

(defun module-best-remote (module &key (if-does-not-exist :error) allow-self)
  "Return the preferred remote among those providing MODULE.
Currently implements a static 'gates are preferred' policy."
  (let ((module (coerce-to-module module)))
    (or (choose-gate-or-else (do-remotes (r)
                               (when (and (remote-defines-module-p r module)
                                          (or allow-self
                                              (not (eq (remote-distributor r) *self*))))
                                 (collect r))))
        (ecase if-does-not-exist
          (:error (error 'insatiable-desire :desire module))
          (:continue nil)))))

(defun module-best-distributor (module &key (if-does-not-exist :error) allow-self)
  "Find the best-suited distributor occuring to provide MODULE.
Those distributors with a gate best-remote are preferred, obviously."
  (when-let ((r (module-best-remote module :if-does-not-exist if-does-not-exist :allow-self allow-self)))
    (remote-distributor r)))

(defun module-fetch-url (module &key allow-self)
  "Return the URL which is to be used while fetching MODULE,
that is the location of MODULE in the preferred remote.
When ALLOW-SELF is specified, and non-NIL, remotes within *SELF* are
not discarded from consideration."
  (let ((module (coerce-to-module module)))
    (url (module-best-remote module :allow-self allow-self) module)))

(defun touch-module (module &aux (module (coerce-to-module module)))
  "Try 'access' MODULE via its preferred remote and return
whether the attempt was successful."
  (touch-remote-module (module-best-remote module) module))

(defun distributor-module-enabled-remote (distributor module &aux (module (coerce-to-module module)) (distributor (coerce-to-distributor distributor)))
  "Return the first non-disabled DISTRIBUTOR's remote providing MODULE.
   The second value is a boolean, indicating non-emptiness of the set of
   providing remotes, regardless of the enabled-p flag."
  (choose-gate-or-else
   (do-distributor-remotes (r distributor)
     (when (and (remote-defines-module-p r module) (not (remote-disabled-p r)))
       (collect r)))))

(defun meta-path (&optional (local-distributor *self*))
  "Return the path to the meta directory."
  (subdirectory* (locality-pathname (gate local-distributor)) ".meta"))

(defun compute-locality-module-presence (&optional (locality (gate *self*)))
  "Scan LOCALITY for known modules and update its and those modules's idea
about their relationship.
The value returned is the list of found modules."
  (do-modules (module)
    (when (module-locally-present-p module locality t)
      (collect module))))

(defmacro do-present-modules ((module &optional (locality '(gate *self*))) &body body)
  "Iterate the BODY over all modules cached as present in LOCALITY, with MODULE specifying
   the driven variable binding."
  `(do-modules (,module)
     (when (module-locally-present-p ,module ,locality nil nil)
       ,@body)))

(defun clone-metastore (url locality-pathname branch)
  "Clone metastore from URL, with working directory optionally changed to
LOCALITY-PATHNAME. BRANCH is then checked out."
  (within-directory (locality-pathname)
    (multiple-value-bind (type cred host port path) (parse-remote-namestring url)
      (declare (ignore type cred port path))
      (let ((remote-name (string-downcase host))
            (meta-dir (subdirectory* locality-pathname ".meta")))
        (with-explanation ("cloning .meta ~A/.meta in ~S" url *default-pathname-defaults*)
          (git "clone" "-o" remote-name (concatenate 'string url "/.meta")))
        (within-directory (meta-dir)
          (unless (gitbranch-present-p :master)
            (add-gitbranch :master '("HEAD")))
          (git-checkout-ref '("master"))
          (git-repository-reset-hard `("remotes" ,remote-name ,(downstring branch))))))))

(defun reestablish-metastore-subscriptions (metastore-pathname)
  (within-directory (metastore-pathname)
    (iter (for (nil remote-name branch-name) in (remove-if-not #'ref-remotep (refs-by-value (ref-value '("master") metastore-pathname)
                                                                                            metastore-pathname)))
          (for d = (distributor (string-upcase remote-name) :if-does-not-exist :continue))
          (unless d (continue))
          (for rel = (make-instance 'definition-subscription :from *self* :to d :branch branch-name))
          (set-rel d *self* rel)
          (set-rel *self* d rel))))

(defmacro within-wishmaster-meta ((wishmaster branch &key (metastore '(meta-path)) update-p) &body body)
  (once-only (wishmaster metastore)
    `(within-directory (,metastore)
       ,@(when update-p `((git-fetch-remote (gate ,wishmaster) :.meta)))
       (with-git-ref (list "remotes" (down-case-name ,wishmaster) ,branch)
         ,@body))))

(defun merge-remote-wishmaster (wishmaster &optional (metastore (meta-path)))
  "Merge definitions from WISHMASTER."
  (let ((branch (if-let ((r (rel *self* wishmaster)))
                  (rel-branch r)
                  "master")))
    (within-wishmaster-meta (wishmaster branch :metastore metastore :update-p t)
      (load-definitions :source wishmaster :force-source nil :metastore metastore))))

(defun merge-remote-wishmasters (&optional (metastore (meta-path)))
  (do-wishmasters (w)
    (unless (eq w *self*)
      (merge-remote-wishmaster w metastore))))

(defun determine-tools-and-update-remote-accessibility ()
  "Find out which and where VCS tools are available and disable correspondingly inaccessible remotes."
  (let ((present (cons *gate-vcs-type* (unzip #'find-and-register-tools-for-remote-type (set-difference *supported-vcs-types* (list *gate-vcs-type*))))))
    (do-remotes (r)
      (setf (remote-disabled-p r) (not (member (vcs-type r) present))))
    (find-executable 'make)
    (find-executable 'cp)))

(defun ensure-present-module-systems-loadable (&optional (locality (gate *self*)))
  "Ensure that all modules present in LOCALITY have their systems loadable.
   Return the list of present modules."
  (do-present-modules (module locality)
    (ensure-module-systems-loadable module locality)))

(defgeneric load-definitions (&key source force-source metastore))
(defgeneric save-current-definitions (&key seal commit-message metastore))

(defun ensure-root-sanity (directory)
  (unless (directory-exists-p directory)
    (error "~@<The specified root at ~S does not exist.~:@>" directory))
  (let ((gitroot (subdirectory* directory "git")))
    (when (and (probe-file gitroot) (not (directory-exists-p gitroot)))
      (error "~@<The specified root at ~S contains a file named 'git', which violates the requirement for a sane root.~:@>" directory))
    (ensure-directories-exist gitroot)
    (ensure-directories-exist (subdirectory* directory "tmp")))
  directory)

(defun init (path &key as (merge-remote-wishmasters *merge-remote-wishmasters*) (wishmaster-branch :master))
  "Make Desire fully functional, with PATH chosen as storage location.

AS, when specified, will be interpreted as a distributor name, whose
definition will be looked up. Consequently, an attempt to establish
an identity relationship with that definition will be performed,
by looking up locally the modules defined for export. The rest of
locally present modules will be marked as converted."
  (let* ((path (fad:pathname-as-directory path))
         (absolute-path (if (pathname-absolute-p path)
                            path
                            (merge-pathnames path))))
    (setf *desire-root* (ensure-root-sanity (parse-namestring absolute-path)))
    (let* ((gate-path (merge-pathnames (make-pathname :directory (list :relative (downstring *gate-vcs-type*))) *desire-root*))
           (meta-path (merge-pathnames #p".meta/" gate-path)))
      (clear-definitions)
      (with-class-slot (git hg darcs cvs svn tarball) required-executables
        (setf git '(git) hg '(hg)  darcs '(darcs darcs-to-git wget) cvs '(rsync git cvs) svn '(rsync git) tarball '(git)))
      (with-class-slot (git hg darcs cvs svn tarball) enabled-p
        (setf git nil hg nil darcs nil cvs nil svn nil tarball nil))
      (unless (find-and-register-tools-for-remote-type *gate-vcs-type*)
        (error "The executable of gate VCS (~A) is missing, and so, DESIRE is of no use." *gate-vcs-type*))
      (unless (metastore-present-p meta-path '(definitions))
        (syncformat t ";;; no metastore found in ~S, bootstrapping from ~S~%" meta-path *bootstrap-wishmaster-url*)
        (clone-metastore *bootstrap-wishmaster-url* gate-path wishmaster-branch))
      (syncformat t ";;; loading definitions from ~S~%" (metafile-path 'definitions meta-path))
      (load-definitions :force-source t :metastore meta-path)
      (setf *self* (if-let ((d (and as (distributor as))))
                     (progn (syncformat t ";;; trying to establish self as ~A~%" as)
                            (change-class d 'local-distributor :root *desire-root*))
                     (let ((local-name (intern (string-upcase (machine-instance)) #.*package*)))
                       (syncformat t ";;; establishing self as non-well-known distributor ~A~%" local-name)
                       (make-instance 'local-distributor :name local-name :root *desire-root* :omit-registration t))))
      (unless (gitvar 'user.name)
        (let ((username (format nil "Desire operator on ~A" (down-case-name *self*))))
          (syncformat t ";;; setting git user name to ~S~%" username)
          (setf (gitvar 'user.name nil t) username)))
      (reestablish-metastore-subscriptions meta-path)
      (when merge-remote-wishmasters
        (syncformat t ";;; merging definitions from remote wishmasters...~%")
        (merge-remote-wishmasters meta-path))
      (setf *unsaved-definition-changes-p* nil)
      (syncformat t ";;; determining available tools and deducing accessible remotes~%")
      (determine-tools-and-update-remote-accessibility)
      (syncformat t ";;; registering ~S with ASDF~%" (locality-asdf-registry-path (gate *self*)))
      (locality-register-with-asdf (gate *self*))
      (syncformat t ";;; ensuring that present modules have their defined systems accessible~%")
      ;; TODO: make this a method on NOTICE-MODULE-APPEARED
      (ensure-present-module-systems-loadable (gate *self*))
      (syncformat t ";;; tweaking environment for CL-LAUNCH~%")
      (sb-posix:putenv "LISP_FASL_CACHE=NIL")
      (syncformat t ";;; all done~%")
      (values))))

(defun reinit ()
  "Execute INIT with the arguments that were passed to it last time."
  (init *desire-root* :as (when (distributor (name *self*) :if-does-not-exist :continue)
                            (name *self*))))

(defun define-locality (name vcs-type &rest keys &key &allow-other-keys)
  "Define locality of VCS-TYPE at PATH, if one doesn't exist already, 
   in which case an error is signalled."
  (apply #'make-instance (format-symbol (symbol-package vcs-type) "~A-LOCALITY" vcs-type) :name name (remove-from-plist keys :name)))

;;;
;;; Conditions.
;;;
(define-condition desire-condition (condition) ())
(define-condition desire-error (desire-condition error) ())
(define-condition remote-error (desire-error)
  ((remote :accessor condition-remote :initarg :remote)))
(define-condition repository-error (desire-error)
  ((locality :accessor condition-locality :initarg :locality)
   (module :accessor condition-module :initarg :module)))

(define-reported-condition insatiable-desire (desire-error)
  ((desire :accessor condition-desire :initarg :desire))
  (:report (desire)
           "~@<It is not known to me how to satisfy the desire for ~S.~:@>" desire))

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
    (serialise-definitions f))
  (when bail-out-early
    (return-from test-core))
  (clear-definitions)
  (read-definitions path-int-0)
  (with-output-to-file (f path-int-1)
    (serialise-definitions f))
  (clear-definitions)
  (read-definitions path-int-1)
  (with-output-to-file (f path-int-2)
    (serialise-definitions f)))
