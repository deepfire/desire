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
(defvar *default-world-readable*                   t   "Whether to make GIT repositories anonymously accessible by default.")
(defvar *default-publishable*                      t   "Whether to publish GIT repositories by default.")
(defvar *self*                                     nil "Possibly unknown distributor whom we identify as.")
(defvar *combined-remotes-prefer-native-over-http* t   "Whether multi-protocol Git remotes prefer native git protocol to HTTP.")
(defvar *default-system-type*                      'asdf-system)
(defvar *merge-remote-wishmasters*                 t   "Whether to merge definitions from remote wishmasters.")
(defvar *verbose-repository-maintenance*           nil)

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

(defparameter *implementation-provided-system-names*
  #+sbcl '("ASDF-INSTALL" "SB-ACLREPL" "SB-BSD-SOCKETS" "SB-CLTL2" "SB-COVER" "SB-EXECUTABLE" "SB-GROVEL"
           "SB-INTROSPECT" "SB-MD5" "SB-POSIX" "SB-QUEUE" "SB-ROTATE-BYTE" "SB-RT" "SB-SIMPLE-STREAMS" "SB-SPROF")
  #-sbcl nil)

(defvar *unsaved-definition-changes-p* nil
  "Whether the idea about the world changed, since INIT was performed, or
SAVE-DEFINITIONS was called.")

(defun clear-definitions ()
  "Empty all global definitions."
  (dolist (var '(*distributors* *remotes* *localities* *localities-by-path* *modules* *leaves* *nonleaves* *systems* *apps*))
    (setf (symbol-value var) (make-hash-table :test #'equal)))
  (values))

;;;;
;;;; Base
;;;;
(defclass desirable (registered synchronisable)
  ()
  (:documentation
   "Base class for all desire objects."))

;;;;
;;;; Distributor
;;;;
;;;
;;; Note: distributor name is its hostname.
;;;
(defclass distributor (desirable)
  ((remotes :accessor distributor-remotes :initarg :remotes :documentation "Specified.")
   (modules :accessor distributor-modules :documentation "Cache.")
   (git :accessor gate :documentation "Manually managed.")
   (relationships :accessor distributor-relationships :initarg :relationships))
  (:default-initargs
   :registrator #'(setf distributor) :modules nil :remotes nil
   :relationships nil))

(defclass local-distributor (distributor)
  ((root        :reader   root          :initarg :root :documentation "Root of all desires.")
   (meta        :reader   meta          :initarg :meta :documentation "Metastore path.")
   (localmeta   :reader   localmeta     :initarg :localmeta :documentation "Local metastore path.")
   (git         :accessor local-git     :documentation "Transitory git locality of a locally-accessible distributor.")
   (hg          :accessor local-hg      :documentation "Transitory hg locality of a locally-accessible distributor.")
   (darcs       :accessor local-darcs   :documentation "Transitory darcs locality of a locally-accessible distributor.")
   (cvs         :accessor local-cvs     :documentation "Transitory cvs locality of a locally-accessible distributor.")
   (svn         :accessor local-svn     :documentation "Transitory svn locality of a locally-accessible distributor.")
   (tarball     :accessor local-tarball :documentation "Transitory tarball locality of a locally-accessible distributor.")))

(defgeneric locality (specifier &optional local-distributor)
  (:documentation
   "Return the master locality of type denoted by SPECIFIER withing LOCAL-DISTRIBUTOR.")
  (:method ((o symbol)  &optional (local-distributor *self*))
    (slot-value local-distributor o)))

(defclass relationship ()
  ((from :accessor rel-from :initarg :from)
   (to :accessor rel-to :initarg :to)))

(defclass definition-subscription (relationship)
  ((branch :accessor rel-branch :initarg :branch)))

(defun rel (distributor to)
  (cdr (assoc to (distributor-relationships distributor) :test #'eq)))

(defun set-rel (distributor to value &aux
                (rships (distributor-relationships distributor)))
  (if-let ((cell (assoc to rships :test #'eq)))
    (setf (cdr cell) value)
    (setf (distributor-relationships distributor) (acons to value rships))))

(defsetf rel set-rel)

;;;;
;;;; VCS nomenclature
;;;;
(defclass vcs-type-mixin () ((vcs-type :reader vcs-type :initarg :vcs-type)))
(defmethod vcs-type ((s symbol)) nil)

(defvar *supported-vcs-types* '(git hg darcs cvs svn tarball))
(defvar *gate-vcs-type* 'git)

(defclass wrinkle-mixin ()
  ((wrinkles :accessor wrinkles :initarg :wrinkles))
  (:default-initargs :wrinkles nil))

;;; exhaustive partition of VCS-TYPE-MIXIN
(defclass git (vcs-type-mixin)                            () (:default-initargs :vcs-type 'git))
(defclass nongit-mixin ()                                 ())
(defclass hg (vcs-type-mixin nongit-mixin)                () (:default-initargs :vcs-type 'hg))
(defclass darcs (vcs-type-mixin nongit-mixin)             () (:default-initargs :vcs-type 'darcs))
(defclass cvs (vcs-type-mixin wrinkle-mixin nongit-mixin) () (:default-initargs :vcs-type 'cvs))
(defclass svn (vcs-type-mixin wrinkle-mixin nongit-mixin) () (:default-initargs :vcs-type 'svn))
(defclass tarball (vcs-type-mixin nongit-mixin)           () (:default-initargs :vcs-type 'tarball))

(defun vcs-enabled-p (type)
  (class-slot type 'enabled-p))

(defgeneric remote-module-wrinkle (remote module-name)
  (:method ((r wrinkle-mixin) module-name)
    (cadr (assoc module-name (wrinkles r) :test #'string=))))

(defgeneric set-remote-module-wrinkle (remote module-name wrinkle)
  (:method ((r wrinkle-mixin) module-name wrinkle)
    (if wrinkle
        (if-let ((cell (assoc module-name (wrinkles r) :test #'string=)))
          (setf (cadr cell) wrinkle)
          (push (list (canonicalise-name module-name) wrinkle) (wrinkles r)))
        (removef (wrinkles r) module-name :key #'car :test #'string=))))

(defun module-wrinkle (module &aux
                       (module (coerce-to-module module)))
  "Get the 'quirk' or 'wrinkle' string associated with MODULE within
its 'best remote'."
  (remote-module-wrinkle (module-best-remote module) (name module)))

(defun set-module-wrinkle (module wrinkle &aux
                           (module (coerce-to-module module)))
  "Set the 'quirk' or 'wrinkle' string associated with MODULE within
its 'best remote'."
  (declare (type (or null string) wrinkle))
  (set-remote-module-wrinkle (module-best-remote module) (name module) wrinkle))

(defsetf module-wrinkle set-module-wrinkle)

(defun find-and-register-tools-for-remote-type (type)
  "Find and make available executables for fetching from remotes of TYPE.
   Return T when all executables required by TYPE are available, or NIL."
  (setf (class-slot type 'enabled-p)
        (every #'find-executable (class-slot type 'required-executables))))

(defclass schema-mixin () ((schema :reader schema :initarg :schema)))
(defclass transport-mixin (schema-mixin) ((transport :reader transport :initarg :transport)))
(defclass clone-separation-mixin () ())
(defclass fetch-indirection-mixin () ())

;;; exhaustive partition of TRANSPORT-MIXIN
(defclass native (transport-mixin) () (:default-initargs :transport 'native))
(defclass http (transport-mixin) () (:default-initargs :transport 'http :schema 'http))
(defclass rsync (transport-mixin) () (:default-initargs :transport 'rsync :schema 'rsync))

;;; exhaustive partition of FETCH-INDIRECTION-MIXIN
(defclass separate-clone (clone-separation-mixin) ())
(defclass clone-is-fetch (clone-separation-mixin) ())

;;; exhaustive partition of FETCH-INDIRECTION-MIXIN
(defclass direct-fetch (fetch-indirection-mixin) ())
(defclass indirect-fetch (fetch-indirection-mixin) ())

;;; exhaustive partition of type product of VCS-TYPE, TRANSPORT-MIXIN, CLONE-SEPARATION-MIXIN, and
;;;   FETCH-INDIRECTION-MIXIN
(defclass git-native   (git        native direct-fetch   clone-is-fetch) () (:default-initargs :schema 'git))
(defclass git-http     (git        http   direct-fetch   clone-is-fetch) ())
(defclass hg-http      (hg         http   indirect-fetch separate-clone) ())
(defclass darcs-http   (darcs      http   indirect-fetch separate-clone) ())
(defclass cvs-rsync    (cvs        rsync  indirect-fetch clone-is-fetch) ())
(defclass cvs-native   (cvs        native direct-fetch   clone-is-fetch) () (:default-initargs :schema '|:PSERVER|))
(defclass tarball-http (tarball    http   direct-fetch   clone-is-fetch) ((initial-version :accessor initial-tarball-version :initarg :initial-version)))
(defclass svn-rsync    (svn        rsync  indirect-fetch clone-is-fetch) ())
;;; ...... 8< ......
(defclass svn-direct   (svn               direct-fetch) ())
;;; ...... >8 ......
(defclass svn-http     (svn-direct http   #| direct |#   clone-is-fetch) ())
(defclass svn-native   (svn-direct native #| direct |#   clone-is-fetch) () (:default-initargs :schema 'svn))

;;;;
;;;; Location
;;;;
(defclass location (desirable)
  ((module-names :accessor location-module-names :initarg :module-names :documentation "Specified or maybe cached, for LOCALITYs."))
  (:default-initargs
   :module-names nil))

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
   :registrator #'(setf loc)
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
  (name (definition-error "~@<Won't create an unnamed credential.~:@>") :type symbol)
  (username (definition-error "~@<Won't create a credential without a username.~:@>") :type string)
  (password (definition-error "~@<Won't create a credential without a password.~:@>") :type (or null string)))

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

(defmethod locality ((o remote) &optional (local-distributor *self*))
  (slot-value local-distributor (vcs-type o)))

(defun wishmasterp (distributor)
  "See whether DISTRIBUTOR participates in the DESIRE protocol,
that is, whether it has a gate remote -- a git remote containing
a special module called '.meta'."
  (find-if (of-type 'gate) (distributor-remotes distributor)))

(defclass gate-remote (gate remote) ())
(defclass gate-locality (gate-remote locality) 
  ((unpublished-module-names :accessor gate-unpublished-module-names :initarg :unpublished-module-names :documentation "Complex computation.")
   (hidden-module-names :accessor gate-hidden-module-names :initarg :hidden-module-names :documentation "Complex computation.")))

;;;;
;;;; Location * VCS
;;;;
(defclass git-remote (remote git) ())
(defclass darcs-remote (remote darcs) ())
(defclass hg-remote (remote hg) ())
(defclass cvs-remote (remote cvs) ())
(defclass svn-remote (remote svn) ())
(defclass tarball-remote (remote tarball) ())

;;; almost most specific (due to GATE mixin), exhaustive partition of REMOTE
(defclass git-native-remote (git-remote git-native) ())
(defclass git-http-remote (git-remote git-http) ())
(defclass git-combined-remote (git-remote git-native git-http) ())
(defclass hg-http-remote (hg-remote hg-http) ())
(defclass darcs-http-remote (darcs-remote darcs-http) ())
(defclass cvs-rsync-remote (cvs-remote cvs-rsync) ())
(defclass cvs-native-remote (cvs-remote cvs-native) ())
(defclass svn-rsync-remote (svn-remote svn-rsync) ())
(defclass svn-http-remote (svn-remote svn-http) ())
(defclass svn-native-remote (svn-remote svn-native) ())
(defclass tarball-http-remote (tarball-remote tarball-http) ())

;;; A special case location*vcs*role extension which is /going/ to be
;;; troublesome, as it violates simplicity.
(defclass gate-native-remote (gate-remote git-native-remote) ())
(defclass gate-http-remote (gate-remote git-http-remote) ())

;; Handle remote localisation, for printing purposes.
(defun remote-canonical-class-name (remote)
  (or (when (eq (remote-distributor remote) *self*)
        'gate-native-remote)
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
           ("hg+http" 'hg-http-remote)
           ("http" (ecase hint
                     (git 'git-http-remote)
                     (hg 'hg-http-remote)
                     (darcs 'darcs-http-remote)
                     (svn 'svn-http-remote)
                     ((nil) (definition-error "~@<The 'http' uri type is ambiguous, and there was no hint given.~:@>"))))
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
(defclass git-locality (git locality) ())
(defclass hg-locality (hg locality) ())
(defclass darcs-locality (darcs locality) ())
(defclass cvs-locality (cvs locality) ())
(defclass svn-locality (svn locality) ())
(defclass tarball-locality (tarball locality) ())

(defclass git-gate-locality (gate-native-remote git-locality gate-locality) ())

;;;;
;;;; Here are the constraints on the remote class precedence lists, as imposed by
;;;; generic functions.
;;;; The notation used is "X < Y" meaning that X precedes Y in the class precedence list.
;;;;
;;;; REMOTE < NONGIT-MIXIN < SEPARATE-CLONE, by FETCH-MODULE-USING-REMOTE :AROUND
;;;; REMOTE < WRINKLE-MIXIN, by URL-USING-MODULE :AROUND
;;;;

;;;
;;; Welcome to layers of crap upon layers of crap.
;;;
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

(defmethod vcs-type ((o (eql 'git-locality))) 'git)
(defmethod vcs-type ((o (eql 'hg-locality))) 'hg)
(defmethod vcs-type ((o (eql 'darcs-locality))) 'darcs)
(defmethod vcs-type ((o (eql 'cvs-locality))) 'cvs)
(defmethod vcs-type ((o (eql 'svn-locality))) 'svn)
(defmethod vcs-type ((o (eql 'tarball-locality))) 'tarball)
(defmethod vcs-type ((o (eql 'git-gate-locality))) 'git)

;;;
;;; Locality methods
;;;
(defmethod initialize-instance :after ((o locality) &key pathname omit-registration-by-path &allow-other-keys)
  (unless pathname
    (definition-error "~@<A location without path specified is useless. ~S is one of many.~:@>" o))
  (unless omit-registration-by-path
    (setf (locality-by-path pathname) o))
  (unless (directory-exists-p pathname)
    (ensure-directories-exist pathname)))

(defmethod initialize-instance :after ((o cvs-locality) &key pathname &allow-other-keys)
  (unless (directory-exists-p pathname)
    (ensure-directories-exist pathname))
  (ensure-directories-exist (cvs-locality-lock-path o) :verbose t))

(defgeneric update-gate-conversions (locality)
  (:documentation
   "Actually, this cannot be used on its own.")
  (:method ((o gate-locality))
    (when *verbose-repository-maintenance*
      (format t "~@<;; ~@;Updating module presence list in gate ~S.~:@>~%" (locality-pathname o)))
    (multiple-value-bind (publishable unpublishable hidden) (compute-locality-module-presence o)
      (let ((publishable-names (mapcar #'name publishable))
            (unpublishable-names (mapcar #'name unpublishable))
            (hidden-names (mapcar #'name hidden)))
        (when *verbose-repository-maintenance*
          (format t "~@<;; ~@;Publishable:~{ ~A~}~:@>~%" publishable-names)
          (format t "~@<;; ~@;Unpublished:~{ ~A~}~:@>~%" unpublishable-names)
          (format t "~@<;; ~@;Hidden:~{ ~A~}~:@>~%" hidden-names))
        (setf (gate-converted-module-names o) publishable-names
              (gate-unpublished-module-names o) unpublishable-names
              (gate-hidden-module-names o) hidden-names)))))

(defmethod shared-initialize :after ((o gate-locality) slot-names &key &allow-other-keys)
  (update-gate-conversions o))

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
(defgeneric define-local-distributor-locality (local-distributor name-qualifier type-or-vcs-type &rest arguments)
  (:documentation "Define a locality of VCS-TYPE in a subdirectory of LOCAL-DISTIBUTOR's root.")
  (:method ((o local-distributor) name-qualifier type-or-vcs-type &rest arguments)
    (multiple-value-bind (locality-type vcs-type) (if-let ((vcs-type (vcs-type type-or-vcs-type))) ;; fully qualified (approx)?
                                                    (values type-or-vcs-type
                                                            vcs-type)
                                                    (values (format-symbol #.*package* "~A-LOCALITY" type-or-vcs-type)
                                                            type-or-vcs-type))
      (setf (slot-value o vcs-type)
            (apply #'make-instance (find-class locality-type) :distributor o
                   :name (format-symbol #.*package* "~A~:[~;-~:*~A~]"
                                        (default-remote-name (name o) vcs-type 'native) name-qualifier)
                   :pathname (subdirectory* (root o) (string-downcase (string vcs-type)))
                   arguments)))))

(defmethod shared-initialize :after ((o local-distributor) slot-names &key &allow-other-keys)
  ;; The locality typed *gate-vcs-type* needs to be constructed differently between make-instance/change-class.
  (mapc (curry #'define-local-distributor-locality o nil) (remove *gate-vcs-type* *supported-vcs-types*)))

(defgeneric update-local-distributor-conversions (distributor converted-set)
  (:method ((o local-distributor) converted-set)
    (let ((gate (gate o)))
      (let (;; The logic here is that if we're engaging in this whole game,
            ;; we're only releasing via our gate remote.
            (release-set (location-module-names gate))
            (present (gate-converted-module-names gate))
            (unpublished (gate-unpublished-module-names gate))
            (hidden (gate-hidden-module-names gate)))
        (let* ((release-missing (set-difference release-set present))
               (converted-missing (set-difference converted-set present))
               (new-unpublished (append (intersection release-missing unpublished)
                                        (intersection converted-missing unpublished)))
               (new-hidden (append (intersection release-missing hidden)
                                   (intersection converted-missing hidden)))
               (new-demoted (append new-unpublished new-hidden))
               (release-set (set-difference release-set new-demoted))
               (converted-set (set-difference converted-set new-demoted))
               (release-mia (set-difference release-missing new-demoted))
               (converted-mia (set-difference converted-missing new-demoted)))
          (when new-unpublished
            (format t ";; Following modules were demoted to unpublished:~{ ~A~}~%" new-unpublished))
          (when new-hidden
            (format t ";; Following modules were demoted to hidden:~{ ~A~}~%" new-hidden))
          (when release-mia
            (restart-case (desire-error "~@<Release modules ~A, which are required to establish identity with distributor ~A, are missing from ~A.~:@>"
                                        release-mia (name o) (root o))
              (continue ()
                :report "Mark missing modules as no longer released."
                (nset-differencef release-set release-mia))))
          (when converted-mia
            (restart-case (desire-error "~@<Converted modules ~A, which are required to establish identity with distributor ~A, are missing from ~A.~:@>"
                                        converted-mia (name o) (root o))
              (continue ()
                :report "Mark missing modules as no longer converted."
                (nset-differencef converted-set converted-mia))))
          (dolist (m-name release-set)
            (let ((m (module m-name)))
              (unless (typep m 'origin-module)
                (change-class m 'origin-module))))
          (setf (location-module-names gate) release-set
                (gate-converted-module-names gate) converted-set))))))

(defgeneric initialise-wishmaster-gate (distributor &optional well-known-p root)
  (:method ((o local-distributor) &optional well-known-p root)
    (if well-known-p
        (let* ((gate (find-if (of-type 'git) (distributor-remotes o)))
               (converted (gate-converted-module-names gate)))
          (when-let ((intersection (intersection converted (location-module-names gate))))
            (definition-error "~@<Bad local definitions: intersection between exported ~
                                  and converted modules: ~S.~:@>"
                intersection))
          (setf (gate o) (change-class gate 'git-gate-locality :pathname (merge-pathnames #p"git/" root)))
          ;; the above line ran UPDATE-GATE-CONVERSIONS using SHARED-INITIALISE :AFTER on GATE-LOCALITY
          (update-local-distributor-conversions o converted))
        (define-local-distributor-locality o nil 'git-gate-locality :registrator #'(setf loc) :path nil))))

(defmethod update-instance-for-different-class :after ((d distributor) (w local-distributor) &key root &allow-other-keys)
  "Called once, during INIT, if we're pretending to be someone well-known."
  (initialise-wishmaster-gate w t root))

(defmethod initialize-instance :after ((o local-distributor) &key &allow-other-keys)
  (initialise-wishmaster-gate o))

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
                              (distributor-error distributor "~@<Specified remote name ~A conflicts in distributor ~A.~:@>" specified-name distributor-name)))
          (t (or (choose-default-remote-name distributor vcs-type transport)
                 (distributor-error distributor "~@<Cannot choose an unambiguous name for a ~A remote in distributor ~A, provide one explicitly.~:@>"
                                    vcs-type distributor-name))))))

(defmethod initialize-instance :before ((o remote) &key distributor vcs-type transport name &allow-other-keys)
  (setf (name o) (init-time-select-remote-name distributor vcs-type transport name)))

(defun compute-path-form (path)
  `(lambda ()
     (declare (special *module* *umbrella*))
     (list ,@(iter (for elt in path)
                   (collect
                       (etypecase elt
                         ((or string (eql :no/))
                          elt)
                         ((member *module* *umbrella*)
                          `(symbol-value (find-symbol ,(symbol-name elt) :desire)))
                         (t
                          (definition-error
                              "~@<Malformed remote pathname specifier ~S: ~
                              only strings and one of :NO/, DESR:*MODULE* and ~
                              DESR:*UMBRELLA* are allowed.~:@>"
                              elt))))))))

(defmethod initialize-instance :after ((o remote) &key distributor path &allow-other-keys)
  (appendf (distributor-remotes distributor) (list o))
  (setf (remote-path-fn o) (compile nil (compute-path-form path))))

;;;;
;;;; Modules
;;;;
(defclass module (desirable)
  ((umbrella :accessor module-umbrella :initarg :umbrella :documentation "Transitory?")
   (system-path-whitelist :accessor module-system-path-whitelist :initarg :path-whitelist :documentation "Specified.")
   (system-path-blacklist :accessor module-system-path-blacklist :initarg :path-blacklist :documentation "Specified.")
   (scan-positive-localities :accessor module-scan-positive-localities :initarg :scan-positive-localities :documentation "Cache. Locality scans should fill this one.")
   (remotes :accessor module-remotes :initarg :remotes :documentation "Cache. COMPUTE-MODULE-CACHES")
   (systems :accessor module-systems :initarg :systems :documentation "Cache. COMPUTE-MODULE-CACHES"))
  (:default-initargs
   :registrator #'(setf module)
   ;; name and umbrella are mandatory
   :systems nil
   :path-whitelist nil
   :path-blacklist nil
   :scan-positive-localities nil
   :remotes nil))

(defclass origin-module (module) 
  ((status :accessor module-status :initarg :status)
   (public-packages :accessor module-public-packages :initarg :public-packages))
  (:default-initargs
   :status :unknown
   :public-packages nil))

(defmethod initialize-instance :around ((o module) &key name &allow-other-keys)
  (when (module name :if-does-not-exist :continue)
    (break))
  (call-next-method))

(defun location-link-module (location module)
  (pushnew (name module) (location-module-names location)))

(defun location-unlink-module (location module)
  (removef (location-module-names location) (name module)))

(defgeneric remote-link-module (remote module &key &allow-other-keys)
  (:documentation
   "Establish a 'publishes' relationship between REMOTE and MODULE.")
  (:method :around ((r wrinkle-mixin) (m module) &key wrinkle)
    (when (and wrinkle
               (not (string= wrinkle (down-case-name m))))
      (push (cons (name m) wrinkle) (wrinkles r)))
    (call-next-method))
  (:method ((r remote) (m module) &key &allow-other-keys)
    (location-link-module r m)
    (pushnew r (module-remotes m))))

(defgeneric remote-unlink-module (remote module)
  (:documentation
   "Undo a 'publishes' relationship between REMOTE and MODULE.")
  (:method :around ((r wrinkle-mixin) (m module))
    (removef (wrinkles r) (name m) :key #'car)
    (call-next-method))
  (:method ((r remote) (m module))
    (location-unlink-module r m)
    (removef (module-remotes m) r)))

(defun location-defines-module-p (location module)
  "See whether MODULE is defined by LOCATION."
  (let ((name (coerce-to-name module)))
    (or (not (null (find name (location-module-names location))))
        (when (typep location 'gate)
          (not (null (find name (gate-converted-module-names location))))))))

;;;;
;;;; Systems
;;;;
(defclass system-type-mixin ()
  ((pathname-type :accessor system-pathname-type :initarg :pathname-type)))

(defclass host-provided () ())
(defclass unknown () ())
(defclass asdf (system-type-mixin) () (:default-initargs :pathname-type "asd"))
(defclass mudball (system-type-mixin) () (:default-initargs :pathname-type "mb"))
(defclass xcvb (system-type-mixin) () (:default-initargs :pathname-type "xcvb"))

(defclass system (desirable)
  ((module :accessor system-module             :initarg :module
                                               :documentation "Specified.")
   (applications :accessor system-applications :initarg :applications
                                               :documentation "List of applications. Cache.")
   (definition-pathname-name :accessor system-definition-pathname-name
                                               :initarg :definition-pathname-name
                                               :documentation "Specified, see documentation for SYSTEM-HIDDEN-P.")
   (direct-dependency-names :reader system-direct-dependency-names
                                               :initarg :direct-dependency-names
                                               :documentation "List of system names. Cache.")
   (dependencies :reader system-dependencies   :initarg :dependencies
                                               :documentation "Complete list of dependency objects. Cache.")
   (definition-complete-p :reader system-definition-complete-p
                                               :initarg :definition-complete-p
                                               :documentation "All dependencies are known systems. Cache."))
  (:default-initargs
   :registrator #'(setf system)
   :module nil
   :applications nil
   :definition-pathname-name nil))

(defclass host-system (host-provided system)
  ()
  (:default-initargs
   :module nil
   :direct-dependency-names nil
   :dependencies nil
   :definition-complete-p t))

(defclass unknown-system (unknown system)
  ()
  (:default-initargs
   :module nil
   :direct-dependency-names nil
   :dependencies nil
   :definition-complete-p nil))

(defclass known-system (system)
  ((definition-pathname :reader system-definition-pathname :initarg :definition-pathname :documentation "Cache.")
   (definition-write-date :reader system-definition-write-date :initarg :definition-write-date :documentation "Cache."))
  (:default-initargs
   :definition-write-date nil
   :definition-complete-p nil)
  (:documentation
   "Note that we don't remember how to find systems within the module's
directory hierarchy, as we rely on the recursor to properly register the
system with the system definition backend, so we can query the backend,
when needed."))

(defun system-hidden-p (system)
  "A hidden system is a system whose definition resides in a file
named differently from system's name. We have to store the name of
the definition file for such systems.
Find out whether SYSTEM is hidden."
  (declare (type system system))
  (not (null (system-definition-pathname-name system))))

(defun system-host-p (system)
  "Whether SYSTEM is provided by the host implementation."
  (declare (type system system))
  (typep system 'host-system))

(defun system-known-p (system)
  "Whether SYSTEM is mentioned in DEFINITIONS."
  (declare (type system system))
  (not (typep system 'unknown-system)))

(defun system-locally-present-p (system)
  "Whether SYSTEM is present within local gate."
  (declare (type system system))
  (and (system-known-p system)
       (not (system-host-p system))
       (slot-boundp system 'definition-pathname)))

(defun system-makunpresent (system)
  "Change SYSTEM so that SYSTEM-LOCALLY-PRESENT-P will return NIL on it."
  (declare (type system system))
  (when (system-host-p system)
    (system-error system "~@<SYSTEM-MAKUNPRESENT: asked to make a host system ~A unpresent.~:@>"
                  (name system)))
  (setf (slot-value system 'definition-complete-p) nil)
  (slot-makunbound system 'dependencies)
  (slot-makunbound system 'direct-dependency-names)
  (slot-makunbound system 'definition-pathname)
  (slot-makunbound system 'definition-write-date))

(defclass asdf-system (asdf known-system) ())
(defclass mudball-system (mudball known-system) ())
(defclass xcvb-system (xcvb known-system) ())

(defmethod initialize-instance :after ((o known-system) &key module &allow-other-keys)
  (unless (system-host-p o)
    (appendf (module-systems module) (list o))))

;;;;
;;;; Applications
;;;;
(defclass application (desirable)
  ((system :accessor app-system :initarg :system :documentation "Specified.")
   (package :accessor app-package :initarg :package :documentation "Specified.")
   (function :accessor app-function :initarg :function :documentation "Specified.")
   (default-parameters :accessor app-default-parameters :initarg :default-parameters :documentation "Specified."))
  (:default-initargs
   :registrator #'(setf app) :system nil :package nil :function nil :default-parameters nil))

(defmethod initialize-instance :after ((o application) &key system &allow-other-keys)
  (appendf (system-applications system) (list o)))

;;;;
;;;; Object nomenclature
;;;;
(define-root-container *distributors*   distributor   :name-transform-fn coerce-to-namestring :remover %remove-distributor :coercer t :mapper map-distributors :iterator do-distributors)
(define-root-container *modules*        module        :name-transform-fn coerce-to-namestring :remover %remove-module :coercer t :mapper map-modules :if-exists :error :iterator do-modules)
(define-root-container *leaves*         leaf          :name-transform-fn coerce-to-namestring :type module :mapper map-leaves :if-exists :continue)
(define-root-container *nonleaves*      nonleaf       :name-transform-fn coerce-to-namestring :type module :mapper map-nonleaves :if-exists :continue)
(define-root-container *systems*        system        :name-transform-fn coerce-to-namestring :remover %remove-system :coercer t :mapper map-systems :if-exists :error :iterator do-systems)
(define-root-container *apps*           app           :name-transform-fn coerce-to-namestring :remover %remove-app :coercer t :mapper map-apps :type application)
(define-root-container *remotes*        remote        :name-transform-fn coerce-to-namestring :remover %remove-remote :coercer t :mapper map-remotes :type remote :if-exists :error :iterator do-remotes)
(define-root-container *localities*     loc           :type locality :mapper map-localities :if-exists :error)
(define-root-container *credentials*    cred          :type credentials :iterator do-credentials :if-exists :error)
(define-root-container *localities-by-path* locality-by-path :type locality :if-exists :error)

;;;;
;;;; Distributors, in context of knowledge base
;;;;
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
;;;; Object knowledge base tampering
;;;;
(defun remove-distributor (distributor-designator &aux
                           (d (coerce-to-distributor distributor-designator)))
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

(defun remove-remote (remote-designator &key keep-modules &aux
                      (r (coerce-to-remote remote-designator)))
  (removef (distributor-remotes (remote-distributor r)) r)
  (do-remove-remote r :keep-modules keep-modules))

(defun do-remove-module (m)
  (dolist (s (module-systems m))
    (do-remove-system s))
  (%remove-module (name m)))

(defun remove-module (module-designator &key keep-locations &aux
                      (m (coerce-to-module module-designator)))
  (unless keep-locations
    (do-remotes (r)
      (when (location-defines-module-p r m)
        (removef (location-module-names r) (name m))
        (when (typep r 'gate)
          (removef (gate-converted-module-names r) (name m))
          (removef (gate-unpublished-module-names r) (name m))
          (removef (gate-hidden-module-names r) (name m))))))
  (do-remove-module m))

(defun do-remove-system (s)
  (when (system-host-p s)
    (system-error s "~@<Trying to remove host-provided system ~A.~:@>" (name s)))
  (dolist (a (system-applications s))
    (%remove-app a))
  (%remove-system (name s)))

(defun remove-system (system-designator &aux
                      (s (coerce-to-system system-designator)))
  (do-remove-system s)
  (when (system-known-p s)
    (removef (module-systems (system-module s)) s)))

(defun remove-app (app-designator &aux
                   (a (coerce-to-application app-designator)))
  (removef (system-applications (app-system a)) a)
  (%remove-app (name a)))

;;;
;;; Conditions.
;;;
(define-condition desire-condition (condition) ())
(define-condition definition-condition (desire-condition) ())
(define-condition recursor-condition (desire-condition) ())

(define-condition distributor-condition (desire-condition) ((distributor :reader condition-distributor :initarg :distributor)))
(define-condition remote-condition (desire-condition)      ((remote :reader condition-remote :initarg :remote)))
(define-condition locality-condition (desire-condition)    ((locality :reader condition-locality :initarg :locality)))
(define-condition module-condition (desire-condition)      ((module :reader condition-module :initarg :module)))
(define-condition system-condition (desire-condition)      ((system :reader condition-system :initarg :system)))
(define-condition application-condition (desire-condition) ((application :reader condition-application :initarg :application)))
(define-condition repository-condition (locality-condition module-condition)
  ((pathname :reader condition-pathname :initarg :pathname)))

(define-condition desire-error (desire-condition error) ())
(define-condition definition-error (definition-condition desire-error) ())
(define-condition recursor-error (recursor-condition desire-error) ())

(define-condition distributor-error (distributor-condition desire-error) ())
(define-condition remote-error (remote-condition desire-error) ())
(define-condition locality-error (locality-condition desire-error) ())
(define-condition module-error (module-condition desire-error) ())
(define-condition system-error (system-condition desire-error) ())
(define-condition application-error (application-condition desire-error) ())
(define-condition repository-error (repository-condition desire-error) ())

(define-simple-error desire-error)
(define-simple-error definition-error)
(define-simple-error recursor-error)

(define-simple-error distributor-error :object-initarg :distributor)
(define-simple-error remote-error :object-initarg :remote)
(define-simple-error locality-error :object-initarg :locality)
(define-simple-error module-error :object-initarg :module)
(define-simple-error system-error :object-initarg :system)
(define-simple-error application-error :object-initarg :application)

(define-reported-condition insatiable-desire (desire-error)
  ((desire :accessor condition-desire :initarg :desire))
  (:report (desire)
           "~@<It is not known to me how to satisfy the desire for ~S.~:@>" desire))

;;;;
;;;; Remotes, in context of knowledge base
;;;;
(defun match-credentials (username password)
  "Find a named credentials entry with matching USERNAME and PASSWORD."
  (do-credentials (c)
    (when (credentials-match-p c username password)
      (return c))))

(defun parse-remote-namestring (namestring &key gate-p slashless type-hint)
  "Given a remote NAMESTRING, deduce the remote's type, host, port and path,
and return them as multiple values.
Note that http is interpreted as git-http -type remote.
DARCS/CVS/SVN need darcs://, cvs:// and svn:// schemas, correspondingly."
  (multiple-value-bind (schema user password hostname port path directoryp) (parse-uri namestring :slashless-header slashless)
    (let ((cred (when user
                  (or (match-credentials user password)
                      (desire-error "~@<Credentials were provided in an URI namestring ~S, but were not recognised.~:@>" namestring)))))
      (values (or (uri-type-to-remote-type schema :gate-p gate-p :hint type-hint)
                  (desire-error "Bad URI type ~S in remote namestring ~S." schema namestring))
              cred hostname port path directoryp))))

;;; NOTE: this is the reason why remotes have names
;;;
;;; Why it's here?  The metastore functions below need it.
(defun git-fetch-remote (remote module-name &optional directory)
  "Fetch from REMOTE, with working directory optionally changed
to DIRECTORY."
  (maybe-within-directory directory
    (let ((module-url (url remote module-name)))
      (ensure-gitremote (name remote) module-url))
    (fetch-gitremote (name remote))))

;;;;
;;;; Modules, in context of knowledge base
;;;;
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
             (multiple-value-bind (core maybe-wrinkle) (call-next-method)
               (values (apply #'concatenate 'simple-base-string
                              (downstring (schema r))
                              core
                              (unless (remote-domain-name-takeover r)
                                (down-case-name (remote-distributor r)))
                              (unless (remote-domain-name-takeover r)
                                (when-let ((port (remote-distributor-port r)))
                                  (format nil ":~D" port)))
                              (unless (remote-domain-name-takeover r)
                                "/")
                              path)
                       maybe-wrinkle))))
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

(defun url (remote &optional module)
  "Compute the URL to MODULE within REMOTE."
  (declare (type (or remote symbol) remote) (type (or module symbol) module))
  (url-using-module (coerce-to-remote remote)
                    (or (when (symbolp module)
                          (module module :if-does-not-exist :continue))
                        module)))

(defun canonicalise-name (name &optional preserve-case (package (load-time-value (find-package :desire))))
  "Given an object's NAME, whether in form of a string, keyword or a symbol
in any other package, return the canonical name, as a symbol in the
'DESIRE' package.."
  (intern (xform (not preserve-case) #'string-upcase
                 (etypecase name
                   (symbol (symbol-name name))
                   (string name)))
          package))

(defun compute-module-presence (module &optional (locality (gate *self*)))
  (git-repository-present-p (module-pathname module locality)))

(defun update-module-presence (module &optional (locality (gate *self*)) (force-to nil forcep))
  "Recompute MODULE's presence in LOCALITY, updating MODULE's presence cache.
Optionally, when FORCE-TO is specified, the actual check is omitted and the
value of FORCE-TO is assumed."
  (with-slots (scan-positive-localities) module
    (lret* ((actual-presence (compute-module-presence module locality))
            (presence (if forcep force-to actual-presence)))
      (if presence
          (pushnew locality scan-positive-localities)
          (removef scan-positive-localities locality))
      (when *verbose-repository-maintenance*
        (format t "~@<;;; ~@;looked up ~A: ~:[missing~;present~], new presence list: ~S~:@>~%"
                (name module) actual-presence (mapcar #'name scan-positive-localities))))))

(defun module-locally-present-p (module-or-name &optional (locality (gate *self*)) check-when-present-p (check-when-missing-p t) &aux
                                 (module (coerce-to-module module-or-name)))
  "See if MODULE's presence cache is positive for LOCALITY, failing that perform
actual repository check and update the presence cache.
CHECK-WHEN-PRESENT-P determines if presence check is forced when MODULE's cache
is positive.
CHECK-WHEN-MISSING-P defermines if presence check is performed upon negative
cache results."
  (if-let ((cache-hit (find locality (module-scan-positive-localities module))))
    (if check-when-present-p
        (update-module-presence module locality)
        t)
    (if check-when-missing-p
        (update-module-presence module locality)
        nil)))

(defun module-best-remote (module &key (if-does-not-exist :error) allow-self)
  "Return the preferred remote among those providing MODULE.
Currently implements a static 'gates are preferred' policy."
  (let ((module (coerce-to-module module)))
    (or (choose-gate-or-else (do-remotes (r)
                               (when (and (location-defines-module-p r module)
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

(defun module-fetch-url (module &key allow-self (if-does-not-exist :error))
  "Return the URL which is to be used while fetching MODULE,
that is the location of MODULE in the preferred remote.
When ALLOW-SELF is specified, and non-NIL, remotes within *SELF* are
not discarded from consideration."
  (let ((module (coerce-to-module module)))
    (when-let ((remote (module-best-remote module :allow-self allow-self :if-does-not-exist if-does-not-exist)))
      (url remote module))))

(defun touch-module (module &aux
                     (module (coerce-to-module module)))
  "Try 'access' MODULE via its preferred remote and return
whether the attempt was successful."
  (let* ((best-remote (module-best-remote module :if-does-not-exist :continue))
         (localp (and (null best-remote) (module-best-remote module :allow-self t))))
    (if localp
        (prog1 t
          (write-line "Module is local."))
        (touch-remote-module best-remote module))))

(defun choose-gate-or-else (remotes)
  (or (find-if (of-type 'gate) remotes)
      (first remotes)))

(defun distributor-module-enabled-remote (distributor module &aux
                                          (distributor (coerce-to-distributor distributor))
                                          (module (coerce-to-module module)))
  "Return the first non-disabled DISTRIBUTOR's remote providing MODULE.
   The second value is a boolean, indicating non-emptiness of the set of
   providing remotes, regardless of the enabled-p flag."
  (choose-gate-or-else
   (do-distributor-remotes (r distributor)
     (when (and (location-defines-module-p r module) (not (remote-disabled-p r)))
       (collect r)))))

(defun hide-module (module &optional (locality (gate *self*)) &aux
                    (module (coerce-to-module module))
                    (name (name module)))
  "Stop advertising MODULE in definitions, as well as make it inaccessible
to general public within LOCALITY."
  (removef (location-module-names locality) name)
  (removef (gate-converted-module-names locality) name)
  (removef (gate-unpublished-module-names locality) name)
  (pushnew name (gate-hidden-module-names locality))
  (setf (git-repository-world-readable-p (module-pathname module locality)) nil))

(defun make-module-unpublished (module &optional (locality (gate *self*)) &aux
                                (module (coerce-to-module module))
                                (name (name module)))
  "Stop advertising MODULE in definitions, without completely hiding it.
If it is hidden, unhide it."
  (removef (location-module-names locality) name)
  (removef (gate-converted-module-names locality) name)
  (pushnew name (gate-unpublished-module-names locality))
  (setf (git-repository-world-readable-p (module-pathname module locality)) t)
  (removef (gate-hidden-module-names locality) name))

(defun declare-module-converted (module &optional (locality (gate *self*)) &aux
                                 (module (coerce-to-module module))
                                 (name (name module)))
  "Advertise MODULE as converted by LOCALITY."
  (unless (typep locality 'gate-locality)
    (desire-error "~@<The locality argument must be a local gate.~:@>"))
  (when (find name (location-module-names locality))
    (module-error "~@<Cannot declare ~A as converted by local gate ~A, as it is already released there.~:@>"
                  name (name locality)))
  (pushnew name (gate-converted-module-names locality))
  (removef (gate-unpublished-module-names locality) name)
  (removef (gate-hidden-module-names locality) name))

(defun module-hidden-p (module &optional (locality (gate *self*)))
  "Determine whether MODULE is hidden, with regard to LOCALITY.
This is a function of MODULE repository's anonymous accessibility."
  (not (git-repository-world-readable-p (module-pathname (coerce-to-module module) locality))))

(defun module-publishable-p (module &optional (locality (gate *self*)))
  "Determine whether MODULE is publishable via gate LOCALITY.
This is a function of intent expressed by MODULE's membership
in non-unpublished remotes."
  (not (null (or (find (name module) (location-module-names locality))
                 (find (name module) (gate-converted-module-names locality))))))

(defun compute-locality-module-presence (&optional (locality (gate *self*)))
  "Scan LOCALITY for known modules and update its and those modules's idea
about their relationship.
The value returned is the list of found modules."
  (do-modules (module)
    (when (update-module-presence module locality)
      (cond ((module-hidden-p module locality)
             (collect module into hidden-modules))
            ((not (module-publishable-p module locality))
             (collect module into unpublishable-modules))
            (t
             (collect module into publishable-modules))))
    (finally (return (values publishable-modules
                             unpublishable-modules
                             hidden-modules)))))

(defmacro do-present-modules ((module &optional (locality '(gate *self*)) block-name) &body body)
  "Iterate the BODY over all modules cached as present in LOCALITY, with MODULE specifying
the driven variable binding."
  `(do-modules (,module ,@(when block-name `(,block-name)))
     (when (module-locally-present-p ,module ,locality nil nil)
       ,@body)))

(defun missing-modules (&optional (locality (gate *self*)))
  (do-modules (m)
    (unless (or (location-defines-module-p locality m)
                (member (string (name m))
                        '("ASDF-BINARY-LOCATIONS" "ASDF-SYSTEM-CONNECTIONS" "CLBUILD" "COMMON-DB" "CVSPS"  "DARCS2GIT" "ECL" "FARE-UTILS" "GIT" "LIBPCIACCESS" "OGRE" "SBCL" "SALZA")
                        :test #'string=))
      (collect (name m)))))

(defmacro do-present-systems ((system &optional (locality '(gate *self*)) block-name) &body body)
  "Iterate the BODY over all systems whose modules are cached as being present in LOCALITY,
with SYSTEM specifying the driven variable binding."
  `(do-systems (,system ,@(when block-name `(,block-name)))
     (when (and (system-known-p ,system)
                (or (system-host-p ,system)
                    (module-locally-present-p (system-module ,system) ,locality nil nil)))
       ,@body)))

;;;;
;;;; Metastore: communication with other desire nodes
;;;;
(defun reset-metastore (&optional (metastore-pathname (meta *self*)))
  "Drop unsaved changes recorded in METASTORE-PATHNAME."
  (git-set-branch-index-tree nil metastore-pathname))

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
          (unless (git-branch-present-p :master)
            (git-set-branch :master))
          (git-set-head-index-tree :master)
          (git-set-branch-index-tree (make-remote-ref remote-name branch)))))))

(defun reestablish-metastore-subscriptions (metastore-pathname)
  (within-directory (metastore-pathname)
    (iter (for (nil remote-name branch-name) in (remove-if-not #'ref-remotep (refs-by-value (ref-value '("master") metastore-pathname)
                                                                                            metastore-pathname)))
          (for d = (distributor (string-upcase remote-name) :if-does-not-exist :continue))
          (unless d (continue))
          (for rel = (make-instance 'definition-subscription :from *self* :to d :branch branch-name))
          (set-rel d *self* rel)
          (set-rel *self* d rel))))

(defmacro within-wishmaster-meta ((wishmaster branch &key update-p) &body body)
  (once-only (wishmaster)
    `(within-directory ((meta *self*))
       ,@(when update-p `((git-fetch-remote (gate ,wishmaster) :.meta)))
       (unwind-protect (progn (git-set-head-index-tree (make-remote-ref (down-case-name ,wishmaster) ,branch))
                              ,@body)
         (git-set-head-index-tree :master)))))

(defun merge-remote-wishmaster (wishmaster)
  "Merge definitions from WISHMASTER."
  (let ((branch (if-let ((r (rel *self* wishmaster)))
                  (rel-branch r)
                  "master"))
        (metastore (meta *self*)))
    (within-wishmaster-meta (wishmaster branch :update-p t)
      (read-definitions :source wishmaster :force-source nil :metastore metastore))))

(defun merge-remote-wishmasters ()
  (do-wishmasters (w)
    (unless (eq w *self*)
      (merge-remote-wishmaster w))))

;;;
;;; Desires.  Bitrotten and unused.
;;;
(defun distributor-related-desires (distributor-spec)
  "Yield the names of modules currently desired from DISTRIBUTOR-SPEC."
  (rest (find (coerce-to-name distributor-spec) *desires*)))

(defun set-distributor-related-desires (new distributor-spec)
  (setf (rest (find (coerce-to-name distributor-spec) *desires*)) new))

(defsetf distributor-related-desires set-distributor-related-desires)

(defun add-desire (distributor &optional (module-spec :everything) &aux
                   (distributor (coerce-to-distributor distributor)))
  "Add MODULE-SPEC (which is either a list of module specifications or an
   :EVERYTHING wildcard) to the list of modules desired from DISTRIBUTOR."
  (check-type module-spec (or (eql :everything) list))
  (unionf (distributor-related-desires distributor)
          (if (eq module-spec :everything)
              (compute-distributor-modules distributor)
              (mapcar #'coerce-to-name module-spec))))

(defun module-desired-p (module &aux
                         (module (coerce-to-module module)))
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

;;;;
;;;; Localities.  Utilities not used anywhere at this moment.
;;;;
(defun define-locality (name vcs-type &rest keys &key &allow-other-keys)
  "Define locality of VCS-TYPE at PATH, if one doesn't exist already, 
   in which case an error is signalled."
  (apply #'make-instance (format-symbol (symbol-package vcs-type) "~A-LOCALITY" vcs-type) :name name (remove-from-plist keys :name)))
