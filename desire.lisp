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


(defparameter *implementation-provided-systems*
  #+sbcl '("ASDF-INSTALL" "SB-ACLREPL" "SB-BSD-SOCKETS" "SB-COVER" "SB-GROVEL" "SB-MD5" "SB-POSIX" "SB-ROTATE-BYTE" "SB-RT" "SB-SIMPLE-STREAMS")
  #-sbcl nil)

;;;
;;; Remote update-ness:
;;;
;;; git: peek-remote, contents of HEAD
;;; svn: rsync, mod date of db/current
;;; cvs: rsync, mod date of CVSROOT/history (anything better?)
;;; darcs: no wai? (http header wootery?)
;;;

;; (defgeneric time-identity (repo)
;;   (:method-combination primary-method-not-required)
;;   (:method :around ((o symbol))
;;     (get-time-identity (module o)))
;;   (:method :around ((o module))
;;     (get-time-identity (module-master-repository o)))
;;   (:method :around ((o repository))
;;     (file-write-date (merge-pathnames (call-next-method) (path o))))
;;   (:method ((o local-git-repository))
;;     (make-pathname :directory '(:relative ".git" "refs" "heads") :name "master"))
;;   (:method ((o local-darcs-repository))
;;     (make-pathname :directory '(:relative "_darcs" "patches")))
;;   (:method ((o local-cvs-repository))
;;     (make-pathname :directory '(:relative "CVSROOT") :name "history"))
;;   (:method ((o local-svn-repository))
;;     (make-pathname :directory '(:relative "db") :name "current")))

;;;   (multiple-value-bind (primary backups) (url from)
;;;     (with-condition-restart-binding ((executable-failure continue))
;;;       (iter (for url in (cons primary backups))
;;;             (restart-bind ((continue (lambda (cond) (declare (ignore cond)) (format t "failed to fetch from ~S~%" url) (next-iteration))
;;;                              :report-function (lambda (stream) (format stream "Try fetching from other backup URLs."))))
;;;               (call-next-method o :url url)
;;;               (return-from fetch-module t))
;;;             (finally (warn 'module-fetch-failed :module o)))))

(defvar *purgeworth-binaries* 
  '("dfsl"        ;; OpenMCL
    "ppcf" "x86f" ;; CMUCL
    "fasl"        ;; SBCL
    "fas" "o"     ;; ECL
    "lib" "obj"   ;; ECL/win32
    )) 

(defun purge-binaries (&optional directory)
  "Purge files with type among one of *PURGEWORTH-BINARIES* either in DIRECTORY,
   or, when it's NIL, in *DEFAULT-PATHNAME-DEFAULTS*."
  (dolist (type *purgeworth-binaries*)
    (mapc #'delete-file (directory (subfile directory '(:wild-inferiors :wild) :type type)))))

(define-reported-condition repository-not-clean-during-fetch (repository-error executable-failure) ()
  (:report (locality module)
           "~@<repository for ~S in ~S has uncommitted changes during fetch~:@>" module locality))

(defgeneric fetch-remote (locality remote module))

(defmethod fetch-remote ((locality git-locality) (remote git) module)
  (let ((repo-dir (module-pathname module locality)))
    (unless (directory-exists-p repo-dir)
      (within-directory (repo-dir :lisp nil :if-exists :error :if-does-not-exist :create)
        (with-explanation ("initialising git repository of module ~A in ~S" (name module) *default-pathname-defaults*)
          (git "init-db"))))
    (git-fetch-remote remote (name module) repo-dir)))

(defmethod fetch-remote ((git-locality git-locality) (remote darcs-http-remote) module)
  (let ((darcs-repo-dir (module-pathname module (local-darcs *self*)))
        (url (url remote module)))
    (if (directory-exists-p darcs-repo-dir)
        (with-explanation ("on behalf of module ~A, pulling from darcs remote ~A to ~S" (name module) url darcs-repo-dir)
          (darcs "pull" "--all" "--repodir" (namestring darcs-repo-dir) url))
        (with-explanation ("on behalf of module ~A, getting from darcs remote ~A to ~S" (name module) url darcs-repo-dir)
          (darcs "get" url (namestring darcs-repo-dir))))))

(defmethod fetch-remote ((git-locality git-locality) (remote cvs-rsync-remote) module)
  (let* ((cvs-locality (local-cvs *self*))
         (cvs-repo-dir (module-pathname module cvs-locality))
         (url (url remote module)))
    (with-explanation ("on behalf of module ~A, rsyncing from cvs remote ~A to ~S" (name module) url cvs-repo-dir)
      (rsync "-ravPz" (format nil "~A/cvsroot/" url) (namestring cvs-repo-dir)))))

(defmethod fetch-remote ((git-locality git-locality) (remote svn-rsync-remote) module)
  (let ((svn-repo-dir (module-pathname module (local-svn *self*)))
        (url (url remote module)))
    (with-explanation ("on behalf of module ~A, rsyncing from svn remote ~A to ~S" (name module) url svn-repo-dir)
      (rsync "-ravPz" url (namestring svn-repo-dir)))))

(defmethod fetch-remote :around (locality remote module)
  (with-error-resignaling (executable-failure
                           ((cond) 'fetch-failure :remote remote :module module :execution-error (format nil "~A" cond)))
    (call-next-method)))

(defmethod fetch :around (locality remote module)
  (with-maybe-just-printing-conditions (t fetch-failure) (not *fetch-errors-serious*)
    (restart-bind ((retry (lambda () 
                            (maybe-within-directory (module-pathname module locality)
                              (git "gui"))
                            (invoke-restart (find-restart 'retry)))
                     :test-function (of-type 'repository-not-clean-during-fetch)
                     :report-function (formatter "Launch git gui to fix the issue, then retry the command.")))
      (call-next-method))))

(defmethod fetch :before ((locality locality) (remote remote) module)
  (fetch-remote locality remote module))

(defmethod fetch ((to git-locality) (from git-locality) module)
  (fetch-remote to from module))

(defmethod fetch ((locality git-locality) (remote git-remote) module))

#+(or)
(defmethod fetch ((git-locality git-locality) (remote hg-http-remote) module)
  (let ((hg-repo-dir (module-pathname module (local-hg *self*)))
        (git-repo-dir (module-pathname module git-locality)))))

(defmethod fetch ((git-locality git-locality) (remote darcs-http-remote) module)
  (let ((darcs-repo-dir (module-pathname module (local-darcs *self*)))
        (git-repo-dir (module-pathname module git-locality)))
    (purge-binaries git-repo-dir)
    (within-directory (git-repo-dir :lisp nil :if-does-not-exist :create)
      (with-explanation ("on behalf of module ~A, converting from darcs to git: ~S => ~S" (name module) darcs-repo-dir *default-pathname-defaults*)
        (darcs-to-git (namestring darcs-repo-dir))))
    (when (git-repository-bare-p git-repo-dir)
      (setf (git-repository-bare-p git-repo-dir) nil))))

(defmethod fetch ((git-locality git-locality) (remote cvs-rsync-remote) module &aux (name (down-case-name module)))
  (let* ((cvs-locality (local-cvs *self*))
         (cvs-repo-dir (module-pathname module cvs-locality))
         (git-repo-dir (module-pathname module git-locality)))
    (with-output-to-file (stream (subfile* cvs-repo-dir "CVSROOT" "config") :if-exists :supersede)
      (format stream "LockDir=~A~%" (namestring (cvs-locality-lock-path cvs-locality))))
    (with-exit-code-to-error-translation ((9 'repository-not-clean-during-fetch :module module :locality git-locality))
      (with-explanation ("on behalf of module ~A, converting from cvs to git: ~S => ~S" (name module) cvs-repo-dir git-repo-dir)
        (git "cvsimport" "-v" "-C" (namestring git-repo-dir) "-d" (format nil ":local:~A" (string-right-trim "/" (namestring cvs-repo-dir))) name)))))

(defmethod fetch ((git-locality git-locality) (remote svn-rsync-remote) module)
  (let ((svn-repo-dir (module-pathname module (local-svn *self*)))
        (git-repo-dir (module-pathname module git-locality)))
    (unless (directory-exists-p git-repo-dir)
      (within-directory (git-repo-dir :lisp nil :if-does-not-exist :create :if-exists :error)
        (with-explanation ("on behalf of module ~A, setting up svn to git conversion: ~S => ~S" (name module) svn-repo-dir *default-pathname-defaults*)
          (git "svn" "init" (format nil "file://~A" (namestring svn-repo-dir)))))) ;; gratuitious SVN complication
    (within-directory (git-repo-dir :lisp nil)
      (with-explanation ("on behalf of module ~A, converting from svn to git: ~S => ~S" (name module) svn-repo-dir *default-pathname-defaults*)
        (git "svn" "fetch")))))

(defmethod fetch :after ((git-locality git-locality) remote module)
  (setf (git-repository-world-readable-p (module-pathname module git-locality)) *default-world-readable*))

(defun fetch-anyway (module-name)
  (cons module-name nil))

(defun desire-do-one-step (desires skip-present system-vocabulary)
  (declare (special *locality*))
  (labels ((syspath (name) (declare (special *syspath*)) (gethash name *syspath*))
           (set-syspath (name value) (declare (special *syspath*)) (setf (gethash name *syspath*) value))
           (vocab-next-fail (vocab &optional (test #'eq))
             (car (find nil vocab :key #'cdr :test test)))
           (vocab-presentp (vocab subj &optional (test #'eq))
             (not (null (assoc subj vocab :test test))))
           (vocab-successp (vocab subj &optional (test #'eq))
             (cdr (assoc subj vocab :test test)))
           ((setf vocab-successp) (successp vocab subj &optional (test #'eq))
             (setf (cdr (assoc subj vocab :test test)) successp))
           (vocab-maybe-extend-require (vocab subj &optional (test #'eq))
             (if (assoc subj vocab :test test)
                 vocab
                 (acons subj nil vocab)))
           (make-fail-vocab (fail-subjs &optional vocab)
             (if fail-subjs
                 (acons (first fail-subjs) nil (make-fail-vocab (rest fail-subjs) vocab))
                 vocab))
           (next-unsatisfied-module () (vocab-next-fail desires))
           ((setf desire-satisfied) (val modname) (setf (vocab-successp desires modname) val))
           (next-unsatisfied-system (system-vocabulary) (vocab-next-fail system-vocabulary #'string=))
           (system-satisfiedp (system-vocabulary sysname) (vocab-successp system-vocabulary sysname #'string=))
           ((setf system-satisfiedp) (val system-vocabulary sysname) (setf (vocab-successp system-vocabulary sysname #'string=) val))
           (system-maybe-add-new (system-vocabulary sysname) (vocab-maybe-extend-require system-vocabulary sysname #'string=))
           (fetch-if-missing (module-name)
             (cons module-name (not (module-locally-present-p (module module-name) *locality*))))
           (register-new-system (name path type module)
             (report t ";; Registering a previously unknown system ~A~%" name)
             (setf *unsaved-definition-changes-p* t)
             (make-instance type :name name :module module
                            :definition-pathname-name (when-let* ((pathname-name (pathname-name path))
                                                                  (hidden-p (not (equal pathname-name (downstring name)))))
                                                        pathname-name)))
           (add-system-dependencies (module system modules system-vocabulary)
             "Given a MODULE's SYSTEM, detect its dependencies and, accordingly, extend the sets
              of known REQUIRED systems, MODULES and MISSING unknown systems, returning them
              as multiple values."
             (multiple-value-bind (new-sysdeps new-missing) (system-dependencies system)
               (multiple-value-bind (local-newdeps othermodule-newdeps) (unzip (compose (feq module) #'system-module) new-sysdeps)
                 (let ((extended-system-vocabulary system-vocabulary))
                   (dolist (newdep local-newdeps)
                     (setf extended-system-vocabulary (system-maybe-add-new extended-system-vocabulary (string (name newdep)))))
                   (dolist (missing new-missing)
                     (setf extended-system-vocabulary (system-maybe-add-new extended-system-vocabulary missing)))
                   ;; NOTE: on module boundaries we lose precise system dependency names
                   (values (append (mapcar (compose #'name #'system-module) othermodule-newdeps) modules)
                           extended-system-vocabulary)))))
           (satisfy-next-system (module &optional modules system-vocabulary)
             "Given a MODULE and a list of its REQUIRED systems, pick one and try to handle
              the fallout. Return the modified sets of known REQUIRED systems, MODULES and
              MISSING unknown systems, returning them as multiple values."
             (declare (special *syspath*))
             (if-let ((name (next-unsatisfied-system system-vocabulary)))
               (let* ((system (system name))
                      (path (or (syspath name) (error "~@<Internal invariant violation during dependency resolution: failed to find system ~S among syspathed ~S~:@>~%" name (hash-table-keys *syspath*))))
                      (type (interpret-system-pathname-type path)))
                 (unless (typep system type)
                   (error "~@<During dependency resolution: asked for a system ~S of type ~S, got one of type ~S~:@>" type name (type-of system)))
                 (ensure-system-loadable system path *locality*)
                 ;; A hidden system is a system definition residing in a file named differently from main system's name.
                 ;; Find them.
                 (let ((hidden-system-names (and (eq type 'asdf-system) (not (system-hidden-p system)) (asdf-hidden-system-names system)))
                       hidden-system-vocabulary)
                   (dolist (hidden-name hidden-system-names)
                     (unless (vocab-presentp system-vocabulary hidden-name #'string=)
                       (setf hidden-system-vocabulary (system-maybe-add-new hidden-system-vocabulary hidden-name)))
                     (let ((hidden (or (system hidden-name :if-does-not-exist :continue)
                                       ;; XXX: how come we ever hit this?
                                       (register-new-system (intern hidden-name) path type module))))
                       (ensure-system-loadable hidden path *locality*)))
                   (setf (system-satisfiedp system-vocabulary name) t) ;; made loadable, deps added, hiddens uncovered
                   (add-system-dependencies module system modules (append system-vocabulary hidden-system-vocabulary))))
               (values modules system-vocabulary)))
           (add-visible-system (module name path system-vocabulary)
             (lret* ((type (interpret-system-pathname-type path))
                     (system (or (lret ((system (system name :if-does-not-exist :continue)))
                                   (when system
                                     (unless (typep system type)
                                       (error "~@<During dependency resolution: asked for a system ~S of type ~S, got one of type ~S~:@>" type name (type-of system)))))
                                 (register-new-system (intern name) path type module)))
                     (extended-system-vocabulary (acons name nil system-vocabulary)))
               (set-syspath name path)
               (dolist (hidden-system-name (and (eq type 'asdf-system) (asdf-hidden-system-names system)))
                 (set-syspath hidden-system-name path)
                 (unless (system hidden-system-name :if-does-not-exist :continue)
                   (register-new-system (intern hidden-system-name) path type module)))))
           (module-dependencies (m system-vocabulary)
             (let* ((required-sysfiles (module-system-definitions m 'asdf-system *locality*))
                    (unsatisfied-module-system-names (mapcar #'system-pathname-name required-sysfiles))
                    (extended-system-vocabulary system-vocabulary))
               (iter (for required-sysfile in required-sysfiles)
                     (for unsatisfied-module-system-name in unsatisfied-module-system-names)
                     (setf extended-system-vocabulary (add-visible-system m unsatisfied-module-system-name required-sysfile extended-system-vocabulary)))
               (iter (with modules)
                     ;; Progress is made because NEXT-UNSATISFIED-SYSTEM proceeds from the head of the vocabulary,
                     ;; where we've appended our required systems.
                     (for (values modules-new new-extended-system-vocabulary) = (satisfy-next-system m modules extended-system-vocabulary))
                     (setf (values modules extended-system-vocabulary) (values modules-new new-extended-system-vocabulary))
                     (when (every (curry #'system-satisfiedp extended-system-vocabulary) unsatisfied-module-system-names)
                       (return (values (remove-duplicates modules) extended-system-vocabulary)))))))
    (if-let ((an-unsatisfied-name (next-unsatisfied-module)))
      (let ((an-unsatisfied-module (module an-unsatisfied-name)))
        (fetch *locality* (module-best-remote an-unsatisfied-module) an-unsatisfied-module)
        (ensure-module-systems-loadable an-unsatisfied-module *locality*) ;; this either succeeds or signals an error
        (setf (desire-satisfied an-unsatisfied-name) t)
        (multiple-value-bind (module-deps new-system-vocabulary) (module-dependencies an-unsatisfied-module system-vocabulary)
          (let ((added-deps-from-this-module (remove-if (rcurry #'assoc desires) module-deps)))
            (appendf desires (mapcar (if skip-present #'fetch-if-missing #'fetch-anyway) added-deps-from-this-module))
            (report t "~&~@<;; ~@;~S,~:[~; added ~:*~A,~] ~D left~:@>~%"
                    an-unsatisfied-name added-deps-from-this-module (count-if-not #'cdr desires))
            (finish-output))
          (desire-do-one-step desires skip-present new-system-vocabulary)))
      (values t system-vocabulary))))

(defun desire (desires &key skip-present (seal-p t))
  "Satisfy module DESIRES and return the list of names of updated modules.

   Desire satisfaction means:
    - for specified missing modules, retrieval,
    - for specified present modules, update, unless SKIP-PRESENT is
      non-nil,

   In all cases, all systems present in the set union of specified and
   depended upon modules are ensured to be loadable.

   When individual desires are symbols, they are interpreted as module
   names, and are intepreted in the context of the global *DESIRES*.

   When they are lists, their first element is interpreted as the source
   distributor, from which the rest of the list is supposed to be imported.

   These two forms can be mixed in the list of desires.

   Defined keywords:
    - SKIP-PRESENT - whether to skip updating specified modules which are 
      already present, defaults to nil."
  (let* ((interpreted-desires (mapcar (curry #'xform-if-not #'consp (lambda (m) (list (name (module-best-distributor m)) m))) desires)))
    (iter (for (distributor-name . modules) in interpreted-desires)
          (for distributor = (distributor distributor-name))
          (when-let ((missing (remove-if (curry #'distributor-module-enabled-remote distributor) modules)))
            (error "~@<Distributor ~S does not provide following modules: ~S~:@>" distributor missing)))
    (let* ((*desires* (substitute-desires *desires* (remove-if-not #'consp desires)))
           (*locality* (gate *self*))
           (*syspath* (make-hash-table :test #'equal))
           (desired-list (mapcan #'rest interpreted-desires))
           (initial-system-vocabulary (mapcar (rcurry #'cons t) *implementation-provided-systems*)))
      (declare (special *locality* *syspath*))
      (report t "; Satisfying desire for ~D module~:*~P:~%" (length desired-list))
      (multiple-value-bind (success system-vocabulary) (desire-do-one-step (mapcar #'fetch-anyway desired-list) skip-present initial-system-vocabulary)
        (declare (ignore success))
        (when-let ((missing-system-names (mapcar #'car (remove t system-vocabulary :key #'cdr))))
          (report t "; Required systems missing from definitions:~{ ~A~}~%" missing-system-names)))
      (when (and *unsaved-definition-changes-p* seal-p)
        (report t "; Definitions modified, committing changes.~%")
        (save-current-definitions :seal-p t))
      (report t "; All done.~%")
      t)))

(defun lust (&rest desires)
  "A spread interface function for DESIRE.
Updates present specified modules and skips present depended ones."
  (desire desires))

;; (defgeneric fetch-desired-p (repo)
;;   (:method ((o derived-repository))
;;     (or (not (probe-file (path o)))
;;         (etypecase (repo-master o)
;;           (local-repository (> (time-identity (repo-master o)) (time-identity o)))
;;           (remote-repository t)))))

;; (defun update (os &key skip-loadable (check-success t) &aux (o (coerce-to-module os)))
;;   (let* ((full-set (module-full-dependencies o))
;;          (initially-unloadable (remove-if #'loadable-p full-set)))
;;     (mapc #'update-single (xform skip-loadable (curry #'remove-if #'loadable-p) full-set))
;;     (let* ((still-unloadable (remove-if #'loadable-p full-set))
;;            (degraded (intersection still-unloadable (set-difference full-set initially-unloadable))))
;;       (cond ((not check-success)
;;              (warn "~@<success check suppressed~:@>"))
;;             (degraded
;;              (error "~@<modules degraded after update: ~S~:@>" degraded))
;;             (still-unloadable
;;              (error "~@<modules remained unloadable after update: ~S~:@>" still-unloadable))
;;             (t)))))
