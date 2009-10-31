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

(defgeneric touch-remote-module (remote module)
  (:method :around ((o remote) (m module))
           (touch-remote-module o (name m)))
  (:method ((o git-remote) name)
    (with-valid-exit-codes ((128 nil)) (git "peek-remote" (url o name))))
  (:method ((o darcs-http-remote) name)
    (with-valid-exit-codes ((8 nil)) (wget "--spider" `(,(url o name) "_darcs/inventory"))))
  (:method ((o rsync) name)
    (with-valid-exit-codes ((23 nil)) (rsync "--list-only" (url o name))))
  (:method ((o cvs-native-remote) name)
    (with-valid-exit-codes ((1 nil)) (cvs "-d" (url o name) "history")))
  (:method ((o svn-http-remote) name)
    (with-valid-exit-codes ((8 nil)) (wget "--spider" (url o name)))))

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
      (rsync "-ravPz" url (namestring cvs-repo-dir)))))

(defmethod fetch-remote ((locality git-locality) (cvs cvs-native-remote) module)
  (let ((repo-dir (module-pathname module locality)))
    (multiple-value-bind (url module-name) (url cvs module)
      (within-directory (repo-dir :if-does-not-exist :create)
        (git "cvsimport" "-d" url module-name)))))

(defmethod fetch-remote ((git-locality git-locality) (remote svn-rsync-remote) module)
  (let ((svn-repo-dir (module-pathname module (local-svn *self*)))
        (url (url remote module)))
    (with-explanation ("on behalf of module ~A, rsyncing from svn remote ~A to ~S" (name module) url svn-repo-dir)
      (rsync "-ravPz" url (namestring svn-repo-dir)))))

(defmethod fetch-remote ((locality git-locality) (svn svn-http-remote) module)
  (let* ((repo-dir (module-pathname module locality))
         (exists-p (directory-exists-p repo-dir))
         (url (url svn module)))
    (within-directory (repo-dir :if-does-not-exist :create)
      (unless exists-p
        (with-explanation ("on behalf of module ~A, initialising import to git repository from SVN ~S in ~S" (name module) url *default-pathname-defaults*)
          (git "svn" `("-T" ,url))))
      (with-explanation ("on behalf of module ~A, importing from ~S in ~S" (name module) url *default-pathname-defaults*)
        (git "svn" "fetch")))))

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
                     :report-function (formatter "Launch git gui to fix the issue, then retry the operation.")))
      (call-next-method))))

(defmethod fetch :before ((locality locality) (remote remote) module)
  (fetch-remote locality remote module))

(defmethod fetch ((to git-locality) (from git-locality) module)
  (fetch-remote to from module))

(defmethod fetch ((locality git-locality) (remote git-remote) module))
(defmethod fetch ((locality git-locality) (remote cvs-native-remote) module))
(defmethod fetch ((locality git-locality) (remote svn-http-remote) module))

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

(defgeneric update-module-using-remote (module remote &optional locality)
  (:method (module remote &optional (locality (gate *self*)))
    (fetch locality remote module))
  (:method :after (module (remote git-remote) &optional locality)
    (reset-gitbranch-to-remote-branch :master `(,(down-case-name remote) "master") (module-pathname module locality) t)))

(defun update-module (module &optional (locality (gate *self*)))
  (let ((best-remote (module-best-remote module)))
    (report t ";; Fetching module ~A from ~A remote ~A, ~A~%" (name module) (vcs-type best-remote) (name best-remote) (url best-remote module))
    (update-module-using-remote module best-remote locality)
    (report t ";; Done fetching module ~A~%" (name module))))

;; system vocabulary
(defun make-unwanted-missing (name) (cons name (cons nil nil)))
(defun make-wanted-missing (name) (cons name (cons :wanted nil)))
(defun make-unwanted-present (name) (cons name (cons nil :present)))
;; module vocabulary
(defun make-notprocessing-undone (name) (cons name (cons nil nil)))
(defun make-processing-undone (name) (cons name (cons :processing nil)))

(defun module-dependencies (module &optional (locality (gate *self*)) (system-type *default-system-type*) complete system-vocabulary)
  (let ((syspath (make-hash-table :test #'equal)))
    (labels ((syspath (name) (gethash name syspath))
             (set-syspath (name value) (setf (gethash name syspath) value))
             (subj (c) (car c))
             (cell (c) (cdr c))
             ((setf celldr) (v c) (setf (cddr c) v))
             (entry (vocab subj &optional (test #'eq))
               (assoc subj vocab :test test))
             (vocab-maybe-add-wanted-missing (vocab subj &optional (test #'eq))
               (if (entry vocab subj test)
                   vocab
                   (cons (make-wanted-missing subj) vocab)))
             (syscell-wanted-missingp (c) (and (car c) (not (cdr c))))
             (next-unsatisfied-system (system-vocabulary)
               (subj (find-if #'syscell-wanted-missingp system-vocabulary :key #'cell)))
             ((setf system-satisfiedp) (val system-vocabulary sysname)
               (setf (celldr (entry system-vocabulary sysname #'string=)) val))
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
                     (dolist (newdep (append (mapcar (compose #'string #'name) local-newdeps)
                                             new-missing))
                       (if-let ((cell (cell (entry system-vocabulary newdep #'string=))))
                         (setf (car cell) :wanted)
                         (setf extended-system-vocabulary (cons (make-wanted-missing newdep) extended-system-vocabulary))))
                     ;; NOTE: on module boundaries we lose precise system dependency names
                     (values (append (mapcar (compose #'name #'system-module) othermodule-newdeps) modules)
                             extended-system-vocabulary)))))
             (satisfy-next-system (module system-type &optional modules system-vocabulary)
               "Given a MODULE and a list of its REQUIRED systems, pick one and try to handle
              the fallout. Return the modified sets of known REQUIRED systems, MODULES and
              MISSING unknown systems, returning them as multiple values."
               (declare (special *syspath*))
               (if-let ((name (next-unsatisfied-system system-vocabulary)))
                 (if-let ((system (system name :if-does-not-exist :continue)))
                   (let* ((path (or (syspath name)
                                    (error "~@<Internal invariant violation during dependency resolution: failed to find system ~S among syspathed ~S~:@>~%"
                                           name (hash-table-keys *syspath*))))
                          (actual-type (system-definition-type path)))
                     (unless (subtypep system-type actual-type)
                       (error "~@<While operating in ~A mode, encountered an ~A at ~S.~:@>" system-type actual-type path))
                     (unless (typep system system-type)
                       (error "~@<While operating in ~A mode, encountered an ~A.~:@>" system-type (type-of system)))
                     (setf (system-satisfiedp system-vocabulary name) :present) ; made loadable, hiddens uncovered, deps about to be added
                     (add-system-dependencies module system modules system-vocabulary))
                   (if-let ((cell (cell (entry system-vocabulary name #'string=))))
                     (progn
                       (setf (car cell) :wanted)
                       (values modules (cons cell (remove cell system-vocabulary))))
                     (error "~@<Encountered a non-local dependency on an unknown system ~A.~:@>" name)))
                 (values modules system-vocabulary)))
             (add-visible-system (module name path type vocabulary known-visible &optional (actual-type (system-definition-type path)))
               (unless (eq type actual-type)
                 (error "~@<While operating in ~A mode, encountered an ~A at ~S.~:@>" type actual-type path))
               (let ((system (or (lret ((system (system name :if-does-not-exist :continue)))
                                   (when system
                                     (unless (typep system type)
                                       (error "~@<During dependency resolution: asked for a system ~S of type ~S, got one of type ~S~:@>"
                                              type name (type-of system)))))
                                 (register-new-system (intern (string-upcase name)) path type module))))
                 (set-syspath name path)
                 (ensure-system-loadable system path t locality)
                 (append vocabulary
                         (when (typep system 'asdf-system)
                           ;; A hidden system is a system definition residing in a file named differently from main system's name.
                           ;; Find them.
                           (iter (for hidden-system-name in (set-difference (asdf-hidden-system-names system) known-visible :test #'equal))
                                 (set-syspath hidden-system-name path)
                                 (let ((hidden-system (or (system hidden-system-name :if-does-not-exist :continue)
                                                          (register-new-system (intern hidden-system-name) path type module))))
                                   (ensure-system-loadable hidden-system path nil locality))
                                 (collect (if complete
                                              (make-wanted-missing hidden-system-name)
                                              (make-unwanted-missing hidden-system-name)))))))))
      (let* ((all-sysfiles (module-system-definitions module system-type locality))
             (main-sysfile (central-module-system-definition-pathname module system-type locality))
             (other-sysfiles (remove main-sysfile all-sysfiles)))
        ;; This doesn't deal with other modules providing same systems. Will silently break.
        (let* ((required-sysfiles (xform main-sysfile (curry #'cons main-sysfile) (when complete other-sysfiles)))
               (also-sysfiles (unless complete other-sysfiles))
               (required-names (mapcar (curry #'system-definition-name system-type) required-sysfiles))
               (also-names (mapcar (curry #'system-definition-name system-type) also-sysfiles))
               (all-names (append required-names also-names))
               (extended-system-vocabulary (append (mapcar #'make-wanted-missing required-names)
                                                   (mapcar #'make-unwanted-missing also-names)
                                                   system-vocabulary)))
          (iter (for sysfile in (append required-sysfiles also-sysfiles))
                (for name in (append required-names also-names))
                (setf extended-system-vocabulary (add-visible-system module name sysfile system-type extended-system-vocabulary all-names)))
          (iter (with modules)
                ;; Progress is made because NEXT-UNSATISFIED-SYSTEM proceeds from the head of the vocabulary,
                ;; where we've appended our required systems.
                (for (values modules-new new-extended-system-vocabulary) = (satisfy-next-system module system-type modules extended-system-vocabulary))
                (setf (values modules extended-system-vocabulary) (values modules-new new-extended-system-vocabulary))
                (when (not (iter (for (name wanted . satisfied) in extended-system-vocabulary)
                                 (finding name such-that (and wanted (not satisfied)))))
                  (return (values (remove-duplicates modules) extended-system-vocabulary)))))))))

(defgeneric satisfy-module (name &optional locality system-type complete skip-present module-vocabulary system-vocabulary)
  (:method ((name symbol) &optional (locality (gate *self*)) (system-type *default-system-type*) complete skip-present module-vocabulary system-vocabulary)
    (let ((cell (or (cdr (assoc name module-vocabulary))
                    (cdr (first (push (make-notprocessing-undone name) module-vocabulary)))))  ; extend the vocabulary
          (module (module name)))
      (cond
        ((car cell)
         (when (and skip-present (module-locally-present-p module locality))
           (setf (cdr cell) :done))
         (values module-vocabulary system-vocabulary))
        (t
         (setf (car cell) :processing)
         (update-module module locality)
         (multiple-value-bind (module-deps new-system-vocabulary) (module-dependencies module locality system-type complete system-vocabulary)
           (let* ((new-deps-from-this-module (remove-if (rcurry #'assoc module-vocabulary) module-deps))
                  (new-module-vocabulary (append module-vocabulary (mapcar #'make-notprocessing-undone new-deps-from-this-module))))
             (report t "~&~@<;; ~@;~S,~:[ no further dependencies~; added ~:*~A,~]~:@>~%" name new-deps-from-this-module)
             (multiple-value-prog1
                 (if new-deps-from-this-module
                     (satisfy-modules new-deps-from-this-module locality system-type complete skip-present new-module-vocabulary new-system-vocabulary)
                     (values new-module-vocabulary new-system-vocabulary))
               (report t "~&~@<;; ~@;Done processing ~S, ~D left~:@>~%" name (count-if-not #'cddr new-module-vocabulary))
               (setf (cdr cell) :done)))))))))

(defun satisfy-modules (module-names locality system-type complete skip-present module-vocabulary system-vocabulary &optional toplevel)
  (iter (for module-name in module-names)
        (for (values updated-module-vocabulary updated-system-vocabulary) = (satisfy-module module-name locality system-type complete skip-present module-vocabulary system-vocabulary))
        (setf (values module-vocabulary system-vocabulary) (values updated-module-vocabulary updated-system-vocabulary))
        (finally
         (when-let ((undone (and toplevel (mapcar #'car (remove-if #'cddr module-vocabulary)))))
           (report t "WARNING: after all gyrations following modules were left unsatisfied:~{ ~S~}~%" undone))
         (return (values module-vocabulary system-vocabulary)))))

(defun desire (desires &key complete skip-present (seal t))
  "Satisfy module DESIRES and return the list of names of updated modules.

Desire satisfaction means:
   - for specified missing modules, retrieval,
   - for specified present modules, update, unless SKIP-PRESENT is
     non-nil,

In all cases, systems present in the set union of specified and
depended upon modules are ensured to be loadable. See also the
documentation of the COMPLETE keyword.

When individual desires are symbols, they are interpreted as module names.
When they are lists, their first element is interpreted as the source
distributor, from which the rest of the list is supposed to be fetched.
These two forms can be mixed in the list of desires.

Defined keywords:
   - SKIP-PRESENT - whether to skip updating specified modules which are 
     already present, defaults to nil,
   - SEAL - whether to commit any definition changes, and,
   - COMPLETE - whether to obtain all modules' systems, even those not
     part of main module systems' complete dependency graphs."
  (let* ((interpreted-desires (mapcar (curry #'xform-if-not #'consp (lambda (m) (list (name (module-best-distributor m)) m))) desires)))
    (iter (for (distributor-name . modules) in interpreted-desires)
          (for distributor = (distributor distributor-name))
          (when-let ((missing (remove-if (curry #'distributor-module-enabled-remote distributor) modules)))
            (error "~@<Distributor ~S does not provide following modules: ~S~:@>" distributor missing)))
    (let ((*desires* (substitute-desires *desires* (remove-if-not #'consp desires))) ; currently unused
          (desired-module-names (mapcan #'rest interpreted-desires))
          (module-vocabulary nil)
          (system-vocabulary (mapcar #'make-unwanted-present *implementation-provided-systems*)))
      (report t "; Satisfying desire for ~D module~:*~P:~%" (length desired-module-names))
      (satisfy-modules desired-module-names (gate *self*) *default-system-type* complete skip-present module-vocabulary system-vocabulary :sure-as-hell))
    (when (and *unsaved-definition-changes-p* seal)
      (report t "; Definitions modified and sealing was requested, committing changes.~%")
      (save-current-definitions :seal t))
    (report t "; All done.~%")
    t))

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
