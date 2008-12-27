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

(defvar *register-happy-matches* t
  "Whether or not to define previously undefined systems when they are found,
   if they are also depended upon by other systems.")

(defvar *register-all-martians* nil
  "Whether or not to define previously undefined systems when they are found,
   even if they are not depended upon by other systems.")

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
;;;     (with-condition-restart-binding ((external-program-failure continue))
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

(define-reported-condition repository-not-clean-during-fetch (repository-error external-program-failure) ()
  (:report (locality module)
           "~@<repository for ~S in ~S has uncommitted changes during fetch~:@>" module locality))

(defgeneric fetch-remote (locality remote module))

(defmethod fetch-remote ((locality git-locality) (remote git) module)
  (let ((repo-dir (module-path module locality)))
    (unless (directory-exists-p repo-dir)
      (within-directory (dir repo-dir :if-exists :error :if-does-not-exist :create)
        (git "init-db")))
    (maybe-within-directory repo-dir
      (ensure-gitremote (name remote) (url remote module))
      (git "fetch" (down-case-name remote))
      (ensure-master-branch-from-remote :remote-name (name remote)))))

(defmethod fetch-remote ((git-locality git-locality) (remote darcs-http-remote) module)
  (let ((darcs-repo-dir (module-path module (master 'darcs)))
        (url (url remote module)))
    (if (directory-exists-p darcs-repo-dir)
        (darcs "pull" "--all" "--repodir" (namestring darcs-repo-dir) url)
        (darcs "get" url (namestring darcs-repo-dir)))))

(defmethod fetch-remote ((git-locality git-locality) (remote cvs-rsync-remote) module)
  (let* ((cvs-locality (master 'cvs))
         (cvs-repo-dir (module-path module cvs-locality)))
    (rsync "-ravPz" (format nil "~A/cvsroot/" (url remote module)) (namestring cvs-repo-dir))))

(defmethod fetch-remote ((git-locality git-locality) (remote svn-rsync-remote) module)
  (let ((svn-repo-dir (module-path module (master 'svn))))
    (rsync "-ravPz" (url remote module) (namestring svn-repo-dir))))

(defmethod fetch-remote :around (locality remote module)
  (with-error-resignaling (external-program-failure
                           ((cond) 'fetch-failure :remote remote :module module :execution-error (format nil "~A" cond)))
    (call-next-method)))

(defmethod fetch :around (locality remote module)
  (with-condition-printing (t fetch-failure)
    (restart-bind ((retry (lambda () 
                            (maybe-within-directory (module-path module locality)
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

(defmethod fetch ((git-locality git-locality) (remote hg-http-remote) module)
  (let ((hg-repo-dir (module-path module (master 'hg)))
        (git-repo-dir (module-path module git-locality)))
    ))

(defmethod fetch ((git-locality git-locality) (remote darcs-http-remote) module)
  (let ((darcs-repo-dir (module-path module (master 'darcs)))
        (git-repo-dir (module-path module git-locality)))
    (purge-binaries git-repo-dir)
    (within-directory (git-repo-dir git-repo-dir :if-does-not-exist :create)
      (darcs-to-git (namestring darcs-repo-dir)))
    (when (repository-bare-p git-repo-dir)
      (setf (repository-bare-p git-repo-dir) nil))))

(defmethod fetch ((git-locality git-locality) (remote cvs-rsync-remote) module &aux (name (down-case-name module)))
  (let* ((cvs-locality (master 'cvs))
         (cvs-repo-dir (module-path module cvs-locality))
         (git-repo-dir (module-path module git-locality)))
    (with-output-to-file (stream (subfile* cvs-repo-dir "CVSROOT" "config") :if-exists :supersede)
      (format stream "LockDir=~A~%" (namestring (cvs-locality-lock-path cvs-locality))))
    (with-exit-code-to-error-translation ((9 'repository-not-clean-during-fetch :module module :locality git-locality))
      (git-cvsimport "-v" "-C" (namestring git-repo-dir) "-d" (format nil ":local:~A" (string-right-trim "/" (namestring cvs-repo-dir))) name))))

(defmethod fetch ((git-locality git-locality) (remote svn-rsync-remote) module)
  (let ((svn-repo-dir (module-path module (master 'svn)))
        (git-repo-dir (module-path module git-locality)))
    (unless (directory-exists-p git-repo-dir)
      (within-directory (git-repo-dir git-repo-dir :if-does-not-exist :create :if-exists :error)
        (git-svn "init" (format nil "file://~A" (namestring svn-repo-dir))))) ;; gratuitious SVN complication
    (within-directory (git-repo-dir git-repo-dir)
      (git-svn "fetch"))))

(defmethod fetch :after ((git-locality git-locality) remote module)
  (setf (repository-world-readable-p (module-path module git-locality)) *default-world-readable*))

(defun fetch-anyway (module-name)
  (cons module-name nil))

(defun desire-do-one-step (desires skip-present missing martians)
  (declare (special *departed-from-definitions-p* *locality*) (optimize (debug 3)))
  (labels ((syspath (name) (declare (special *syspath*)) (gethash name *syspath*))
           (set-syspath (name value) (declare (special *syspath*)) (setf (gethash name *syspath*) value))
           (next-unsatisfied-module ()
             (car (find nil desires :key #'cdr)))
           (fetch-if-missing (module-name)
             (cons module-name (not (module-present-p (module module-name) *locality*))))
           ((setf desire-satisfied) (val m) (setf (cdr (assoc m desires)) val))
           (maybe-register-martian (martian type module missing)
             (when (or *register-all-martians*
                       (and *register-happy-matches* (find martian missing :test #'string=)))
               (report t ";; Registered a previously unknown system ~A~%" martian)
               (setf *departed-from-definitions-p* t)
               (make-instance type :name martian :module module)))
           (add-system-dependencies (system system-names modules missing martians)
             (multiple-value-bind (new-sysdeps new-missing) (system-dependencies system)
               (let* ((total-missing (union new-missing missing :test #'string=))
                      (happy-matches (when *register-happy-matches*
                                       (remove-if-not (rcurry #'member total-missing :test #'string=) martians))))
                 (values (append happy-matches system-names) ;; Let them wash in the next tide.
                         (append (mapcar (compose #'name #'system-module) new-sysdeps) modules)
                         (set-difference total-missing happy-matches :test #'string=)
                         (set-difference martians happy-matches :test #'string=)))))
           (dependencies-add-system-def (module system-names &optional modules missing martians)
             (if-let ((system-name (first system-names)))
               (let ((system-type (interpret-system-pathname-type (syspath system-name))))
                 (if-let ((system (or (lret ((system (system system-name :if-does-not-exist :continue)))
                                        (when system
                                          (unless (typep system system-type)
                                            (error "~@<During dependency resolution: asked for a system ~S of type ~S, got one of type ~S~:@>"
                                                   system-type system-name (type-of system)))))
                                      (maybe-register-martian (intern system-name) system-type module missing)))) ;; A happy match?
                   (progn (ensure-system-loadable system (syspath system-name) *locality*)
                          (add-system-dependencies system (rest system-names) modules (remove system-name missing :test #'string=) martians))
                   (values (rest system-names) modules missing (adjoin system-name martians :test #'string=))))
               (values nil modules missing martians)))
           (module-dependencies (m missing martians &aux (sysfiles (system-definitions (module-path m *locality*) 'asdf-system)))
             (let ((sysnames (mapcar #'system-pathname-name sysfiles)))
               (mapc #'set-syspath sysnames sysfiles)
               (iter (with modules)
                     (for (values sysnames-new modules-new missing-new martians-new) = (dependencies-add-system-def m sysnames modules missing martians))
                     (setf (values sysnames modules missing martians) (values sysnames-new modules-new missing-new martians-new))
                     (unless sysnames
                       (return (values (remove-duplicates modules) missing martians)))))))
    (if-let ((an-unsatisfied-name (next-unsatisfied-module)))
      (let ((an-unsatisfied-module (module an-unsatisfied-name)))
        (fetch *locality* (module-remote an-unsatisfied-module) an-unsatisfied-module)
        (ensure-module-systems-loadable an-unsatisfied-module *locality*) ;; this either succeeds, or errors
        (setf (desire-satisfied an-unsatisfied-name) t)
        (multiple-value-bind (module-deps missing martians) (module-dependencies an-unsatisfied-module missing martians)
          (let ((added-deps-from-this-module (remove-if (rcurry #'assoc desires) module-deps)))
            (appendf desires (mapcar (if skip-present #'fetch-if-missing #'fetch-anyway) added-deps-from-this-module))
            (report t "~&~@<;; ~@;~S,~:[~; added ~:*~S,~] ~D left~:@>~%"
                    an-unsatisfied-name added-deps-from-this-module (count-if-not #'cdr desires))
            (finish-output))
          (desire-do-one-step desires skip-present missing martians)))
      (values t missing martians))))

(defun desire (desires &key skip-present)
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
  (let* ((interpreted-desires (mapcar (curry #'xform-if-not #'consp (lambda (m) (list (name (module-distributor m)) m))) desires)))
    (iter (for (distributor-name . modules) in interpreted-desires)
          (for distributor = (distributor distributor-name))
          (when-let ((missing (remove-if (curry #'distributor-provides-module-p distributor) modules)))
            (error "~@<Distributor ~S does not provide following modules: ~S~:@>" distributor missing)))
    (let* ((*desires* (substitute-desires *desires* (remove-if-not #'consp desires)))
           *departed-from-definitions-p*
           (*locality* (master 'git))
           (*syspath* (make-hash-table :test #'equal))
           (desired-list (mapcan #'rest interpreted-desires)))
      (declare (special *departed-from-definitions-p* *locality* *syspath*))
      (report t "; Satisfying desire for ~D module~:*~P:~%" (length desired-list))
      (multiple-value-bind (success missing martians) (desire-do-one-step (mapcar #'fetch-anyway desired-list) skip-present nil nil)
        (declare (ignore success))
        (when-let ((missing (set-difference missing *implementation-provided-systems* :test #'string=)))
          (report t "; Required systems missing from definitions:~{ ~A~}~%" missing))
        (when martians
          (report t "; Unexpected systems:~{ ~A~}~%" martians)))
      (when *departed-from-definitions-p*
        (report t "; Definitions modified, committing changes.~%")
        (save-current-definitions :seal-p t))
      (report t "; All done.~%"))))

(defun desire* (&rest desires)
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
