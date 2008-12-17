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

(defgeneric fetch-remote (locality remote module))

(defmethod fetch-remote ((locality git-locality) (remote git) module)
  (let ((repo-dir (module-path module locality)))
    (unless (directory-exists-p repo-dir)
      (within-directory (dir repo-dir :if-exists :error :if-does-not-exist :create)
        (git "init-db")))
    (ensure-module-gitremote module remote)
    (within-directory (dir repo-dir)
      (git "fetch" (down-case-name remote)))
    (ensure-module-master-branch-from-remote module locality)))

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
                            (with-slots (locality module) *debug-condition*
                              (within-module-repository (dir module locality)
                                (git "gui")))
                            (invoke-restart (find-restart 'retry)))
                     :test-function (lambda (cond) (typep cond 'repository-not-clean-during-fetch))
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
    (purge-module-binaries module)
    (within-directory (git-repo-dir git-repo-dir :if-does-not-exist :create)
      (darcs-to-git (namestring darcs-repo-dir)))
    (when (module-bare-p module git-locality)
      (setf (module-bare-p module git-locality) nil))))

(defmethod fetch ((git-locality git-locality) (remote cvs-rsync-remote) module &aux (name (down-case-name module)))
  (let* ((cvs-locality (master 'cvs))
         (cvs-repo-dir (module-path module cvs-locality))
         (git-repo-dir (module-path module git-locality)))
    (with-output-to-file (stream (subfile* cvs-repo-dir "CVSROOT" "config"))
      (format stream "LockDir=~A~%" (namestring (cvs-locality-lock-path cvs-locality))))
    (with-exit-code-to-error-translation ((9 'repository-not-clean-during-fetch :module module :locality git-locality))
      (git-cvsimport "-v" "-C" (namestring git-repo-dir) "-d" (format nil ":local:~A" (string-right-trim "/" (namestring cvs-repo-dir))) name))))

(defmethod fetch ((git-locality git-locality) (remote svn-rsync-remote) module)
  (let ((svn-repo-dir (module-path module (master 'svn)))
        (git-repo-dir (module-path module git-locality)))
    (unless (directory-exists-p git-repo-dir)
      (within-directory (git-repo-dir git-repo-dir :if-does-not-exist :create :if-exists :error)
        (git-svn "init" (format nil "file:/~A" (namestring svn-repo-dir))))) ;; gratuitious SVN complication
    (within-directory (git-repo-dir git-repo-dir)
      (git-svn "fetch"))))

(defmethod fetch :after ((git-locality git-locality) remote module)
  (setf (module-world-readable-p module) *default-world-readable*))


(defgeneric system-dependencies (s)
  (:method ((s mudballs-system)))
  (:method ((s asdf-system))
    (iter (for depname in (cdr (assoc 'asdf:load-op (asdf:component-depends-on 'asdf:load-op (asdf:find-system (name s))))))
          (if-let ((depsystem (system depname :if-does-not-exist :continue)))
            (collect depsystem into known-systems)
            (collect depname into unknown-names))
          (finally (return (values known-systems unknown-names))))))

(defun fetch-anyway (module-name)
  (cons module-name nil))

(defun desire-do-one-step (desires skip-present &optional unknown-sys-dep-acc unknown-sys-enc-acc)
  (declare (special *locality*) (optimize (debug 3)))
  (flet ((next-unsatisfied-module ()
           (car (find nil desires :key #'cdr)))
         (fetch-if-missing (module-name)
           (cons module-name (not (module-present-p (module module-name) *locality*))))
         ((setf desire-satisfied) (val m) (setf (cdr (assoc m desires)) val))
         (module-dependencies (m)
           (iter (for system-file in (directory (merge-pathnames (make-pathname :directory (pathname-directory (module-path m)) :name :wild :type "asd")
                                                                 (make-pathname :directory '(:relative :wild-inferiors)))))
                 (if-let* ((system (system (pathname-name system-file) :if-does-not-exist :continue)))
                          (multiple-value-bind (sysdeps unknown-sysdeps) (system-dependencies system)
                            (ensure-system-loadable system)
                            (when unknown-sysdeps
                              (appending unknown-sysdeps into unknown-systems-depended-upon))
                            (appending
                             (remove-duplicates (mapcar (compose #'name #'system-module) sysdeps))
                             into module-dependencies))
                          (collect (intern (string-upcase (pathname-name system-file))) into unknown-system-encounters))
                 (finally (return (values module-dependencies unknown-systems-depended-upon unknown-system-encounters))))))
    (if-let ((an-unsatisfied-name (next-unsatisfied-module)))
      (let ((an-unsatisfied-module (module an-unsatisfied-name)))
        (fetch *locality* (module-remote an-unsatisfied-module) an-unsatisfied-module)
        (ensure-module-systems-loadable an-unsatisfied-module *locality*) ;; this either succeeds, or errors
        (setf (desire-satisfied an-unsatisfied-name) t)
        (multiple-value-bind (module-deps unknown-sys-dep unknown-sys-enc) (module-dependencies an-unsatisfied-module)
          (let ((added-deps-from-this-module (remove-if (rcurry #'assoc desires) module-deps)))
            (appendf desires (mapcar (if skip-present #'fetch-if-missing #'fetch-anyway) added-deps-from-this-module))
            (format t "~&~S,~:[~; added ~:*~S,~] ~D left~%"
                    an-unsatisfied-name added-deps-from-this-module (count-if-not #'cdr desires))
            (finish-output))
          (desire-do-one-step desires skip-present (union unknown-sys-dep unknown-sys-dep-acc) (union unknown-sys-enc unknown-sys-enc-acc))))
      (values t unknown-sys-dep-acc unknown-sys-enc-acc))))

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
           (*locality* (master 'git))
           (desired-list (mapcan #'rest interpreted-desires)))
      (declare (special *locality*))
      (format t "Satisfying desire for ~D module~:*~P:~%" (length desired-list)) (finish-output)
      (desire-do-one-step (mapcar #'fetch-anyway desired-list) skip-present)
      (format t "All done.~%") (finish-output))))

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
