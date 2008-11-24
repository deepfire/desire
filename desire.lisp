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

;; (defun clone (to from)
;;   (declare (type local-git-repository to) (type remote-git-repository from))
;;   (with-changed-directory (namestring (ensure-directories-exist (path to)))
;;     (git "init"))
;;   (setf (repo-var to 'remote.origin.url) (url from)
;;         (repo-var to 'remote.origin.fetch) "+refs/heads/*:refs/heads/*")
;;   #| sync local/remote HEADs |#)

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

;; (defgeneric fetch (to from))

;; (define-condition repository-fetch-failed (simple-warning)
;;   ((from :accessor cond-from :type repository :initarg :from)
;;    (to :accessor cond-to :type repository :initarg :to))
;;   (:report (lambda (cond stream)
;;              (format stream "~@<failed to fetch from ~S to ~S~:@>" (cond-from cond) (cond-to cond)))))

;;;   (multiple-value-bind (primary backups) (url from)
;;;     (with-condition-restart-binding ((external-program-failure continue))
;;;       (iter (for url in (cons primary backups))
;;;             (restart-bind ((continue (lambda (cond) (declare (ignore cond)) (format t "failed to fetch from ~S~%" url) (next-iteration))
;;;                              :report-function (lambda (stream) (format stream "Try fetching from other backup URLs."))))
;;;               (call-next-method o :url url)
;;;               (return-from fetch-module t))
;;;             (finally (warn 'module-fetch-failed :module o)))))

;; (defmethod fetch :around (to from)
;;   (format t "fetching: ~S => ~S~%" from to)
;;   (with-condition-restart-binding ((external-program-failure resignal))
;;     (restart-bind ((resignal (lambda (cond) (declare (ignore cond)) (warn 'repository-fetch-failed :from from :to to) (return-from fetch))))
;;       (call-next-method))))

;; (defgeneric update-configuration (repository)
;;   (:method ((o repository)) t)
;;   (:method ((o local-git-repository))
;;     (let ((gitignore (make-pathname :directory (pathname-directory (path o)) :name ".gitignore")))
;;       (unless (probe-file gitignore)
;;         (with-open-file (s gitignore :direction :output :if-does-not-exist :create)
;;           (format s ".gitignore~%*~~~%*.o~%*.fasl~%"))))))

;;; How do we name localities?
;;; define-master-locality (proto) git->hostname, *->hostname-*
;;; define-locality (proto name)

(defmethod fetch ((to git-locality) (from git-locality) module &aux (name (down-case-name module)))
  (let ((from-repo-dir (subdirectory (locality-path from) name))
        (to-repo-dir (subdirectory (locality-path to) name)))
    (unless (directory-exists-p to-repo-dir)
      (within-directory (dir to-repo-dir :if-exists :error :if-does-not-exist :create)
        (git "init-db")
        (git "remote" "add" (down-case-name from) (namestring from-repo-dir))))
    (within-directory (dir to-repo-dir)
      (git "fetch" (down-case-name from)))))

(defmethod fetch ((locality git-locality) (remote git-remote) module &aux (name (down-case-name module)))
  (let ((repo-dir (subdirectory (locality-path locality) name)))
    (unless (directory-exists-p repo-dir)
      (within-directory (dir repo-dir :if-exists :error :if-does-not-exist :create)
        (git "init-db")))
    (ensure-module-gitremote module remote)
    (within-directory (dir repo-dir)
      (git "fetch" (down-case-name remote)))))

(defmethod fetch ((git-locality git-locality) (remote hg-http-remote) module &aux (name (down-case-name module)))
  (let ((hg-repo-dir (subdirectory (locality-path (master 'hg)) name))
        (git-repo-dir (subdirectory (locality-path git-locality) name)))
    ))

(defmethod fetch ((git-locality git-locality) (remote darcs-http-remote) module &aux (name (down-case-name module)))
  (let ((darcs-repo-dir (subdirectory (locality-path (master 'darcs)) name))
        (git-repo-dir (subdirectory (locality-path git-locality) name))
        (url (url remote module)))
    (if (directory-exists-p darcs-repo-dir)
        (darcs "pull" "--all" "--repodir" (namestring darcs-repo-dir) url)
        (darcs "get" url (namestring darcs-repo-dir)))
    (within-directory (git-repo-dir git-repo-dir :if-does-not-exist :create)
      (darcs-to-git (namestring darcs-repo-dir)))
    (when (module-bare-p module git-locality)
      (setf (module-bare-p module git-locality) nil))))

(defmethod fetch ((git-locality git-locality) (remote cvs-rsync-remote) module &aux (name (down-case-name module)))
  (let* ((cvs-locality (master 'cvs))
         (cvs-repo-dir (subdirectory (locality-path cvs-locality) name))
         (git-repo-dir (subdirectory (locality-path git-locality) name)))
    (rsync "-ravPz" (format nil "~A/cvsroot/" (url remote module)) (namestring cvs-repo-dir))
    (with-output-to-file (stream (subfile cvs-repo-dir "CVSROOT" "config"))
      (format stream "LockDir=~A~%" (namestring (cvs-locality-lock-path cvs-locality))))
    (git-cvsimport "-v" "-C" (namestring git-repo-dir) "-d" (format nil ":local:~A" (string-right-trim "/" (namestring cvs-repo-dir))) name ;; *REPO-CVS-MODULE*
                   )))

(defmethod fetch ((git-locality git-locality) (remote svn-rsync-remote) module &aux (name (down-case-name module)))
  (let ((svn-repo-dir (subdirectory (locality-path (master 'svn)) name))
        (git-repo-dir (subdirectory (locality-path git-locality) name)))
    (rsync "-ravPz" (url remote module) (namestring svn-repo-dir))
    (unless (directory-exists-p git-repo-dir)
      (within-directory (git-repo-dir git-repo-dir :if-does-not-exist :create :if-exists :error)
        (git-svn "init" (format nil "file:/~A" (namestring svn-repo-dir))))) ;; gratuitious SVN complication
    (within-directory (git-repo-dir git-repo-dir)
      (git-svn "fetch"))))

(defun satisfy-single-module-desire (module &optional (locality (master 'git)) &aux (module (coerce-to-module module)))
  "Satisfy the desire for MODULE."
  (multiple-value-call #'fetch (single-module-desire-satisfaction module locality) module))

(defun satisfy-module-desire (module &optional (locality (master 'git)) (loops (make-hash-table)) &aux (module (coerce-to-module module)))
  "Satisfy the desire for MODULE, and its dependencies."
  (sb-posix:putenv "PAGER=cat")
  (sb-posix:putenv "GIT_PAGER=cat")
  (when (gethash (name module) loops)
    (return-from satisfy-module-desire))
  (setf (gethash (name module) loops) t)
  (mapcar (rcurry #'satisfy-module-desire locality loops) (module-dependencies module))
  (satisfy-single-module-desire module locality))

(defun desire (&rest desires)
  "Satisfy module DESIRES and return the list of names of updated modules.

   When individual parameters are symbols, they are interpreted as module
   names, and are intepreted in the context of the global *DESIRES*.

   When they are lists, their first element is interpreted as the source
   distributor, from which the rest of the list is supposed to be imported.

   These two forms can be mixed."
  (let* ((satisfaction (apply #'desire-satisfaction desires))
         (to-update (mapcar (compose #'name #'third) satisfaction)))
    (format t "Will try updating following modules:~{ ~S~}~%" to-update)
    (remove nil (mapcar #'and-p (mapcar (curry #'apply #'fetch) satisfaction) to-update))))

;; (defgeneric fetch-desired-p (repo)
;;   (:method ((o derived-repository))
;;     (or (not (probe-file (path o)))
;;         (etypecase (repo-master o)
;;           (local-repository (> (time-identity (repo-master o)) (time-identity o)))
;;           (remote-repository t)))))

;; (defgeneric fetch-chain (repo)
;;   (:method (o) t)
;;   (:method ((o derived-repository))
;;     (fetch-chain (repo-master o))
;;     (if (fetch-desired-p o)
;;         (fetch o (repo-master o))
;;         (format t "fetch not desired for ~S~%" o))))

;; (defun update-single (os &aux (o (coerce-to-module os)))
;;   (fetch-chain (module-master-repository o))
;;   (ensure-loadable o))

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

;; (defun desire (os &key skip-loadable &aux (o (coerce-to-module os)))
;;   (update o :skip-loadable skip-loadable)
;;   (load-system o))

;; (defun init (&key (runcontrol (make-pathname :directory (pathname-directory (user-homedir-pathname)) :name ".desirerc")) (try-load-clung t))
;;   (format t "loading user run control file ~S~%" runcontrol)
;;   (let ((*package* (find-package :desire)))
;;     (when (probe-file runcontrol)
;;       (load runcontrol)))
;; ;;   (when (and try-load-clung (loadable-p (module 'clung)))
;; ;;     (require :clung))
;;   )
