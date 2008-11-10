;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLING; Base: 10 -*-
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

(in-package :cling)

(defun system-spec (o)
  (cons (name o) (when-let ((rel (system-relativity o))) (list :relativity rel))))

(defun define-module-systems (module system-specs)
  (appendf (module-systems module)
           (mapcar (compose (curry #'apply (curry #'make-instance 'system :module module :name)) #'ensure-list) system-specs)))

(defmacro defdistributor (dist-name &rest definitions)
  (flet ((collate (method)
           (case method
             (git (values 'git-native-remote 'git))
             (git-http (values 'git-http-remote 'http))
             (darcs (values 'darcs-http-remote 'http))
             (cvs (values 'cvs-rsync-remote 'rsync))
             (svn (values 'svn-rsync-remote 'rsync)))))
    (let* ((path-forms (rest (find :url-schemas definitions :key #'first)))
           (mod-specs (iter (for (type . mod-specs) in (rest (find :modules definitions :key #'first)))
                            (collect (cons type (iter (for mod-spec in mod-specs)
                                                      (appending
                                                       (destructuring-bind (umbrella-name . module-specs)
                                                           (if (consp mod-spec) mod-spec (list mod-spec mod-spec))
                                                         (iter (for module-spec in module-specs)
                                                               (collect (list* umbrella-name (ensure-cons module-spec))))))))))))
      `(progn
         (unless (distributor ',dist-name :if-does-not-exist :continue)
           (make-instance 'distributor :name ',dist-name))
         ,@(iter (for (type . module-specs) in mod-specs)
                 (multiple-value-bind (location-type form-spec-type) (collate type)
                   (destructuring-bind ((repovar) . body) (rest (find form-spec-type path-forms :key #'car))
                     (appending `((if-let* ((extension (find '((,repovar) ,@body) (distributor-remotes (distributor ',dist-name))
                                                            :test #'equal :key #'remote-path-form)))
                                           extension
                                           (make-instance ',location-type
                                                          :distributor (distributor ',dist-name)
                                                          :path-form '((,repovar) ,@body)
                                                          :path-fn (lambda (,repovar) (list ,@body))
                                                          :modules ',(mapcar #'cadr module-specs))))))))
         ,@(iter (for (umbrella modname . args) in (mappend #'rest mod-specs))
                 (destructuring-bind (&rest rest &key (systems (list modname)) &allow-other-keys) args
                   (when-let ((unhandled (remove-from-plist rest :systems)))
                     (warn "Unhandled module parameters: ~S~%" unhandled))
                   (appending (cons `(unless (module ',modname :if-does-not-exist :continue)
                                         (make-instance 'module :name ',modname :umbrella ',umbrella))
                                    (iter (for system in systems)
                                          (destructuring-bind (name &rest rest &key relativity &allow-other-keys) (ensure-cons system)
                                            (collect `(make-instance 'system :name ',name :module (module ',modname)
                                                                     ,@(when relativity `(:relativity ',relativity))
                                                                     ,@(remove-from-plist rest :relativity)))))))))))))

(defmacro define-module-dependencies (&body body)
  `(iter (for (module-name . dependencies) in '(,@body))
         (for module = (module module-name))
         (setf (nonleaf module-name) module)
         (dolist (dep dependencies)
           (setf (leaf dep) (module dep))
           (depend module (module dep)))))

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

;; (defmethod fetch :around ((to git-repository) from)
;;   (let ((preexisting (probe-file (path to))))
;;     (call-next-method)
;;     (unless preexisting ;; new shiny repository
;;       (when *default-world-readable*
;;         (setf (world-readable-p to) t)))
;;     (update-configuration to)))

;; (defmethod fetch ((to local-git-repository) (from remote-git-repository) &aux (path (namestring (path to))) (url (url from)))
;;   (if (probe-file path)
;;       (with-changed-directory path
;;         (git "pull"))
;;       (with-changed-directory (repo-pool-root to)
;;         (git "clone" url))))

;; (defmethod fetch ((to local-darcs-repository) (from remote-darcs-repository) &aux (path (namestring (path to))) (url (url from)))
;;   (if (probe-file path)
;;       (darcs "pull" "--all" "--repodir" path url)
;;       (darcs "get" url path)))

;; (defmethod fetch ((to local-svn-repository) (from remote-svn-repository))
;;   (rsync "-ravPz" (url from) (namestring (path to))))

;; (defmethod fetch ((to local-cvs-repository) (from remote-cvs-repository))
;;   (rsync "-ravPz" (format nil "~Acvsroot/" (url from)) (namestring (path to))))

;; (defmethod fetch ((to local-git-repository) (from local-cvs-repository))
;;   (ensure-directories-exist (make-pathname :directory (append (pathname-directory (lockdir *perspective*)) (list (downstring (name (repo-module to)))))))
;;   (git-cvsimport "-v" "-C" (namestring (path to)) "-d" (format nil ":local:~A" (string-right-trim "/" (namestring (path from)))) (downstring (repo-cvs-module (repo-master from)))))

;; (defmethod fetch ((to local-git-repository) (from local-svn-repository) &aux (path (namestring (path to))))
;;   (unless (probe-file path)
;;     (ensure-directories-exist path)
;;     (with-changed-directory path
;;       (git-svn "init" (url from))))
;;   (with-changed-directory path
;;       (git-svn "fetch")))

;; (defmethod fetch ((to local-git-repository) (from local-darcs-repository))
;;   (ensure-directories-exist (namestring (path to)))
;;   (within-repository (to)
;;     (darcs-to-git (namestring (path from))))
;;   (when (repository-bare-p to)
;;     (setf (repository-bare-p to) nil)))

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

;; (defun cling (os &key skip-loadable &aux (o (coerce-to-module os)))
;;   (update o :skip-loadable skip-loadable)
;;   (load-system o))

;; (defun init (&key (runcontrol (make-pathname :directory (pathname-directory (user-homedir-pathname)) :name ".clingrc")) (try-load-clung t))
;;   (format t "loading user run control file ~S~%" runcontrol)
;;   (let ((*package* (find-package :cling)))
;;     (when (probe-file runcontrol)
;;       (load runcontrol)))
;; ;;   (when (and try-load-clung (loadable-p (module 'clung)))
;; ;;     (require :clung))
;;   )
