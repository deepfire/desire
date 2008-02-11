(in-package :cling)

(defun define-module (perspective name)
  (setf (module name) (make-instance 'module :perspective perspective :name name)))

(defun module-direct-dependencies (os &aux (o (coerce-to-module os)))
  (iter (for (nil (dep . nil)) in-hashtable (depsolver::%depobj-dep# o))
        (collect dep)))

(defun module-full-dependencies (os &optional stack &aux (o (coerce-to-module os)))
  (unless (member o stack)
    (cons o
          (iter (for dep in (module-direct-dependencies o))
                (unioning (module-full-dependencies dep (cons o stack)))))))

(defun repository-import-chain (type)
  (ecase type
    (local    (values '(site-origin-git-repository)))
    (git      (values '(site-derived-git-repository) 'remote-git-repository))
    (git-http (values '(site-derived-git-repository) 'remote-git-http-repository))
    (darcs    (values '(site-derived-git-repository local-darcs-repository) 'remote-darcs-repository))
    (svn      (values '(site-derived-git-repository local-svn-repository) 'remote-svn-repository))
    (cvs      (values '(site-derived-git-repository local-cvs-repository) 'remote-cvs-repository))))

(defun system-spec (o)
  (cons (name o) (when-let ((rel (system-relativity o))) (list :relativity rel))))

(defun define-module-systems (module system-specs)
  (let ((systems (iter (for system-spec in system-specs)
                       (destructuring-bind (name &key relativity) (ensure-list system-spec)
                         (collect (make-instance 'system :module module :name name :relativity relativity))))))
    (appendf (module-systems module) (mapcar #'(setf system) systems (mapcar #'name systems)))))

(defun define-module-repositories (module type &rest remote-initargs)
  (multiple-value-bind (locals remote) (repository-import-chain type)
    (let* ((local-repos (mapcar (rcurry #'make-instance :module module) locals))
           (remote-repo (mapcar (rcurry (curry #'apply #'make-instance) (list* :module module (alexandria::sans remote-initargs :systems))) (ensure-list remote)))
           (repos (append local-repos (ensure-list remote-repo))))
      (iter (for repo in repos)
            (setf (repo (repo-name repo)) repo))
      (iter (for (mirror master) on repos)
            (when (and master (typep mirror 'derived-repository))
              (setf (repo-master mirror) master)))
      (setf (module-repositories module) repos
            (module-master-repository module) (first local-repos))
      (define-module-systems module (getf remote-initargs :systems (list (name module)))))))

(defun derive-perspective (from type distributor &rest perspective-initargs)
  (lret ((new-perspective (apply #'make-instance type perspective-initargs)))
    (ensure-directories-exist (git-pool new-perspective))
    (let ((*perspective* new-perspective))
      (iter (for (name module) in-hashtable (modules from))
            (let* ((derived-module (make-instance 'module :perspective new-perspective :name (name module)))
                   (remote (make-instance 'remote-git-repository :module derived-module :distributor distributor))
                   (local (make-instance (perspective-master-repo-typemap type) :module derived-module :master remote)))
              (setf (module name) derived-module
                    (module-master-repository derived-module) local
                    (module-repositories derived-module) (list local remote)
                    (values (repo (repo-name local)) (repo (repo-name remote))) (values local remote))
              (define-module-systems derived-module (mapcar #'system-spec (module-systems module)))))
      (iter (for (name module) in-hashtable (modules from))
            (iter (for dep in (module-direct-dependencies module))
                  (depend (module name) (module (name dep))))))))

(defun switch-perspective (type distributor &rest perspective-initargs)
  (setf *perspective* (apply #'derive-perspective *perspective* type distributor perspective-initargs)))

(defmacro defdistributor (name &rest definitions)
  `(progn ,@(iter (for (op . op-body) in definitions)
                  (appending (ecase op
                               (:url-schemas
                                (iter (for (method (repo) . body) in op-body)
                                      (collect (with-gensyms (m d)
                                                 `(defmethod distributor-repo-url ((,m (eql ',method)) (,d (eql ',name)) ,repo)
                                                    (declare (ignorable ,m ,d))
                                                    (list ,@body))))))
                               (:modules
                                (with-gensyms (module module-prespec module-spec remote-spec type umbrella-name)
                                 `((iter (for (,module-spec ,type ,umbrella-name) in
                                              ',(iter (for (type . repo-specs) in op-body)
                                                      (appending (iter (for repo-spec in repo-specs)
                                                                       (appending (destructuring-bind (umbrella-name . module-specs) (if (consp repo-spec) repo-spec (list repo-spec repo-spec))
                                                                                    (iter (for module-preprespec in module-specs)
                                                                                          (for module-prespec = (ensure-cons module-preprespec))
                                                                                          (collect (list module-prespec type umbrella-name)))))))))
                                         (when (module (car ,module-spec) :if-does-not-exist :continue)
                                           (warn "~@<redefining module ~A in DEFDISTRIBUTOR~:@>" (car ,module-spec)))
                                         (let ((,module (or (module (car ,module-spec) :if-does-not-exist :continue) (define-module *perspective* (car ,module-spec)))))
                                           (apply #'define-module-repositories ,module ,type :distributor ',name :umbrella ,umbrella-name (rest ,module-spec))))))))))))

(defun mark-non-leaf (depkey dep)
  (setf (gethash (coerce-to-name depkey) (nonleaves *perspective*)) dep))

(defun mark-maybe-leaf (depeekey depee)
  (setf (gethash (coerce-to-name depeekey) (leaves *perspective*)) depee))

(defmacro define-module-dependencies (&body body)
  `(iter (for (module-name . dependencies) in '(,@body))
         (for module = (module module-name))
         (mark-non-leaf module-name module)
         (dolist (dep dependencies)
           (mark-maybe-leaf dep (module dep))
           (depend module (module dep)))))

(defun print-dependencies ()
  (labels ((module-deps (dep)
             (iter (for (nil (depdep . color)) in-hashtable (depsolver::%depobj-dep# dep))
                   (collect (name depdep)))))
    (iter (for (name nonleaf) in-hashtable (nonleaves *perspective*))
          (collect (cons name (module-deps nonleaf))))))

(defun minimise-dependencies (&aux (loops (make-hash-table :test #'equal)))
  (labels ((maybe-remove-nonleaf (name leaf)
             (unless (satisfied-p leaf)
               (remhash name (leaves *perspective*))))
           (minimise (current &optional acc-deps)
             (cond ((member current acc-deps) ;; is there a dependency loop?
                    (push (first acc-deps) (gethash current loops))
                    (undepend current (first acc-deps)))
                   (t
                    (dolist (overdep (intersection (cdr acc-deps) (mapcar #'cadr (hash-table-alist (depsolver::%depobj-dep# current)))))
                      (undepend current overdep))
                    (mapc (rcurry #'minimise (cons current acc-deps)) (mapcar #'cdr (hash-table-alist (depsolver::%depobj-rdep# current))))))))
    (maphash #'maybe-remove-nonleaf (leaves *perspective*))
    (maphash-values #'minimise (leaves *perspective*))
    (iter (for (dependent deplist) in-hashtable loops)
          (mapc (curry #'depend dependent) deplist))))

(defgeneric pull (to from))

(define-condition repository-pull-failed (warning)
  ((from :accessor cond-from :type repository :initarg :from)
   (to :accessor cond-to :type repository :initarg :to))
  (:report (lambda (cond stream)
             (format stream "~@<failed to pull from ~S to ~S~:@>" (cond-from cond) (cond-to cond)))))

;;;   (multiple-value-bind (primary backups) (url from)
;;;     (with-condition-restart-binding ((external-program-failure continue))
;;;       (iter (for url in (cons primary backups))
;;;             (restart-bind ((continue (lambda (cond) (declare (ignore cond)) (format t "failed to fetch from ~S~%" url) (next-iteration))
;;;                              :report-function (lambda (stream) (format stream "Try fetching from other backup URLs."))))
;;;               (call-next-method o :url url)
;;;               (return-from fetch-module t))
;;;             (finally (warn 'module-fetch-failed :module o)))))

(defmethod pull :around (to from)
  (format t "pulling from ~S to ~S~%" from to)
  (with-condition-restart-binding ((external-program-failure resignal))
    (restart-bind ((resignal (lambda (cond) (declare (ignore cond)) (signal 'repository-pull-failed :from from :to to))))
      (call-next-method to from))))

(defgeneric update-configuration (repository)
  (:method ((o repository)) t)
  (:method ((o local-git-repository))
    (let ((gitignore (make-pathname :directory (pathname-directory (path o)) :name ".gitignore")))
      (unless (probe-file gitignore)
        (with-open-file (s gitignore :direction :output :if-does-not-exist :create)
          (format s ".gitignore~%*~~~%*.o~%*.fasl~%"))))))

(defmethod pull :around ((to git-repository) from)
  (let ((preexisting (probe-file (path to))))
    (call-next-method)
    (unless preexisting ;; new shiny repository
      (when (default-world-readable *perspective*)
        (setf (world-readable-p to) t)))
    (update-configuration to)))

(defmethod pull ((to local-darcs-repository) (from remote-darcs-repository) &aux (path (namestring (path to))) (url (url from)))
  (if (probe-file path)
      (darcs "pull" "--all" "--repodir" path url)
      (darcs "get" url path)))

(defmethod pull ((to local-git-repository) (from remote-git-repository) &aux (path (namestring (path to))) (url (url from)))
  (if (probe-file path)
      (with-changed-directory path
        (git "pull"))
      (with-changed-directory (repo-pool-root to)
        (git "clone" url))))

(defmethod pull ((to local-svn-repository) (from remote-svn-repository))
  (rsync "-ravPz" (format nil "~Asvn/" (url from)) (namestring (path to))))

(defmethod pull ((to local-cvs-repository) (from remote-cvs-repository))
  (rsync "-ravPz" (format nil "~Acvsroot/" (url from)) (namestring (path to))))

(defmethod pull ((to local-git-repository) (from local-cvs-repository))
  (ensure-directories-exist (make-pathname :directory (append (pathname-directory (lockdir *perspective*)) (list (downstring (name (repo-module to)))))))
  (git-cvsimport "-v" "-C" (namestring (path to)) "-d" (format nil ":local:~A" (path from)) (downstring (repo-cvs-module (repo-master from)))))

(defmethod pull ((to local-git-repository) (from local-svn-repository))
  (git-svnimport "-C" (namestring (path to)) (url from)))

(defmethod pull ((to local-git-repository) (from local-darcs-repository))
  (ensure-directories-exist (namestring (path to)))
  (within-repository (to)
    (darcs-to-git (namestring (path from))))
  (when (repository-bare-p to)
    (setf (repository-bare-p to) nil)))

(defgeneric pull-chain (repo)
  (:method (o) t)
  (:method ((o derived-repository))
    (pull-chain (repo-master o))
    (pull o (repo-master o))))

(defun update-single (os &aux (o (coerce-to-module os)))
  (pull-chain (module-master-repository o))
  (ensure-loadable o))

(defun update (os &key skip-loadable (check-success t) &aux (o (coerce-to-module os)))
  (let* ((full-set (module-full-dependencies o))
         (initially-unloadable (remove-if #'loadable full-set)))
    (mapc #'update-single (xform-if skip-loadable (curry #'remove-if #'loadable) full-set))
    (let* ((still-unloadable (remove-if #'loadable full-set))
           (degraded (intersection still-unloadable (set-difference full-set initially-unloadable))))
      (cond ((not check-success)
             (warn "~@<success check suppressed~:@>"))
            (degraded
             (error "~@<modules degraded after update: ~S~:@>" degraded))
            (still-unloadable
             (error "~@<modules remained unloadable after update: ~S~:@>" still-unloadable))
            (t)))))

(defun cling (os &key skip-loadable &aux (o (coerce-to-module os)))
  (update o :skip-loadable skip-loadable)
  (load-system o))

(defun init (&key (runcontrol (make-pathname :directory (pathname-directory (user-homedir-pathname)) :name ".clingrc")))
  (format t "loading user run control file ~S~%" runcontrol)
  (let ((*package* (find-package :cling)))
    (when (probe-file runcontrol)
      (load runcontrol))))

(defun gui (os &aux (o (coerce-to-module os)))
  (with-changed-directory (path (module-master-repository o)) 
    (git "gui" ;; :environment (cons "DISPLAY=10.128.0.1:0.0" (sb-ext:posix-environ))
         )))
