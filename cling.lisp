(in-package :cling)

(defvar *perspectives* (make-hash-table :test 'eq))

(defun perspective (name)
  (gethash name *perspectives*))

(defun (setf perspective) (val name)
  (declare (type perspective val))
  (setf (gethash name *perspectives*) val))

(setf (perspective :gateway) (make-instance 'gateway-perspective)
      *perspective* (perspective :gateway))

(defun coerce-to-name (name)
  (declare (type (or symbol string) name))
  (typecase name
    (symbol (symbol-name name))
    (string name)))

(defun module (name &key (when-does-not-exist :error))
  (or (gethash (coerce-to-name name) (modules *perspective*))
      (when (eq when-does-not-exist :error)
        (error "~@<module ~S not defined in perspective ~S~:@>" name *perspective*))))

(defun (setf module) (val name)
  (declare (type module val))
  (setf (gethash (coerce-to-name name) (modules *perspective*)) val))

(defun derive-user-perspective (from distributor &rest user-perspective-initargs)
  (lret ((user-perspective (apply #'make-instance 'user-perspective user-perspective-initargs)))
    (let ((*perspective* user-perspective))
      (iter (for (name module) in-hashtable (modules from))
            (let* ((derived-module (make-instance 'module :name name :asdf-name (module-asdf-name module)))
                   (remote (make-instance 'remote-git-repository :module derived-module :distributor distributor))
                   (local (make-instance 'user-local-derived-git-repository :module derived-module :master remote)))
              (setf (module name) derived-module
                    (module-master-repo derived-module) local
                    (module-repositories derived-module) (list local remote)))))))

(defun defmodule (name &key (asdf-name name))
  (setf (module name) (make-instance 'module :name name :asdf-name asdf-name)))

(defgeneric repository-import-chain (type)
  (:method ((type (eql 'local-repository))) (values '(site-local-origin-git-repository)))
  (:method ((type (eql 'git-repository))) (values '(site-local-derived-git-repository) 'remote-git-repository))
  (:method ((type (eql 'git-http-repository))) (values '(site-local-derived-git-repository) 'remote-git-http-repository))
  (:method ((type (eql 'darcs-repository))) (values '(site-local-derived-git-repository local-darcs-repository) 'remote-darcs-repository))
  (:method ((type (eql 'svn-repository))) (values '(site-local-derived-git-repository local-svn-repository) 'remote-svn-repository))
  (:method ((type (eql 'cvs-repository))) (values '(site-local-derived-git-repository local-cvs-repository) 'remote-cvs-repository)))

(defun define-module-repositories (module type &rest remote-initargs)
  (multiple-value-bind (locals remote) (repository-import-chain type)
    (let ((local-repos (mapcar (rcurry #'make-instance :module module) locals))
          (remote-repo (mapcar (rcurry (curry #'apply #'make-instance) (list* :module module remote-initargs)) (ensure-list remote))))
      (iter (for (mirror master) on (append local-repos (ensure-list remote-repo)))
            (when (and master (typep mirror 'derived-repository))
              (setf (repo-master mirror) master)))
      (setf (module-repositories module) (append local-repos remote-repo)
            (module-master-repo module) (first local-repos)))))

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
                                 `((iter (for (,module-prespec ,type ,umbrella-name) in
                                              ',(iter (for (type . repo-specs) in op-body)
                                                      (appending (iter (for repo-spec in repo-specs)
                                                                       (appending (destructuring-bind (umbrella-name . module-specs) (if (consp repo-spec) repo-spec (list repo-spec repo-spec))
                                                                                    (iter (for module-preprespec in module-specs)
                                                                                          (for module-prespec = (ensure-cons module-preprespec))
                                                                                          (collect (list module-prespec type umbrella-name)))))))))
                                         (for ,remote-spec = (remove-from-plist (rest ,module-prespec) :asdf-name))
                                         (for ,module-spec = (cons (first ,module-prespec) (remove-from-plist (rest ,module-prespec) :cvs-module)))
                                         (let ((,module (or (module (car ,module-spec) :when-does-not-exist :ignore) (apply #'defmodule ,module-spec))))
                                           (apply #'define-module-repositories ,module ,type :distributor ',name :umbrella ,umbrella-name ,remote-spec)))))))))))

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

(defun module-full-dependencies (module &optional stack)
  (unless (member module stack)
    (cons module
          (iter (for (nil (dep . color)) in-hashtable (depsolver::%depobj-dep# module))
                (unioning (module-full-dependencies dep (cons module stack)))))))

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

(defmethod pull ((to local-darcs-repository) (from remote-darcs-repository) &aux (path (namestring (path to))) (url (url from)))
  (if (probe-file path)
      (darcs "pull" "--all" "--repodir" path url)
      (darcs "get" url path)))

(defmethod pull ((to local-git-repository) (from remote-git-repository) &aux (path (namestring (path to))) (url (url from)))
  (if (probe-file path)
      (with-changed-directory path
        (git "pull" url))
      (with-changed-directory (repo-pool-root to)
        (git "clone" url))))

(defmethod pull ((to local-svn-repository) (from remote-svn-repository))
  (svn "checkout" (url from) (namestring (path to))))

(defmethod pull ((to local-cvs-repository) (from remote-cvs-repository))
  (rsync "-ravPz" (url from) (namestring (path to))))

(defmethod pull ((to local-git-repository) (from local-cvs-repository))
  (ensure-directories-exist (make-pathname :directory (append (pathname-directory (lockdir *perspective*)) (list (downstring (name (repo-module to)))))))
  (git-cvsimport "-v" "-C" (namestring (path to)) "-d" (format nil ":local:~A" (path from)) (downstring (repo-cvs-module (repo-master from)))))

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

(defun update-single (o)
  (pull-chain (module-master-repo o))
  (setf (asdf-loadable-p o) t))

(defun update (o &key skip-present (check-success t))
  (let* ((full-set (module-full-dependencies o))
         (initially-unloadable (remove-if #'asdf-loadable-p full-set)))
    (mapc #'update-single (xform-if skip-present (curry #'remove-if #'asdf-loadable-p) full-set))
    (let* ((still-unloadable (remove-if #'asdf-loadable-p full-set))
           (degraded (intersection still-unloadable (set-difference full-set initially-unloadable))))
      (cond ((not check-success)
             (warn "~@<success check suppressed~:@>"))
            (degraded
             (error "~@<modules degraded after update: ~S~:@>" degraded))
            (still-unloadable
             (error "~@<modules remained unloadable after update: ~S~:@>" still-unloadable))
            (t)))))

(defun gui (module)
  (with-changed-directory (path (module-master-repo module)) 
    (git "gui" ;; :environment (cons "DISPLAY=10.128.0.1:0.0" (sb-ext:posix-environ))
         )))
