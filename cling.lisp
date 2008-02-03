(in-package :cling)

(defvar *perspectives* (make-hash-table :test 'eq))
(defvar *perspective*)

(defun perspective (name)
  (gethash name *perspectives*))

(defun (setf perspective) (val name)
  (declare (type perspective val))
  (setf (gethash name *perspectives*) val))

(setf (perspective :gateway) (make-instance 'gateway-perspective)
      *perspective* (perspective :gateway))

(defun derive-user-perspective (from name distributor)
  (lret ((user-perspective (make-instance 'user-perspective)))
    (iter (for (name module) in-hashtable (modules from))
          (let* ((derived-module (make-instance 'module :name name :asdf-name (asdf-name module)))
                 (remote (make-instance 'remote-git-repo :module derived-module :distributor distributor))
                 (local (make-instance 'user-local-git-repo :module derived-module :master remote)))
            (setf (gethash name (modules user-perspective)) derived-module
                  (module-master-repo derived-module) local
                  (module-repositories (list local remote)))))))

(defun defmodule (name &key (asdf-name name))
  (setf (gethash name (modules *perspective*)) (make-instance 'module :name name :asdf-name asdf-name)))

(defun module (name)
  (or (gethash name (modules *perspective*))
      (error "~@<module ~S not defined in perspective ~S~:@>" name perspective)))

(defun mark-non-leaf (depkey dep)
  (setf (gethash depkey (nonleaves *perspective*)) dep))

(defun mark-maybe-leaf (depeekey depee)
  (setf (gethash depeekey (leaves *perspective*)) depee))

;; subclassing
;; what's the type signified by TYPE?
;; whether, and what type, if yes, do we want the remote repository to be?
;; whether, and what type, if yes, do we want the mirror repository to be?
(defun define-module-repositories (module type umbrella-name)
  )

(defmacro defdistributor (name &rest definitions)
  `(progn ,@(iter (for (method (repo) . body) in definitions)
                  (collect (with-gensyms (m d)
                             `(defmethod distributor-repo-url ((,m (eql ',method)) (,d (eql ',name)) ,repo)
                                (declare (ignorable ,d))
                                `(,,@repo)))))))

(defmacro defmodule (name &rest definitions)
  `(progn ,@(iter (for (op . op-body) in definitions)
                  (appending (ecase op
                               (:repositories
                                (iter (for (type . repo-specs) in op-body)
                                      (unless (find-class type)
                                        (error "~@<unknown repository type ~S~:@>" type))
                                      (appending (iter (for repo-spec in repo-specs)
                                                       (appending (destructuring-bind (umbrella-name . module-specs) (if (consp repo-spec) repo-spec (list repo-spec repo-spec))
                                                                    (iter (for module-spec in module-specs)
                                                                          (for full-module-spec = (ensure-cons module-spec))
                                                                          (collect `(let ((or (module ',(car full-module-spec)) (defmodule ,@full-module-spec)))
                                                                                      (define-module-repositories module ',type ',umbrella-name)))))))))))))))

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
    (iter (for (name nonleaf) in-hashtable *nonleaves*)
          (collect (cons name (module-deps nonleaf))))))

(defun minimise-dependencies (&aux (loops (make-hash-table :test #'equal)))
  (labels ((maybe-remove-nonleaf (name leaf)
             (unless (satisfied-p leaf)
               (remhash name *leaves*)))
           (minimise (current &optional acc-deps)
             (cond ((member current acc-deps) ;; is there a dependency loop?
                    (push (first acc-deps) (gethash current loops))
                    (undepend current (first acc-deps)))
                   (t
                    (dolist (overdep (intersection (cdr acc-deps) (mapcar #'cadr (hash-table-alist (depsolver::%depobj-dep# current)))))
                      (undepend current overdep))
                    (mapc (rcurry #'minimise (cons current acc-deps)) (mapcar #'cdr (hash-table-alist (depsolver::%depobj-rdep# current))))))))
    (maphash #'maybe-remove-nonleaf *leaves*)
    (maphash-values #'minimise *leaves*)
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
      (git "clone" url path)))

(defmethod pull ((to local-svn-repository) (from remote-svn-repository))
  (svn "checkout" (url from) (namestring (path to))))

(defmethod pull ((to local-cvs-repository) (from remote-cvs-repository))
  (rsync "-ravPz" (url from) (namestring (path to))))

(defmethod pull ((to local-git-repository) (from local-cvs-repository))
  (ensure-directories-exist (make-pathname :directory `(:absolute ,(lockdir *perspective*) (downstring (name (repo-module to))))))
  (git-cvsimport "-v" "-C" (namestring (path to)) "-d" (format nil ":local:~A" (path from)) (downstring (name (repo-module to)))))

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
