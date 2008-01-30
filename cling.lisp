(in-package :cling)

(defvar *cvs-pool-root*)
(defvar *svn-pool-root*)
(defvar *darcs-pool-root*)
(defvar *git-pool-root*)
(defvar *lock-root*)
(defvar *subscribed-homes*)
(defparameter *sbcl-systems-location* '(".sbcl" "systems"))

(defclass repository ()
  ((module :accessor repo-module :initarg :module)))

(defclass git-repository (repository) ())
(defclass darcs-repository (repository) ())
(defclass svn-repository (repository) ())
(defclass cvs-repository (repository) ())

(defclass remote-repository (repository)
  ((url :accessor repo-url :initarg :url)
   (umbrella :accessor repo-umbrella :initarg :umbrella)
   (method :accessor repo-method :initarg :method)
   (distributor :accessor repo-distributor :initarg :distributor)))

(defclass remote-git-repository (git-repository) () (:default-initargs :method 'git))
(defclass remote-git-http-repository (git-repository) () (:default-initargs :method 'http))
(defclass remote-darcs-repository (darcs-repository) () (:default-initargs :method 'http))
(defclass remote-svn-repository (svn-repository) () (:default-initargs :method 'svn))
(defclass remote-cvs-repository (cvs-repository) () (:default-initargs :method 'rsync))

(defgeneric distributor-repo-url (method distributor repo))

(defgeneric url (o)
  (:method ((o remote-repository))
    (namestring (make-pathname :directory `(:relative ,(format nil "~(~A~):/" (repo-method o)) ,@(distributor-repo-url (repo-method o) (repo-distributor o) o))))))

(defclass local-repository (repository)
  ((pool-root :accessor repo-pool-root :initarg :pool-root)))

(defgeneric path (repo)
  (:method ((o local-repository))
    (make-pathname :directory `(:absolute ,(repo-pool-root o) ,(downstring (name (repo-module o)))))))

(defclass derived-repository (local-repository)
  ((master :accessor repo-master :initarg :master)
   (last-update-stamp :accessor repo-last-update-stamp :initform 0)))

(defclass local-git-repository (derived-repository git-repository) () (:default-initargs :pool-root *git-pool-root*))
(defclass local-darcs-repository (derived-repository darcs-repository) () (:default-initargs :pool-root *darcs-pool-root*))
(defclass local-svn-repository (derived-repository svn-repository) () (:default-initargs :pool-root *svn-pool-root*))
(defclass local-cvs-repository (derived-repository cvs-repository) () (:default-initargs :pool-root *cvs-pool-root*))


(defclass named ()
  ((name :accessor name :initarg :name)))

(defun downstring (x)
  (string-downcase (string x)))

(defclass module (named depobj)
  ((name :accessor name :initarg :name)
   (asdf-name :accessor module-asdf-name :initarg :asdf-name)
   (repositories :accessor module-repositories :initarg :repositories)
   (master-repo :accessor module-master-repo :initarg :master-repo))
  (:default-initargs
   :repositories nil))

(defmethod print-object ((o module) stream)
  (format stream "#<~@<~S ~S ASDF name: ~S~:@>>" (type-of o) (name o) (module-asdf-name o)))

(defparameter *software-modules* (make-hash-table :test 'eq))

(defun defmodule (name &key (asdf-name name))
  (setf (gethash name *software-modules*) (make-instance 'module :name name :asdf-name asdf-name)))

;; subclassing
;; what's the type signified by TYPE?
;; whether, and what type, if yes, do we want the remote repository to be?
;; whether, and what type, if yes, do we want the mirror repository to be?
(defun define-module-repositories (module type umbrella-name)
  )

(defun module (name)
  (or (gethash name *software-modules*)
      (error "~@<undefined module ~S~:@>" name)))


(defparameter *leaves* (make-hash-table :test 'eq))
(defparameter *nonleaves* (make-hash-table :test 'eq))

(defun mark-non-leaf (depkey dep)
  (setf (gethash depkey *nonleaves*) dep))

(defun mark-maybe-leaf (depeekey depee)
  (setf (gethash depeekey *leaves*) depee))

(defun merge-paths (&rest paths)
  (namestring (make-pathname :directory paths)))

(defmacro defdistributor (name &rest definitions)
  `(progn ,@(iter (for (op . op-body) in definitions)
                  (appending (ecase op
                               (:url-schemas
                                (iter (for (method (repo) . body) in op-body)
                                      (collect (with-gensyms (m d)
                                                 `(defmethod distributor-repo-url ((,m (eql ',method)) (,d (eql ',name)) ,repo)
                                                    (declare (ignorable ,d))
                                                    `(,,@repo))))))
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


(defclass application (named)
  ((module :accessor app-module :initarg :module)
   (package-name :accessor app-package-name :initarg :package-name)
   (function-name :accessor app-function-name :initarg :function-name)
   (default-parameters :accessor app-default-parameters :initarg :default-parameters)))

(defparameter *applications* (make-hash-table :test 'eq))

(defun defapplication (name module-name package-name function-name &rest default-parameters)
  (setf (gethash name *applications*) (make-instance 'application :module (module module-name)
                                                     :package-name package-name :function-name function-name :default-parameters default-parameters)))

(defun app (name)
  (gethash name *applications*))

(defun run (app &rest parameters)
  (let ((module (app-module app)))
    (unless (asdf-loadable-p module)
      (update module))
    (unless (find-package (app-package-name app))
      (asdf:oos 'asdf:load-op (module-asdf-name module)))
    (apply (symbol-function (find-symbol (string (app-function-name app)) (app-package-name app))) (or parameters (app-default-parameters app)))))



(defun repository-bare-p (module)
  (within-module (module)
    (null (probe-file ".git"))))

(defun (setf repository-bare-p) (val module)
  (within-module (module)
    (if val
        (error "not implemented")
        (progn
          (let ((git-files (directory (make-pathname :directory '(:relative) :name :wild))))
            (sb-posix:mkdir ".git" #o755)
            (dolist (filename git-files)
              (move-to-directory filename (make-pathname :directory '(:relative ".git") :name (pathname-name filename) :type (pathname-type filename)))))
          (git "config" "--replace-all" "core.bare" "false")
          (git "checkout" "master")
          (git "reset" "--hard")
          nil))))

(defun world-readable-p (module)
  (within-module (module ".git")
    (not (null (probe-file "git-daemon-export-ok")))))

(defun (setf world-readable-p) (val module)
  (within-module (module ".git")
    (if val
        (with-open-file (s "git-daemon-export-ok" :if-does-not-exist :create) t)
        (and (delete-file "git-daemon-export-ok") nil))))

(defun asdf-definition (o)
  (make-pathname :directory `(:absolute ,*git-pool-root* ,(downstring (name o))) :name (downstring (module-asdf-name o)) :type "asd"))

(defun asdf-symlink (o home)
  (make-pathname :directory `(:absolute ,home ,@*sbcl-systems-location*) :name (downstring (module-asdf-name o)) :type "asd"))

(defun asdf-loadable-p (o &optional (home (first *subscribed-homes*)))
  (and (probe-file (asdf-symlink o home))
       (probe-file (sb-posix:readlink (asdf-symlink o home)))))

(defun (setf asdf-loadable-p) (val o home)
  (if val
      (unless (asdf-loadable-p o home)
        (let ((symlink (asdf-symlink o home)))
          (when (probe-file symlink)
            (sb-posix:unlink symlink)) ;; FIXME: delete-file refuses to remove dead symlinks)
          (sb-posix:symlink (asdf-definition o) (asdf-symlink o home))))
      (error "not implemented")))

(define-condition module-fetch-failed (warning)
  ((module :accessor cond-module :initarg :module))
  (:report (lambda (cond stream)
                   (format stream "~@<failed to fetch module ~S~:@>" (name (cond-module cond))))))

(defgeneric fetch-module (module &key url)
  (:method :around ((o module) &key url)
    (declare (ignore url))
    (multiple-value-bind (primary backups) (url o)
      (with-condition-restart-binding ((external-program-failure continue))
        (iter (for url in (cons primary backups))
              (restart-bind ((continue (lambda (cond) (declare (ignore cond)) (format t "failed to fetch from ~S~%" url) (next-iteration))
                               :report-function (lambda (stream) (format stream "Try fetching from other backup URLs."))))
                (call-next-method o :url url)
                (return-from fetch-module t))
              (finally (warn 'module-fetch-failed :module o)))))))

(defmethod fetch-module ((o darcs-module) &key url)
  (let ((local-path (merge-paths :absolute *darcs-pool-root* (downstring (name o)))))
    (if (probe-file local-path)
        (darcs "pull" "--all" "--repodir" local-path url)
        (darcs "get" url local-path))))

(defmethod fetch-module ((o git-module) &key url)
  (let ((local-path (merge-paths :absolute *git-pool-root* (downstring (name o)))))
    (if (probe-file local-path)
        (with-changed-directory local-path
          (git "pull" url))
        (git "clone" url local-path))))

(defmethod fetch-module ((o svn-module) &key url)
  (svn "checkout" url (merge-paths :absolute *svn-pool-root* (downstring (name o)))))

(defmethod fetch-module ((o cvs-module) &key url)
  (rsync "-ravPz" url (merge-paths :absolute *cvs-pool-root* (downstring (name o)))))

(defgeneric engit-module (o))

(defmethod engit-module ((o cvs-module))
  (ensure-directories-exist (merge-paths :absolute *lock-root* (downstring (name o))))
  (git-cvsimport "-v" "-C" (merge-paths :absolute *git-pool-root* (downstring (name o))) "-d" (format nil ":local:~A~A" *cvs-pool-root* (downstring (name o))) (downstring (name o))))

(defmethod engit-module ((o darcs-module))
  (ensure-directories-exist (gitpath o))
  (within-module (o)
    (darcs-to-git (sourcepath o)))
  (when (repository-bare-p o)
    (setf (repository-bare-p o) nil)))

;; (defmethod engit-module ((o darcs-module))
;;   (darcs2git (merge-paths :absolute *darcs-pool-root* (downstring (name o))) "-d" (merge-paths :absolute *git-pool-root* (downstring (name o))))
;;   (when (repository-bare-p o)
;;     (setf (repository-bare-p o) nil)))

(defun uplink-module (o &key (homes *subscribed-homes*))
  (mapc (curry #'(setf asdf-loadable-p) t o) homes))

(defun update (o &key skip-present (check-success t))
  (let* ((full-set (module-full-dependencies o))
         (initially-unloadable (remove-if #'asdf-loadable-p full-set)))
    (flet ((report (module)
             (format t "updating ~S from ~A~%" (name module) (url module))))
      (mapc (bukkake #'report #'fetch-module #'engit-module #'uplink-module)
            (xform-if skip-present (curry #'remove-if #'asdf-loadable-p) full-set)))
    (let* ((unloadable (remove-if #'asdf-loadable-p full-set))
           (degraded (intersection unloadable (set-difference full-set initially-unloadable))))
      (cond ((not check-success)
             (warn "~@<success check suppressed~:@>"))
            (degraded
             (error "~@<modules degraded after update: ~S~:@>" degraded))
            (unloadable
             (error "~@<modules remained unloadable after update: ~S~:@>" unloadable))
            (t)))))

(defun gui (module)
  (with-changed-directory (gitpath module) 
    (git "gui" ;; :environment (cons "DISPLAY=10.128.0.1:0.0" (sb-ext:posix-environ))
         )))
