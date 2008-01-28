(defpackage cling
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver)
  (:export
   #:defdistributor #:define-module-dependencies #:module #:update))

(in-package :cling)

(defclass named ()
  ((name :accessor name :initarg :name)))

(defgeneric named-namestring (o)
  (:method ((o named))
    (string-downcase (string (name o)))))

(defclass module (named depobj)
  ((pool-root :accessor module-pool-root :initarg :pool-root)
   (umbrella :accessor module-umbrella :initarg :umbrella)
   (asdf-name :accessor module-asdf-name :initarg :asdf-name)
   (last-update-stamp :accessor module-last-update-stamp :initform nil)
   (method :accessor module-method :initarg :method)
   (distributor :accessor module-distributor :initarg :distributor)))

(defun asdf-namestring (o)
  (string-downcase (string (module-asdf-name o))))

(defun umbrella-name (module)
  (string-downcase (string (module-umbrella module))))

(defun method-url (method distributor &rest components)
  (list* (format nil "~(~A~):/" method) (format nil "~(~A~)" distributor) components))

(defgeneric %module-urls (method distributor module))

(defun merge-paths (&rest paths)
  (namestring (make-pathname :directory paths)))

(defun url (o)
  (let ((urls (mapcar (curry #'apply #'merge-paths :relative) (%module-urls (module-method o) (module-distributor o) o))))
    (values (first urls) (rest urls))))

(defmethod print-object ((o module) stream)
  (format stream "#<~@<~S ~S method: ~S distributor: ~S url: ~S ASDF name: ~S~:@>>" (type-of o) (name o) (module-method o) (module-distributor o) (url o) (module-asdf-name o)))

(defparameter *cvs-pool-root* "/mnt/enter/cvs/")
(defparameter *svn-pool-root* "/mnt/enter/svn/")
(defparameter *darcs-pool-root* "/mnt/enter/darcs/")
(defparameter *git-pool-root* "/mnt/etherstorm/git/")
(defparameter *lock-root* "/var/lock/")

(defparameter *sbcl-systems-location* '(".sbcl" "systems"))
(defparameter *subscribed-homes* '("/home/deepfire/"))

(defclass noop-fetch-module (module) ())
(defclass noop-import-module (module) ())
(defclass cvs-module (module) () (:default-initargs :method 'rsync :pool-root *cvs-pool-root*))
(defclass darcs-module (module) () (:default-initargs :method 'http :pool-root *darcs-pool-root*))
(defclass svn-module (module) () (:default-initargs :method 'svn :pool-root *svn-pool-root*))
(defclass git-module (noop-import-module) () (:default-initargs :method 'git :pool-root *git-pool-root*))
(defclass git-http-module (noop-import-module) () (:default-initargs :method 'http :pool-root *git-pool-root*))
(defclass local-module (noop-import-module noop-fetch-module) () (:default-initargs :method 'nothing))

(defparameter *software-modules* (make-hash-table :test 'eq))
(defparameter *leaves* (make-hash-table :test 'eq))
(defparameter *nonleaves* (make-hash-table :test 'eq))

(defun defmodule (type distributor name umbrella-name asdf-name)
  (assert (subtypep type 'module))
  (setf (gethash name *software-modules*) (make-instance type :name name :distributor distributor :umbrella umbrella-name :asdf-name asdf-name)))

(defun module (name)
  (or (gethash name *software-modules*)
      (error "~@<undefined module ~S~:@>" name)))

(defun mark-non-leaf (depkey dep)
  (setf (gethash depkey *nonleaves*) dep))

(defun mark-maybe-leaf (depeekey depee)
  (setf (gethash depeekey *leaves*) depee))

(defmacro defdistributor (name &rest definitions)
  `(progn ,@(iter (for (op . op-body) in definitions)
                  (appending (ecase op
                               (:url-schemas
                                (iter (for (method (module) . bodies-spec) in op-body)
                                      (collect (let ((bodies (if (and (consp (car bodies-spec)) (eq (caar bodies-spec) 'or))
                                                                 (cdar bodies-spec)
                                                                 (list bodies-spec))))
                                                 (with-gensyms (m d)
                                                   `(defmethod %module-urls ((,m (eql ',method)) (,d (eql ',name)) ,module)
                                                      (declare (ignorable ,d))
                                                      (list ,@(mapcar (curry #'list* 'method-url m) bodies))))))))
                               (:modules
                                (iter (for (type . module-specs) in op-body)
                                      (unless (find-class type)
                                        (error "~@<unknown module type ~S~:@>" type))
                                      (appending (iter (for module-spec in module-specs)
                                                       (appending (destructuring-bind (umbrella-name . module-names) (if (consp module-spec) module-spec (list module-spec module-spec))
                                                                    (iter (for module-name in module-names)
                                                                          (collect (destructuring-bind (module-name &key asdf-name) (if (consp module-name) module-name
                                                                                                                                        (list module-name :asdf-name module-name))
                                                                                     `(defmodule ',type ',name ',module-name ',umbrella-name ',asdf-name)))))))))))))))

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

(define-condition external-program-failure (error)
  ((program :accessor cond-program :initarg :program)
   (parameters :accessor cond-parameters :initarg :parameters)
   (status :accessor cond-status :initarg :status))
  (:report (lambda (cond stream)
             (format stream "~@<running ~A~{ ~A~} failed with exit status ~S~:@>" (cond-program cond) (cond-parameters cond) (cond-status cond)))))

(defun run-external-program (program-pathname parameters)
  (lret* ((exit-code (sb-ext:process-exit-code (apply #'sb-ext:run-program program-pathname parameters '(:output t))))
          (success (zerop exit-code)))
    (unless success
      (error 'external-program-failure :program program-pathname :parameters parameters :status exit-code))))

(defmacro define-external-program (name pathname)
  `(defun ,name (&rest parameters)
     (run-external-program ,pathname parameters)))

(define-external-program darcs #p"/usr/bin/darcs")
(define-external-program git #p"/usr/bin/git")
(define-external-program rsync #p"/usr/bin/rsync")
(define-external-program svn #p"/usr/bin/svn")
(define-external-program git-cvsimport #p"/usr/bin/git-cvsimport")
(define-external-program darcs2git #p"/mnt/enter/git/darcs2git/darcs2git.py")
(define-external-program darcs-to-git #p"/mnt/enter/git/darcs-to-git/darcs-to-git")
(define-external-program rm #p"/bin/rm")

(defun gitpath (o)
  (merge-paths :absolute *git-pool-root* (named-namestring o)))

(defun sourcepath (o)
  (merge-paths :absolute (module-pool-root o) (named-namestring o)))

(define-condition about-to-purge (error)
  ((directory :accessor cond-directory :initarg :directory))
  (:report (lambda (cond stream)
             (format stream "~@<about to purge ~S~:@>" (cond-directory cond)))))

(defun purge-directory (pathname)
  (block wall
    (restart-bind ((purge (lambda () (return-from wall))
                     :report-function (lambda (stream) (format stream "~@<Proceed with purging the directory.~:@>"))
                     :test-function (lambda (cond) (typep cond 'about-to-purge))))
      (error 'about-to-purge :directory pathname)))
  (rm "-rf" pathname))

(defun purge-module-source-directory (module)
  (purge-directory (sourcepath module)))

(defun purge-module-git-directory (module)
  (purge-directory (gitpath module)))

(defun purge-module (module)
  (funcall (bukkake-combine #'purge-module-source-directory #'purge-module-git-directory) module))

(defmacro with-changed-directory (dir &body body)
  (with-gensyms (old)
    (once-only (dir)
      `(let ((,old (sb-posix:getcwd))
             (*default-pathname-defaults* (parse-namestring ,dir)))
         (sb-posix:chdir ,dir)
         (unwind-protect (progn ,@body)
           (sb-posix:chdir ,old))))))

(defmacro do-directory-pathnames ((var (&rest directory-components)) &body body)
  `(dolist (,var (directory (make-pathname :directory '(,@directory-components) :name :wild)))
     ,@body))

(defmacro within-module ((module &rest pathname-elements) &body body)
  `(with-changed-directory (namestring (make-pathname :directory `(:absolute ,(gitpath ,module) ,,@pathname-elements)))
     ,@body))

(defun move-to-directory (pathname target-directory)
  (if (pathname-name pathname)
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (pathname-directory target-directory) :name (pathname-name pathname))))
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (append (pathname-directory target-directory) (list (lastcar (pathname-directory pathname)))))))))

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

(defun present-p (o)
  (not (null (probe-file (gitpath o)))))

(defun asdf-definition (o)
  (make-pathname :directory `(:absolute ,*git-pool-root* ,(named-namestring o)) :name (asdf-namestring o) :type "asd"))

(defun asdf-symlink (o home)
  (make-pathname :directory `(:absolute ,home ,@*sbcl-systems-location*) :name (asdf-namestring o) :type "asd"))

(defun asdfly-loadable-p (o &optional (home (first *subscribed-homes*)))
  (and (probe-file (asdf-symlink o home))
       (probe-file (sb-posix:readlink (asdf-symlink o home)))))

(defun (setf asdfly-loadable-p) (val o home)
  (if val
      (unless (asdfly-loadable-p o home)
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
              (finally (warn 'module-fetch-failed :module o))))))
  (:method ((o noop-fetch-module) &key url)
    (declare (ignore o url)) t))

(defmethod fetch-module ((o darcs-module) &key url)
  (let ((local-path (merge-paths :absolute *darcs-pool-root* (named-namestring o))))
    (if (probe-file local-path)
        (darcs "pull" "--all" "--repodir" local-path url)
        (darcs "get" url local-path))))

(defmethod fetch-module ((o git-module) &key url)
  (let ((local-path (merge-paths :absolute *git-pool-root* (named-namestring o))))
    (if (probe-file local-path)
        (with-changed-directory local-path
          (git "pull" url))
        (git "clone" url local-path))))

(defmethod fetch-module ((o svn-module) &key url)
  (svn "checkout" url (merge-paths :absolute *svn-pool-root* (named-namestring o))))

(defmethod fetch-module ((o cvs-module) &key url)
  (rsync "-ravPz" url (merge-paths :absolute *cvs-pool-root* (named-namestring o))))

(defgeneric import-module (o)
  (:method ((o noop-import-module))
    (declare (ignore o)) t))

(defmethod import-module ((o cvs-module))
  (ensure-directories-exist (merge-paths :absolute *lock-root* (named-namestring o)))
  (git-cvsimport "-v" "-C" (merge-paths :absolute *git-pool-root* (named-namestring o)) "-d" (format nil ":local:~A~A" *cvs-pool-root* (named-namestring o)) (named-namestring o)))

(defmethod import-module ((o darcs-module))
  (ensure-directories-exist (module-gitpath o))
  (within-module (o)
    (darcs-to-git (module-sourcepath o)))
  (when (repository-bare-p o)
    (setf (repository-bare-p o) nil)))

;; (defmethod import-module ((o darcs-module))
;;   (darcs2git (merge-paths :absolute *darcs-pool-root* (named-namestring o)) "-d" (merge-paths :absolute *git-pool-root* (named-namestring o)))
;;   (when (repository-bare-p o)
;;     (setf (repository-bare-p o) nil)))

(defun update-unchecked (o &key skip-present)
  (let* ((full-set (module-full-dependencies o)))
    (flet ((report (module)
             (format t "updating ~S from ~A~%" (name module) (url module))))
     (mapc (bukkake-combine #'report #'fetch-module #'import-module)
           (xform-if skip-present (curry #'remove-if #'present-p) full-set)))))

(defun update (o &key skip-present)
  (let* ((full-set (module-full-dependencies o))
         (initially-unloadable (remove-if #'asdfly-loadable-p full-set)))
    (flet ((report (module)
             (format t "updating ~S from ~A~%" (name module) (url module))))
     (mapc (bukkake-combine #'report #'fetch-module #'import-module (compose (rcurry #'mapc *subscribed-homes*) (curry #'curry #'(setf asdfly-loadable-p) t)))
           (xform-if skip-present (curry #'remove-if #'present-p) full-set)))
    (let* ((unloadable (remove-if #'asdfly-loadable-p full-set))
           (degraded (intersection unloadable (set-difference full-set initially-unloadable))))
      (cond (degraded
             (error "~@<modules degraded after update: ~S~:@>" degraded))
            (unloadable
             (error "~@<modules remained unloadable after update: ~S~:@>" unloadable))
            (t)))))

(defun gui (module)
  (with-changed-directory (module-gitpath module) 
    (git "gui" ;; :environment (cons "DISPLAY=10.128.0.1:0.0" (sb-ext:posix-environ))
         )))
