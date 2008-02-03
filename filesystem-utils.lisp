(in-package :cling)

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

(defvar *executable-search-path* #+unix '(#p"/usr/bin/" #p"/bin/") #+win32 '(#p"c:\\program files\\git\\bin\\git.exe"))

(define-condition executable-not-found (error)
  ((name :accessor cond-name :initarg :name)
   (search-path :accessor cond-search-path :initarg :search-path))
  (:report (lambda (cond stream)
             (format stream "~@<executable named ~S wasn't found in search path ~S~:@>" (cond-name cond) (cond-search-path cond)))))

(defun find-executable (name &optional (paths *executable-search-path*))
  (iter (for path in paths)
        (for exec-path = (merge-pathnames path (make-pathname :name name)))
        (when (probe-file exec-path) (leave exec-path))
        (finally (error 'executable-not-found :name name :search-path paths))))

(defmacro define-external-program (name &optional (exec-name (string-downcase (symbol-name name))))
  `(let ((pathname (find-executable ,exec-name)))
     (defun ,name (&rest parameters)
       (run-external-program pathname parameters))))

(define-external-program git)
(define-external-program rm)
#-win32
(progn
  (define-external-program darcs)
  (define-external-program rsync)
  (define-external-program svn)
  (define-external-program git-cvsimport)
  (define-external-program darcs-to-git))

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

(defun move-to-directory (pathname target-directory)
  (if (pathname-name pathname)
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (pathname-directory target-directory) :name (pathname-name pathname))))
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (append (pathname-directory target-directory) (list (lastcar (pathname-directory pathname)))))))))
