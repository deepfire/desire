(in-package :cling)

(defun gitpath (o)
  (merge-paths :absolute *git-pool-root* (module-namestring o)))

(defun sourcepath (o)
  (merge-paths :absolute (module-pool-root o) (module-namestring o)))

(defun purge-fasls (module)
  (mapc #'delete-file (directory (make-pathname :directory `(:absolute ,(gitpath module) :wild-inferiors) :name :wild :type "fasl"))))

(defmacro within-module ((module &rest pathname-elements) &body body)
  `(with-changed-directory (namestring (make-pathname :directory `(:absolute ,(gitpath ,module) ,,@pathname-elements)))
     ,@body))

(defun move-to-directory (pathname target-directory)
  (if (pathname-name pathname)
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (pathname-directory target-directory) :name (pathname-name pathname))))
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (append (pathname-directory target-directory) (list (lastcar (pathname-directory pathname)))))))))
