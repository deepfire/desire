(in-package :cling)

(defmacro within-repository ((repo &rest pathname-elements) &body body)
  `(with-changed-directory (namestring (make-pathname :directory `(:absolute ,(path ,repo) ,,@pathname-elements)))
     ,@body))

(defun repository-bare-p (repo)
  (within-repository (repo)
    (null (probe-file ".git"))))

(defun (setf repository-bare-p) (val repo)
  (within-repository (repo)
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

(defun world-readable-p (repo)
  (within-repository (repo ".git")
    (not (null (probe-file "git-daemon-export-ok")))))

(defun (setf world-readable-p) (val repo)
  (within-repository (repo ".git")
    (if val
        (with-open-file (s "git-daemon-export-ok" :if-does-not-exist :create) t)
        (and (delete-file "git-daemon-export-ok") nil))))
