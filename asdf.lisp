(in-package :cling)

(defun asdf-definition (o)
  (declare (type module o))
  (merge-pathnames (make-pathname :name (downstring (module-asdf-name o)) :type "asd") (path (module-master-repo o))))

#-win32
(defun asdf-symlink (o)
  (declare (type module o))
  (make-pathname :directory (append (pathname-directory (user-homedir-pathname)) *sbcl-systems-location*) :name (downstring (module-asdf-name o)) :type "asd"))

(defgeneric asdf-loadable-p (o))

(defmethod asdf-loadable-p ((o module))
  (asdf-loadable-p (module-master-repo o)))

(defmethod asdf-loadable-p ((o user-repository))
  (not (null (probe-file (asdf-definition (repo-module o))))))

#-win32
(defmethod asdf-loadable-p ((o site-repository))
  (and (probe-file (asdf-symlink (repo-module o)))
       (probe-file (sb-posix:readlink (asdf-symlink (repo-module o))))))

(defgeneric (setf asdf-loadable-p) (val o))

(defmethod (setf asdf-loadable-p) (val (o module))
  (setf (asdf-loadable-p (module-master-repo o)) val))

(defmethod (setf asdf-loadable-p) ((val (eql t)) (o user-repository))
  t)

#-win32
(defmethod (setf asdf-loadable-p) ((val (eql t)) (o site-repository))
  (unless (asdf-loadable-p o)
    (let ((symlink (asdf-symlink (repo-module o))))
      (when (probe-file symlink)
        (sb-posix:unlink symlink)) ;; FIXME: delete-file refuses to remove dead symlinks)
      (sb-posix:symlink (asdf-definition (repo-module o)) (asdf-symlink (repo-module o))))))

(defgeneric purge-fasls (o)
  (:method ((o repository))
    (mapc #'delete-file (directory (make-pathname :directory (append (pathname-directory (path o)) (list :wild-inferiors)) :name :wild :type "fasl"))))
  (:method ((o module))
    (purge-fasls (module-master-repo o))))
