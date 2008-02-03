(in-package :cling)

(defun asdf-definition (o)
  (merge-pathnames (make-pathname :name (downstring (module-asdf-name o)) :type "asd") (path (module-master-repo o))))

#-win32
(defun asdf-symlink (o)
  (make-pathname :directory (append (pathname-directory (user-homedir-pathname)) *sbcl-systems-location*) :name (downstring (module-asdf-name o)) :type "asd"))

(defgeneric asdf-loadable-p (o))

(defmethod asdf-loadable-p ((o module))
  (asdf-loadable-p (module-master-repo o)))

(defmethod asdf-loadable-p ((o user-repository))
  (not (null (probe-file (asdf-definition (repo-module o))))))

#-win32
(defmethod asdf-loadable-p ((o site-repository))
  (and (probe-file (asdf-symlink o))
       (probe-file (sb-posix:readlink (asdf-symlink o)))))

(defgeneric (setf asdf-loadable-p) (val o))

(defmethod (setf asdf-loadable-p) (val (o module))
  (setf (asdf-loadable-p (module-master-repo o)) val))

(defmethod (setf asdf-loadable-p) ((val (eql t)) (o user-repository))
  t)

#-win32
(defmethod (setf asdf-loadable-p) ((val (eql t)) (o site-repository))
  (unless (asdf-loadable-p o)
    (let ((symlink (asdf-symlink o)))
      (when (probe-file symlink)
        (sb-posix:unlink symlink)) ;; FIXME: delete-file refuses to remove dead symlinks)
      (sb-posix:symlink (asdf-definition o) (asdf-symlink o)))))

(defgeneric purge-fasls (o)
  (:method ((o repository))
    (mapc #'delete-file (directory (make-pathname :directory (append (path o) (list :wild-inferiors)) :name :wild :type "fasl"))))
  (:method ((o module))
    (purge-fasls (module-master-repo o))))
