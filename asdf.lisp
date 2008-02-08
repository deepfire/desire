(in-package :cling)

(defun asdf-definition (o)
  (declare (type module o))
  (make-pathname :directory (pathname-directory (path (module-master-repo o))) :name (downstring (module-asdf-name o)) :type "asd"))

#-win32
(defun asdf-symlink (o)
  (declare (type module o))
  (make-pathname :directory (append (pathname-directory (user-homedir-pathname)) *sbcl-systems-location*) :name (downstring (module-asdf-name o)) :type "asd"))

(defgeneric asdfly-okay (o))

(defmethod asdfly-okay ((o module))
  (asdfly-okay (module-master-repo o)))

(defmethod asdfly-okay :around ((o repository))
  (or (null (module-asdf-name (repo-module o)))
      (call-next-method)))

(defmethod asdfly-okay ((o user-repository))
  (not (null (probe-file (asdf-definition (repo-module o))))))

#-win32
(defmethod asdfly-okay ((o site-repository))
  (and (probe-file (asdf-symlink (repo-module o)))
       (probe-file (sb-posix:readlink (asdf-symlink (repo-module o))))))

(defgeneric ensure-asdfly-okay (o))

(defmethod ensure-asdfly-okay ((o module))
  (ensure-asdfly-okay (module-master-repo o)))

(defmethod ensure-asdfly-okay ((o user-repository))
  t)

#-win32
(defmethod ensure-asdfly-okay ((o site-repository))
  (unless (asdfly-okay o)
    (let ((symlink (asdf-symlink (repo-module o))))
      (when (probe-file symlink)
        (sb-posix:unlink symlink)) ;; FIXME: delete-file refuses to remove dead symlinks)
      (sb-posix:symlink (asdf-definition (repo-module o)) (asdf-symlink (repo-module o))))))

(defgeneric purge-fasls (o)
  (:method ((o repository))
    (mapc #'delete-file (directory (make-pathname :directory (append (pathname-directory (path o)) (list :wild-inferiors)) :name :wild :type "fasl"))))
  (:method ((o module))
    (purge-fasls (module-master-repo o))))


(defun cling-modules-search (system)
  (when-let ((module (module system :if-does-not-exist :continue)))
    (probe-file (asdf-definition module))))

(push 'cling-modules-search asdf:*system-definition-search-functions*)
