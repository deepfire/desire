(in-package :cling)

(defvar *cvs-pool-root*)
(defvar *svn-pool-root*)
(defvar *darcs-pool-root*)
(defvar *git-pool-root*)
(defvar *lock-root*)
(defparameter *user-pool-root* (namestring (make-pathname :directory (append (pathname-directory (user-homedir-pathname)) (list "{asdf}")))))
(defparameter *sbcl-systems-location* '(".sbcl" "systems"))

(defclass repository ()
  ((module :accessor repo-module :initarg :module)))

(defclass git-repository (repository) ())
(defclass darcs-repository (repository) ())
(defclass svn-repository (repository) ())
(defclass cvs-repository (repository) ())

(defclass remote-repository (repository)
  ((umbrella :accessor repo-umbrella :initarg :umbrella)
   (method :accessor repo-method :initarg :method)
   (distributor :accessor repo-distributor :initarg :distributor)))

(defclass remote-git-repository (git-repository) () (:default-initargs :method 'git))
(defclass remote-git-http-repository (remote-git-repository) () (:default-initargs :method 'http))
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

(defclass site-repository (local-repository) ())
(defclass user-repository (local-repository) ())

(defclass derived-repository (local-repository)
  ((master :accessor repo-master :initarg :master)
   (last-update-stamp :accessor repo-last-update-stamp :initform 0)))

(defclass local-git-repository (derived-repository git-repository) ())
(defclass site-local-git-repository (site-repository local-git-repository) () (:default-initargs :pool-root *git-pool-root*))
(defclass user-local-git-repository (user-repository local-git-repository) () (:default-initargs :pool-root *user-pool-root*))
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

(defclass application (named)
  ((module :accessor app-module :initarg :module)
   (package-name :accessor app-package-name :initarg :package-name)
   (function-name :accessor app-function-name :initarg :function-name)
   (default-parameters :accessor app-default-parameters :initarg :default-parameters)))
