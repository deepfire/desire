(in-package :cling)

(defclass perspective ()
  ((modules :accessor modules :initarg :modules)
   (repositories :accessor repositories :initarg :repositories)
   (leaves :accessor leaves :initarg :leaves)
   (nonleaves :accessor nonleaves :initarg :nonleaves)
   (git-pool :accessor git-pool :initarg :git-pool)
   (default-world-readable :accessor default-world-readable :initarg :default-world-readable))
  (:default-initargs
   :modules (make-hash-table :test 'equal)
   :repositories (make-hash-table :test 'equal)
   :leaves (make-hash-table :test 'equal)
   :nonleaves (make-hash-table :test 'equal)
   :default-world-readable t))

(defclass gateway-perspective (perspective)
  ((darcs-pool :accessor darcs-pool :initarg :darcs-pool)
   (svn-pool :accessor svn-pool :initarg :svn-pool)
   (cvs-pool :accessor cvs-pool :initarg :cvs-pool)
   (lockdir :accessor lockdir :initarg :lockdir)))

(defmethod print-object ((o gateway-perspective) stream)
  (format stream "~@<#<~S default ~:[non-~;~]world-readable, git: ~S, darcs: ~S, svn: ~S, cvs: ~S, cvslock: ~S>~:@>"
          (type-of o) (default-world-readable o) (git-pool o) (darcs-pool o) (svn-pool o) (cvs-pool o) (lockdir o)))

(defclass user-perspective (perspective)
  ((home :accessor home :initarg :home))
  (:default-initargs
   :home (user-homedir-pathname)))

(defmethod print-object ((o user-perspective) stream)
  (format stream "~@<#<~S default ~:[non-~;~]world-readable, git: ~S, home: ~S>~:@>"
          (type-of o) (default-world-readable o) (git-pool o) (home o)))

(defmethod initialize-instance :after ((o user-perspective) &key git-pool &allow-other-keys)
  (setf (git-pool o) (or git-pool (namestring (make-pathname :directory (append (pathname-directory (home o)) (list "{asdf}")))))))

(defvar *perspective*)
(defparameter *sbcl-systems-location* '(".sbcl" "systems"))

(defclass named ()
  ((name :accessor name :initarg :name)))

(defclass repository ()
  ((module :accessor repo-module :initarg :module)))

(defmethod name ((o repository))
  (name (repo-module o)))

(defclass git-repository (repository) ())
(defclass darcs-repository (repository) ())
(defclass svn-repository (repository) ())
(defclass cvs-repository (repository) ())

(defclass remote-repository (repository)
  ((umbrella :accessor repo-umbrella :initarg :umbrella)
   (method :accessor repo-method :initarg :method)
   (distributor :accessor repo-distributor :initarg :distributor)))

(defclass remote-git-repository (remote-repository git-repository) () (:default-initargs :method 'git))
(defclass remote-git-http-repository (remote-git-repository) () (:default-initargs :method 'http))
(defclass remote-darcs-repository (remote-repository darcs-repository) () (:default-initargs :method 'http))
(defclass remote-svn-repository (remote-repository svn-repository) () (:default-initargs :method 'svn))
(defclass remote-cvs-repository (remote-repository cvs-repository) 
  ((cvs-module :accessor repo-cvs-module :initarg :cvs-module))
  (:default-initargs :method 'rsync))

(defmethod initialize-instance :after ((o remote-cvs-repository) &key cvs-module &allow-other-keys)
  (setf (repo-cvs-module o) (or cvs-module (name o))))

(defgeneric distributor-repo-url (method distributor repo))

(defgeneric url (o)
  (:method ((o remote-repository))
    (with-output-to-string (s)
           (format s "~(~A~)://" (repo-method o))
           (iter (for x in (distributor-repo-url (repo-method o) (repo-distributor o) o))
                 (princ x s)
                 (princ #\/ s)))))

(defmethod print-object ((o remote-repository) stream)
  (format stream "~@<#<~S ~S>~:@>" (type-of o) (url o)))

(defclass local-repository (repository)
  ((pool-root :accessor repo-pool-root :initarg :pool-root))
  (:default-initargs :pool-root (git-pool *perspective*)))

(defgeneric path (repo)
  (:method ((o local-repository))
    (make-pathname :directory (append (pathname-directory (repo-pool-root o)) (list (downstring (name o)))))))

(defmethod print-object ((o local-repository) stream)
  (format stream "~@<#<~S ~S>~:@>" (type-of o) (path o)))

(defclass site-repository (local-repository) ())
(defclass user-repository (local-repository) ())

(defclass derived-repository (local-repository)
  ((master :accessor repo-master :initarg :master)
   (last-update-stamp :accessor repo-last-update-stamp :initform 0)))

(defclass local-git-repository (git-repository) ())
(defclass site-local-derived-git-repository (site-repository derived-repository local-git-repository) ())
(defclass user-local-derived-git-repository (user-repository derived-repository local-git-repository) ())
(defclass site-local-origin-git-repository (site-repository local-git-repository) ())
(defclass user-local-origin-git-repository (user-repository local-git-repository) ())
(defclass local-darcs-repository (derived-repository darcs-repository) () (:default-initargs :pool-root (darcs-pool *perspective*)))
(defclass local-svn-repository (derived-repository svn-repository) () (:default-initargs :pool-root (svn-pool *perspective*)))
(defclass local-cvs-repository (derived-repository cvs-repository) () (:default-initargs :pool-root (cvs-pool *perspective*)))

(defun perspective-master-repo-typemap (perspective-type)
  (ecase perspective-type
    (gateway-perspective 'site-local-derived-git-repository)
    (user-perspective 'user-local-derived-git-repository)))

(defun coerce-repo-type-to-mnemonic (type)
  (list (cond ((subtypep type 'local-repository) 'local)
              ((subtypep type 'remote-repository) 'remote)
              (t (error "~@<malformed repository type specifier ~S~:@>" type)))
        (cond ((subtypep type 'git-repository) 'git)
              ((subtypep type 'darcs-repository) 'darcs)
              ((subtypep type 'svn-repository) 'svn)
              ((subtypep type 'cvs-repository) 'cvs)
              (t (error "~@<malformed repository type specifier ~S~:@>" type)))))

(defun repo-name (repo)
  (cons (name (repo-module repo)) (coerce-repo-type-to-mnemonic (type-of repo))))

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
