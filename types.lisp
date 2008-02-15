(in-package :cling)

(defclass perspective ()
  ((modules :accessor modules :initarg :modules)
   (repositories :accessor repositories :initarg :repositories)
   (systems :accessor systems :initarg :systems)
   (applications :accessor applications :initarg :applications)
   (leaves :accessor leaves :initarg :leaves)
   (nonleaves :accessor nonleaves :initarg :nonleaves)
   (git-pool :accessor git-pool :initarg :git-pool)
   (default-world-readable :accessor default-world-readable :initarg :default-world-readable))
  (:default-initargs
   :modules (make-hash-table :test 'equal)
   :repositories (make-hash-table :test 'equal)
   :systems (make-hash-table :test 'equal)
   :applications (make-hash-table :test 'equal)
   :leaves (make-hash-table :test 'equal)
   :nonleaves (make-hash-table :test 'equal)
   :default-world-readable t))

(defclass gateway-perspective (perspective)
  ((darcs-pool :accessor darcs-pool :initarg :darcs-pool)
   (svn-pool :accessor svn-pool :initarg :svn-pool)
   (cvs-pool :accessor cvs-pool :initarg :cvs-pool)
   (lockdir :accessor lockdir :initarg :lockdir :documentation "Learn how to get rid of that.")))

(defmethod print-object ((o gateway-perspective) stream)
  (format stream "~@<#<~S default ~:[non-~;~]world-readable, git: ~S, darcs: ~S, svn: ~S, cvs: ~S, cvslock: ~S>~:@>"
          (type-of o) (default-world-readable o) (slot-value* o 'git-pool) (slot-value* o 'darcs-pool) (slot-value* o 'svn-pool) (slot-value* o 'cvs-pool) (slot-value* o 'lockdir)))

(defclass user-perspective (perspective)
  ((home :accessor home :initarg :home))
  (:default-initargs
   :home (user-homedir-pathname)))

(defmethod print-object ((o user-perspective) stream)
  (format stream "~@<#<~S default ~:[non-~;~]world-readable, git: ~S, home: ~S>~:@>"
          (type-of o) (default-world-readable o) (slot-value* o 'git-pool) (home o)))

(defmethod initialize-instance :after ((o user-perspective) &key git-pool &allow-other-keys)
  (setf (git-pool o) (or git-pool (namestring (make-pathname :directory (append (pathname-directory (home o)) (list "{asdf}")))))))

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

(defclass local-repository (repository) ())
(defclass remote-repository (repository)
  ((umbrella :accessor repo-umbrella :initarg :umbrella)
   (method :accessor repo-method :initarg :method)
   (distributor :accessor repo-distributor :initarg :distributor)))

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

(defclass remote-git-repository (remote-repository git-repository) () (:default-initargs :method 'git))
(defclass remote-git-http-repository (remote-git-repository) () (:default-initargs :method 'http))
(defclass remote-darcs-repository (remote-repository darcs-repository) () (:default-initargs :method 'http))
(defclass remote-svn-repository (remote-repository svn-repository) () (:default-initargs :method 'rsync))
(defclass remote-cvs-repository (remote-repository cvs-repository) 
  ((cvs-module :accessor repo-cvs-module :initarg :cvs-module))
  (:default-initargs :method 'rsync))

(defmethod initialize-instance :after ((o remote-cvs-repository) &key cvs-module &allow-other-keys)
  (setf (repo-cvs-module o) (or cvs-module (name o))))

(defgeneric distributor-repo-url (method distributor repo))

(defgeneric path (repo))

(defgeneric url (o)
  (:method ((o local-repository))
    (format nil "file://~A" (namestring (path o))))
  (:method ((o remote-repository))
    (with-output-to-string (s)
           (format s "~(~A~)://" (repo-method o))
           (iter (for x in (distributor-repo-url (repo-method o) (repo-distributor o) o))
                 (princ x s)
                 (princ #\/ s)))))

(defmethod print-object ((o remote-repository) stream)
  (format stream "~@<#<~S ~S>~:@>" (type-of o) (url o)))

(defmethod print-object ((o local-repository) stream)
  (format stream "~@<#<~S ~S>~:@>" (type-of o) (path o)))

(defclass site-repository (local-repository) ())
(defclass user-repository (local-repository) ())

(defclass derived-repository (local-repository)
  ((master :accessor repo-master :initarg :master)
   (last-update-stamp :accessor repo-last-update-stamp :initform 0)))

(defclass local-git-repository (git-repository) ())
(defclass site-derived-git-repository (site-repository derived-repository local-git-repository) ())
(defclass user-derived-git-repository (user-repository derived-repository local-git-repository) ())
(defclass site-origin-git-repository (site-repository local-git-repository) ())
(defclass user-origin-git-repository (user-repository local-git-repository) ())
(defclass local-darcs-repository (derived-repository darcs-repository) ())
(defclass local-svn-repository (derived-repository svn-repository) ())
(defclass local-cvs-repository (derived-repository cvs-repository) ())

(defgeneric repo-pool-root (repo)
  (:method ((o local-git-repository)) (git-pool (module-perspective (repo-module o))))
  (:method ((o local-darcs-repository)) (darcs-pool (module-perspective (repo-module o))))
  (:method ((o local-svn-repository)) (svn-pool (module-perspective (repo-module o))))
  (:method ((o local-cvs-repository)) (cvs-pool (module-perspective (repo-module o)))))

(defun downstring (x)
  (string-downcase (string x)))

(defmethod path ((o local-repository))
    (make-pathname :directory (append (pathname-directory (repo-pool-root o)) (list (downstring (name o))))))

;; what about origin repositories?
(defun perspective-master-repo-typemap (perspective-type)
  (ecase perspective-type
    (gateway-perspective 'site-derived-git-repository)
    (user-perspective 'user-derived-git-repository)))

(defclass module (named depobj)
  ((perspective :accessor module-perspective :initarg :perspective)
   (repositories :accessor module-repositories :initarg :repositories)
   (master-repository :accessor module-master-repository :initarg :master-repo)
   (systems :accessor module-systems :initarg :systems))
  (:default-initargs
   :repositories nil
   :systems nil))

(defmethod print-object ((o module) stream)
  (format stream "#<~@<~S ~S perspective: ~S~:@>>" (type-of o) (slot-value* o 'name) (slot-value* o 'perspective)))

(defun module-core-system (o)
  (declare (type module o))
  (first (module-systems o)))

(defclass system (named)
  ((module :accessor system-module :initarg :module)
   (repository :accessor system-repository :initarg :repository)
   (relativity :accessor system-relativity :initarg :relativity))
  (:default-initargs
   :relativity nil))

(defmethod initialize-instance :after ((o system) &key module &allow-other-keys)
  (setf (system-repository o) (module-master-repository module)))

(defclass application (named)
  ((system :accessor app-system :initarg :system)
   (package :accessor app-package :initarg :package)
   (function :accessor app-function :initarg :function)
   (default-parameters :accessor app-default-parameters :initarg :default-parameters)))

;;;
;;; Root, accessors and coercers: PERSPECTIVE, MODULE, REPO, SYSTEM and APP; COERCE-TO-MODULE and COERCE-TO-SYSTEM.
;;;

(defun coerce-to-name (name)
  (declare (type (or symbol string) name))
  (typecase name
    (symbol (string-upcase (symbol-name name)))
    (string (string-upcase name))))

(defvar *perspectives*)
(defvar *perspective*)

(define-container-hash-accessor *perspectives* perspective :name-transform-fn coerce-to-name)
(define-container-hash-accessor *perspective* module :container-transform modules      :name-transform-fn coerce-to-name :coercer t :mapper t)
(define-container-hash-accessor *perspective* repo   :container-transform repositories :name-transform-fn coerce-to-name :type repository :mapper t :compound-name-p t)
(define-container-hash-accessor *perspective* system :container-transform systems      :name-transform-fn coerce-to-name :coercer t :mapper t)
(define-container-hash-accessor *perspective* app    :container-transform applications :name-transform-fn coerce-to-name :type application)
