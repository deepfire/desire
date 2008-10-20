(in-package :cling)

(defclass named ()
  ((name :accessor name :initarg :name)))

(defclass registered (named)
  ((registrator :accessor registered-registrator :type function :initarg :registrator)))

(defgeneric fully-qualified-name (o)
  (:documentation "A name which is supposed to be unique within the use domain.")
  (:method ((o registered)) (name o)))

(defmethod initialize-instance :after ((o registered) &key registrator &allow-other-keys)
  "Designed for SETF."
  (funcall registrator o (fully-qualified-name o)))

(defun downstring (x)
  (string-downcase (string x)))

(defvar *perspectives*)
(defvar *perspective* nil)

(defclass perspective (registered)
  ((distributors :accessor distributors :initarg :distributors)
   (modules :accessor modules :initarg :modules)
   (repositories :accessor repositories :initarg :repositories)
   (systems :accessor systems :initarg :systems)
   (applications :accessor applications :initarg :applications)
   (leaves :accessor leaves :initarg :leaves)
   (nonleaves :accessor nonleaves :initarg :nonleaves)
   (git-pool :accessor git-pool :initarg :git-pool)
   (default-world-readable :accessor default-world-readable :initarg :default-world-readable))
  (:default-initargs
   :registrator #'(setf perspective)
   :distributors (make-hash-table :test 'equal)
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

(defclass local-perspective (perspective) ())

(defmethod print-object ((o local-perspective) stream)
  (format stream "~@<#<~S default ~:[non-~;~]world-readable, git: ~S>~:@>"
          (type-of o) (default-world-readable o) (slot-value* o 'git-pool)))

(defmethod initialize-instance :after ((o user-perspective) &key git-pool &allow-other-keys)
  (setf (git-pool o) (or git-pool (namestring (make-pathname :directory (append (pathname-directory (home o)) (list "{asdf}")))))))

(defclass distributor (registered)
  ((url-forms :accessor url-forms :initarg :url-forms)
   (url-fns :accessor url-fns :initarg :url-fns)
   (modules :accessor distributor-modules :initarg :modules)
   (repositories :accessor distributor-repositories :initarg :repositories))
  (:default-initargs
   :registrator #'(setf distributor)
   :modules nil
   :repositories nil))

(defmethod print-object ((o distributor) stream)
  (format stream "~@<#D(~;~A ~S~{ ~S~}~;)~:@>" (symbol-name (name o)) (url-forms o) (list :modules (list 'quote (mapcar #'name (distributor-modules o))))))

(defun distributor-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name url-forms &rest initargs) (read stream nil nil t)
    `(or (distributor ',name :if-does-not-exist :continue)
         (make-instance 'distributor :name ',name :url-forms ',url-forms
                        :url-fns (list ,@(iter (for (schema form) in url-forms)
                                               (collect `(list ',schema (lambda (repo) (list ,@form))))))
                        ,@(remove-from-plist initargs :modules :repositories)))))

(defclass module (registered depobj)
  ((distributor :accessor module-distributor :initarg :distributor)
   (depends-on :accessor module-depends-on :initarg :depends-on)
   (systems :accessor module-systems :initarg :systems)
   (essential-p :accessor module-essential-p :initarg :essential-p))
  (:default-initargs
   :registrator #'(setf module)    
   :distributor nil :depends-on nil :systems nil :repositories nil :essential-p nil))

(defvar *force-modules-essential* nil)

(defmethod print-object ((o module) stream)
  (declare (special *force-modules-essential*))
  (format stream "~@<#M(~;~A~{ ~S~}~;)~:@>" (symbol-name (name o))
          (append (list :distributor (name (module-distributor o)))
                  (when-let (deps (remove-duplicates (append (module-depends-on o) (map-dependencies #'name o))))
                    (list :depends-on (list 'quote deps)))
                  (when-let (systems (module-systems o))
                    (list :systems (and (module-systems o) (list 'quote (mapcar #'name systems)))))
                  (when (or (module-essential-p o) *force-modules-essential*)
                    (list :essential-p t)))))

(defun module-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key distributor &allow-other-keys) (read stream nil nil t)
    `(or (module ',name :if-does-not-exist :continue)
         (make-instance 'module :name ',name :distributor (distributor ',distributor) ,@(remove-from-plist initargs :distributor :systems)))))

(defmethod initialize-instance :after ((o module) &key distributor &allow-other-keys)
  (push o (distributor-modules distributor)))

(defclass system (registered)
  ((module :accessor system-module :initarg :module)
   (applications :accessor system-applications :initarg :applications)
   (relativity :accessor system-relativity :initarg :relativity))
  (:default-initargs
   :registrator #'(setf system)
   :module nil :applications nil :relativity nil))

(defmethod print-object ((o system) stream)
  (format stream "~@<#S(~;~A~{ ~S~}~;)~:@>" (symbol-name (name o))
          (append (list :module (name (system-module o)))
                  (and (system-relativity o) (list :relativity (system-relativity o)))
                  (and (system-applications o) (list :applications (list 'quote (mapcar #'name (system-applications o))))))))

(defun system-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key module &allow-other-keys) (read stream nil nil t)
    `(or (system ',name :if-does-not-exist :continue)
         (make-instance 'system :name ',name :module (module ',module) ,@(remove-from-plist initargs :module :applications)))))

(defmethod initialize-instance :after ((o system) &key module &allow-other-keys)
  (push o (module-systems module)))

(defclass application (registered)
  ((system :accessor app-system :initarg :system)
   (package :accessor app-package :initarg :package)
   (function :accessor app-function :initarg :function)
   (default-parameters :accessor app-default-parameters :initarg :default-parameters))
  (:default-initargs
   :registrator #'(setf app)))

(defmethod print-object ((o application) stream)
  (format stream "~@<#A(~;~A~{ ~A~}~;)~:@>" (symbol-name (name o))
          (list :system (name (app-system o)) :package (string (app-package o)) :function (string (app-function o))
                :default-parameters (list 'quote (app-default-parameters o)))))

(defun application-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key system &allow-other-keys) (read stream nil nil t)
    `(or (application ',name :if-does-not-exist :continue)
         (make-instance 'application :name ',name :system (system ',system) ,@(remove-from-plist initargs :system)))))

(defmethod initialize-instance :after ((o application) &key system &allow-other-keys)
  (push o (system-applications system)))

;;;
;;; Repository headbang
;;;

(defclass repository (registered)
  ((module :accessor repo-module :initarg :module))
  (:default-initargs :registrator #'(setf repo)))

;; override the NAMED slot reader
(defmethod name ((o repository))
  (list (name (repo-module o)) (if (typep o 'remote-repository) 'remote 'local) (etypecase o
                                                                                  (git-repository 'git)
                                                                                  (darcs-repository 'darcs)
                                                                                  (cvs-repository 'cvs)
                                                                                  (svn-repository 'svn))))

(defclass git-repository (repository) ())
(defclass darcs-repository (repository) ())
(defclass svn-repository (repository) ())
(defclass cvs-repository (repository) ())

(defclass local-repository (repository) 
  ((last-time-identity :accessor last-time-identity :initarg nil)))
(defclass remote-repository (repository)
  ((umbrella :accessor repo-umbrella :initarg :umbrella)
   (method :accessor repo-method :initarg :method)
   (distributor :accessor repo-distributor :initarg :distributor))
  (:default-initargs :umbrella nil))

(defclass remote-git-repository (remote-repository git-repository) () (:default-initargs :method 'git))
(defclass remote-git-http-repository (remote-git-repository) () (:default-initargs :method 'http))
(defclass remote-darcs-repository (remote-repository darcs-repository) () (:default-initargs :method 'http))
(defclass remote-svn-repository (remote-repository svn-repository) () (:default-initargs :method 'rsync))
(defclass remote-cvs-repository (remote-repository cvs-repository) 
  ((cvs-module :accessor repo-cvs-module :initarg :cvs-module))
  (:default-initargs :method 'rsync))

(defclass site-repository (local-repository) ())
(defclass user-repository (local-repository) ())

(defclass derived-repository (local-repository)
  ((master :accessor repo-master :initarg :master)
   (last-update-stamp :accessor repo-last-update-stamp :initform 0)))

(defclass local-git-repository (local-repository git-repository) ())
(defclass site-derived-git-repository (site-repository derived-repository local-git-repository) ())
(defclass user-derived-git-repository (user-repository derived-repository local-git-repository) ())
(defclass site-origin-git-repository (site-repository local-git-repository) ())
(defclass user-origin-git-repository (user-repository local-git-repository) ())
(defclass local-darcs-repository (derived-repository darcs-repository) ())
(defclass local-svn-repository (derived-repository svn-repository) ())
(defclass local-cvs-repository (derived-repository cvs-repository) ())

(defun coerce-repo-type-to-mnemonic (type)
  (list (cond ((subtypep type 'local-repository) 'local)
              ((subtypep type 'remote-repository) 'remote)
              (t (error "~@<malformed repository type specifier ~S~:@>" type)))
        (cond ((subtypep type 'git-repository) 'git)
              ((subtypep type 'darcs-repository) 'darcs)
              ((subtypep type 'svn-repository) 'svn)
              ((subtypep type 'cvs-repository) 'cvs)
              (t (error "~@<malformed repository type specifier ~S~:@>" type)))))

(defmethod fully-qualified-name ((o repository))
  (cons (name (repo-module o)) (coerce-repo-type-to-mnemonic (type-of o))))

(defgeneric repo-pool-root (repo)
  (:method ((o local-git-repository)) (git-pool (module-perspective (repo-module o))))
  (:method ((o local-darcs-repository)) (darcs-pool (module-perspective (repo-module o))))
  (:method ((o local-svn-repository)) (svn-pool (module-perspective (repo-module o))))
  (:method ((o local-cvs-repository)) (cvs-pool (module-perspective (repo-module o)))))

(defgeneric path (repo)
  (:method ((o local-repository))
    (make-pathname :directory (append (pathname-directory (repo-pool-root o)) (list (format nil "~(~A~)" (name (repo-module o))))))))

(defgeneric url (o)
  (:method ((o local-repository))
    (format nil "file://~A" (namestring (path o))))
  (:method ((o remote-repository))
    (with-output-to-string (s)
           (format s "~(~A~)://" (repo-method o))
           (iter (for x in (distributor-repo-url (repo-method o) (repo-distributor o) o))
                 (princ x s)
                 (princ #\/ s)))))

(defmethod print-object ((o repository) stream)
  (with-slots (master last-update-stamp umbrella method distributor cvs-module) o
    (format stream "~@<#R(~;~S ~S~{ ~S~}~;)~@:>"
            (type-of o) (name o)
            (etypecase o
              (derived-repository (list :master (name master) :last-update-stamp last-update-stamp))
              (local-repository)
              (remote-cvs-repository (list :umbrella umbrella :method  method :distributor (name distributor) :cvs-module cvs-module))
              (remote-repository (list :umbrella umbrella :method method :distributor (name distributor)))))))

(defun repository-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (type (name ltype rtype) &rest initargs &key distributor master &allow-other-keys) (read stream nil nil t)
    `(or (repo '(,name ,ltype ,rtype)) (make-instance ',type :module (module ',name)
                                                      ,@(when distributor `(:distributor (distributor ',distributor)))
                                                      ,@(when master `(:master (repo ',master)))
                                                      ,@(remove-from-plist initargs :distributor :master)))))

;; (defmethod initialize-instance :after ((o repository) &key module &allow-other-keys)
;;   (push o (module-repositories module)))

(defmethod initialize-instance :after ((o remote-repository) &key distributor &allow-other-keys)
  (push o (distributor-repositories distributor)))

(defmethod initialize-instance :after ((o remote-cvs-repository) &key cvs-module &allow-other-keys)
  (setf (repo-cvs-module o) (or cvs-module (name (repo-module o)))))

;;;
;;; Root, accessors and coercers: PERSPECTIVE, MODULE, REPO, SYSTEM and APP; COERCE-TO-MODULE and COERCE-TO-SYSTEM.
;;;

(defun coerce-to-name (name)
  (declare (type (or symbol string) name))
  (typecase name
    (symbol (string-upcase (symbol-name name)))
    (string (string-upcase name))))

(define-container-hash-accessor *perspectives* perspective :name-transform-fn coerce-to-name)
(define-container-hash-accessor *perspective* distributor :container-transform distributors :name-transform-fn coerce-to-name :coercer t :mapper t)
(define-container-hash-accessor *perspective* module :container-transform modules      :name-transform-fn coerce-to-name :coercer t :mapper t)
(define-container-hash-accessor *perspective* repo   :container-transform repositories :name-transform-fn coerce-to-name :type repository :mapper t :compound-name-p t)
(define-container-hash-accessor *perspective* repo*  :container-transform repositories :name-transform-fn coerce-to-name :type repository :compound-name-p t
                                                     :spread-compound-name-p t)
(define-container-hash-accessor *perspective* system :container-transform systems      :name-transform-fn coerce-to-name :coercer t :mapper t)
(define-container-hash-accessor *perspective* app    :container-transform applications :name-transform-fn coerce-to-name :type application)

(defun load-perspective (stream)
  (let ((*readtable* (copy-readtable))
        (*package* #.*package*))
    (set-dispatch-macro-character #\# #\D 'distributor-reader *readtable*)
    (set-dispatch-macro-character #\# #\M 'module-reader *readtable*)
    (set-dispatch-macro-character #\# #\R 'repository-reader *readtable*)
    (set-dispatch-macro-character #\# #\S 'system-reader *readtable*)
    (set-dispatch-macro-character #\# #\A 'application-reader *readtable*)
    (load stream)))

(defun serialize-perspective (&optional stream (perspective *perspective*))
  (let ((*print-case* :downcase)
        (*package* #.*package*))
    (format stream "~&;;;~%;;; Distributors~%;;;")
    (iter (for (nil d) in-hashtable (distributors perspective)) (print d stream))
    (format stream "~%~%;;;~%;;; Modules~%;;;")
    (iter (for (nil m) in-hashtable (modules perspective)) (print m stream))
    (format stream "~%~%;;;~%;;; Systems~%;;;")
    (iter (for (nil s) in-hashtable (systems perspective)) (print s stream))
    (format stream "~%~%;;;~%;;; Applications~%;;;")
    (iter (for (nil a) in-hashtable (applications perspective)) (print a stream))))
