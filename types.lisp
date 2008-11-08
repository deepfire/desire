;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLING; Base: 10 -*-
;;;
;;;  (c) copyright 2007-2008 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

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

(defun down-case-name (x)
  (string-downcase (string (name x))))

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

;;;
;;; Distributor name is its hostname.
;;;
(defclass distributor (registered)
  ((locations :accessor distributor-locations :initarg :locations)
   (url-forms :accessor distributor-url-forms :initarg :url-forms)
   (url-fns :accessor distributor-url-fns :initarg :url-fns))
  (:default-initargs
   :registrator #'(setf distributor) :url-forms nil :url-fns nil
   :locations nil))

(defun distributor-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key locations &allow-other-keys) (read stream nil nil t)
    `(or (distributor ',name :if-does-not-exist :continue)
         (make-instance 'distributor :name ',name :locations (list ,@locations) ,@(remove-from-plist initargs :repositories :locations)))))

(defclass location ()
  ((modules :accessor location-modules :initarg :modules))
  (:default-initargs :modules nil))

(defclass remote (location)
  ((distributor :accessor location-distributor :initarg :distributor)
   (path-form :accessor location-path-form :initarg :path-form)
   (path-fn :accessor location-path-fn :initarg :path-fn)))

(defclass native-remote (remote) ())
(defclass http-remote (remote) ())
(defclass rsync-remote (remote) ())

(defclass locality (location)
  ((path :accessor locality-path :initarg :path)))

(defclass git () ())
(defclass darcs () ())
(defclass cvs () ())
(defclass svn () ())

(defclass git-native-remote (git native-remote) ())
(defclass git-http-remote (git http-remote) ())
(defclass darcs-http-remote (darcs http-remote) ())
(defclass svn-rsync-remote (svn rsync-remote) ())
(defclass cvs-rsync-remote (cvs rsync-remote) ())

(defgeneric schema (location)
  (:method ((git-native-remote o)) "git")
  (:method ((http-remote o)) "http")
  (:method ((rsync-remote o)) "rsync"))

(defun url (location name)
  (declare (type location location) (type symbol name))
  (concatenate 'simple-base-string
               (schema location) "://" (down-case-name (location-distributor location))
               (funcall (location-path-fn location) name)))

(defclass git-locality (git-location locality) ())
(defclass darcs-locality (darcs-location locality) ())
(defclass cvs-locality (cvs-location locality)
  ((lockdir :accessor cvs-locality-lockdir :initarg :lockdir)))
(defclass svn-locality (svn-location locality) ())

(defmethod print-object ((o distributor) stream)
  (format stream "~@<#D(~;~A~{ ~<~S ~S~:@>~}~;)~:@>" (symbol-name (name o)) (list :locations (distributor-locations o))))

(defmethod initialize-instance :after ((o distributor) &rest rest)
  (declare (ignore rest))
  (dolist (loc (distributor-locations o))
    (setf (location-distributor loc) o)))

(defmethod print-object ((o remote) stream &aux (wrapped-p (eq (car (location-path-form o)) 'url)))
  (destructuring-bind ((form-binding) . form-body) (location-path-form o)
   (format stream "~@<#R(~;~A ~A ~@<(~;(~A)~{ ~S~}~;)~:@>~;) ~{ ~<~S ~S~:@>~}~:@>"
           (symbol-name (type-of o)) (symbol-name (xform-if-not #'symbolp #'name (location-distributor o))) form-binding form-body
           (list :modules (mapcar #'down-case-name (location-modules o))))))

(defun remote-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (type distributor ((repovar) &rest path-form) &rest initargs &key modules &allow-other-keys) (read stream nil nil)
    `(make-instance ',type :distributor ',distributor :type ',type :path-form ',(list* 'url (list repovar) path-form) :modules ',modules
                    :path-fn (lambda (,repovar) (list ,@path-form))
                    ,@(remove-from-plist initargs :distributor :type :modules :path-form :path-fn))))

(defclass module (registered depobj)
  ((preferred-distributor :accessor module-preferred-distributor :initarg :preferred-distributor)
   (preferred-remote :accessor module-preferred-remote :initarg :preferred-remote)
   (alternate-remotes :accessor module-alternate-remotes :initarg :alternate-remotes)
   (mirror-locality :accessor module-mirror-locality :initarg :mirror-locality)
   (other-localities :accessor module-other-localities :initarg :other-localities)
   (depends-on :accessor module-depends-on :initarg :depends-on)
   (systems :accessor module-systems :initarg :systems)
   (essential-p :accessor module-essential-p :initarg :essential-p))
  (:default-initargs
   :registrator #'(setf module)    
   :preferred-distributor nil :preferred-remote nil :alternate-remotes nil :mirror-locality nil :other-localities nil
   :depends-on nil :systems nil :essential-p nil))

(defvar *force-modules-essential* nil)

(defmethod print-object ((o module) stream)
  (declare (special *force-modules-essential*))
  (format stream "~@<#M(~;~A~{ ~<~S ~S~:@>~}~;)~:@>" (symbol-name (name o))
          (remove nil (list (list :distributor (name (module-distributor o)))
                            (when-let (deps (remove-duplicates (append (module-depends-on o) (map-dependencies #'name o))))
                              (list :depends-on deps))
                            (when-let (systems (module-systems o))
                              (list :systems (and (module-systems o) (mapcar #'name systems))))
                            (when (or (module-essential-p o) *force-modules-essential*)
                              (list :essential-p t))))))

(defun module-reader (stream &optional sharp char)
  (declare (ignore sharp char))
  (destructuring-bind (name &rest initargs &key distributor depends-on &allow-other-keys) (read stream nil nil t)
    `(or (module ',name :if-does-not-exist :continue)
         (make-instance 'module :name ',name :distributor (distributor ',distributor)
                        ,@(when depends-on `(:depends-on '(,@depends-on)))
                        ,@(remove-from-plist initargs :distributor :systems :depends-on)))))

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
                  (and (system-applications o) (list :applications (mapcar #'name (system-applications o)))))))

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
                :default-parameters (app-default-parameters o))))

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
  (list (name (repo-module o)) (if (typep o 'remote) 'remote 'local) (etypecase o
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

(defclass origin-repository (repository) ())
(defclass derived-repository (local-repository)
  ((master :accessor repo-master :initarg :master)
   (last-update-stamp :accessor repo-last-update-stamp :initform 0)))

(defclass local-git-repository (local-repository git-repository) ())
(defclass site-derived-git-repository (site-repository derived-repository local-git-repository) ())
(defclass user-derived-git-repository (user-repository derived-repository local-git-repository) ())
(defclass site-origin-git-repository (site-repository origin-repository local-git-repository) ())
(defclass user-origin-git-repository (user-repository origin-repository local-git-repository) ())
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

(defun minimise-dependencies (&aux (loops (make-hash-table :test #'equal)))
  (labels ((maybe-remove-nonleaf (name leaf)
             (unless (satisfied-p leaf)
               (remhash name (leaves *perspective*))))
           (minimise (current &optional acc-deps)
             (cond ((member current acc-deps) ;; is there a dependency loop?
                    (push (first acc-deps) (gethash current loops))
                    (undepend current (first acc-deps)))
                   (t
                    (dolist (overdep (intersection (cdr acc-deps) (mapcar #'cadr (hash-table-alist (depsolver::%depobj-dep# current)))))
                      (undepend current overdep))
                    (mapc (rcurry #'minimise (cons current acc-deps)) (mapcar #'cdr (hash-table-alist (depsolver::%depobj-rdep# current))))))))
    (maphash #'maybe-remove-nonleaf (leaves *perspective*))
    (maphash-values #'minimise (leaves *perspective*))
    (iter (for (dependent deplist) in-hashtable loops)
          (mapc (curry #'depend dependent) deplist))))

(defun relink-module-dependencies (o)
  "Leaf processing, dependency relinking."
  (labels ((mark-non-leaf (depkey dep)
             (setf (gethash (coerce-to-name depkey) (nonleaves *perspective*)) dep))
           (mark-maybe-leaf (depeekey depee)
             (setf (gethash (coerce-to-name depeekey) (leaves *perspective*)) depee)))
    (when (module-depends-on o)
      (mark-non-leaf (name o) o))
    (dolist (depname (module-depends-on o))
      (let ((dep (module depname :if-does-not-exist :continue)))
        (unless dep
          (error "~@<Deserialisation failure: can't find ~S's dependency ~S~:@>" o depname))
        (depsolver:depend o dep)
        (mark-maybe-leaf depname dep)))
    (setf (module-depends-on o) nil)
    t))

(defun load-perspective (stream)
  (let ((*readtable* (copy-readtable))
        (*read-eval* nil)
        (*package* #.*package*))
    (set-dispatch-macro-character #\# #\D 'distributor-reader *readtable*)
    (set-dispatch-macro-character #\# #\M 'module-reader *readtable*)
    (set-dispatch-macro-character #\# #\S 'system-reader *readtable*)
    (set-dispatch-macro-character #\# #\A 'application-reader *readtable*)
    (set-dispatch-macro-character #\# #\R 'remote-location-reader *readtable*)
    (load stream)
    (map-modules #'relink-module-dependencies)
    (minimise-dependencies)))

(defvar *locality*)

(defun define-locality (&rest keys &key git-pool &allow-other-keys)
  "Define the default locality.
   {GIT,DARCS,CVS,SVN}-POOL specify the corresponding repository pool roots."
  (unless git-pool
    (error "~@<GIT-POOL must be specified for a useful locality~:@>"))
  (setf *locality* (apply #'make-instance 'locality keys)))

(defun identify-with-distributor (id)
  "Recognize ID's modules as locally hosted by
   creating a corresponding local origin repository."
  (iter (for module in (distributor-modules (distributor id)))
        (for local-origin = (make-instance 'local-git-origin-repository :module module))
        (with-slots (origin-repository local-repository) module
          (setf (values origin-repository local-repository) (values local-origin local-origin)))))

(defun subscribe-to-distributor (id)
  "Mark ID's modules as remotely hosted and build up 
   repository import chains."
  (iter (for module in (distributor-modules (distributor id)))
        (for origin = (make-instance 'local-git-origin-repository :module module))
        (with-slots (origin-repository local-repository) module
          (setf (values origin-repository local-repository) (values local-origin local-origin)))))

(defun serialize-perspective (&optional stream (perspective *perspective*))
  (let ((*print-case* :downcase)
        (*package* #.*package*))
    (format stream "~&;;; -*- Mode: Lisp -*-~%;;;~%;;; Distributors~%;;;")
    (iter (for (nil d) in-hashtable (distributors perspective)) (print d stream))
    (format stream "~%~%;;;~%;;; Modules~%;;;")
    (iter (for (nil m) in-hashtable (modules perspective)) (print m stream))
    (format stream "~%~%;;;~%;;; Systems~%;;;")
    (iter (for (nil s) in-hashtable (systems perspective)) (print s stream))
    (format stream "~%~%;;;~%;;; Applications~%;;;")
    (iter (for (nil a) in-hashtable (applications perspective)) (print a stream))))

(defun test-core-1 (&optional (path-from "/mnt/little/git/cling/definitions.lisp") (path-to "/tmp/essential") (force-essential nil))
  (load path-from)
  (let ((*force-modules-essential* force-essential))
    (with-output-to-file (f path-to)
      (serialize-perspective f))))

(defun test-core-2 (&optional (path-from "/tmp/essential") (path-to "/tmp/essential2") (force-essential nil))
  (setf *perspectives* (make-hash-table :test 'equal)
        *perspective* (make-instance 'gateway-perspective :name 'root))
  (load-perspective path-from)
  (let ((*force-modules-essential* force-essential))
    (with-output-to-file (f path-to)
      (serialize-perspective f))))