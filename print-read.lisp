;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
;;;
;;;  (c) copyright 2009-2011 by
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

(in-package :desire)


(defvar *read-universal-time*)
(defvar *read-time-definitions-author*)
(defvar *read-time-enclosing-distributor*)
(defvar *read-time-enclosing-remote*)
(defvar *read-time-force-source* nil)
(defvar *read-time-merge-source-distributor*)
(defparameter *printing-local-definitions* nil)
(defvar *desirable-interpreter-dispatch-table*)

(defun interpret-desirable (desirable-form)
  (destructuring-bind (token &rest args) desirable-form
    (apply (or (cdr (assoc token *desirable-interpreter-dispatch-table*))
               (definition-error "~@<Unknown object token ~A in definition stream.~:@>" token))
           args)))

(defgeneric merge-slot-value (source owner subject subject-slot source-proposed-value)
  (:documentation
   "Merge value of SUBJECT's SUBJECT-SLOT, as defined by OWNER distributor,
in the context of SOURCE distributor proposing SOURCE-PROPOSED-VALUE.
The value returned is the mergeed value of SUBJECT-SLOT in SUBJECT.")
  (:method (source owner subject subject-slot source-proposed-value)
    (definition-error "~@<Fell through to default method in MERGE-SLOT-VALUE: ~S wants ~S => into slot ~S of ~S/~S.~:@>"
        (when source (name source)) source-proposed-value subject-slot (when owner (name owner)) (when subject (name subject))))
  (:method :around ((source distributor) owner subject subject-slot source-proposed-value)
    (if *read-time-force-source*
        source-proposed-value
        (call-next-method)))
  (:method :around (source (owner null) subject subject-slot source-proposed-value)
    "Special case for newly appearing distributors."
    source-proposed-value)
  (:method :around ((source null) (owner distributor) subject subject-slot source-proposed-value)
    "Special case for initial load of DEFINITIONS."
    source-proposed-value)
  (:method :around ((source distributor) (owner distributor) (subject null) subject-slot source-proposed-value)
    "Special case for initial load of DEFINITIONS."
    source-proposed-value)
  (:method ((source distributor) (owner distributor) (subject remote) (subject-slot (eql 'converted-module-names)) source-proposed-value)
    ;; only allow degradation of released module set from owner
    (when (typep subject 'gate)
      (if (eq source owner)
          source-proposed-value
          (union (slot-value subject 'converted-module-names) source-proposed-value))))
  (:method ((source distributor) (owner distributor) (subject remote) (subject-slot (eql 'module-names)) source-proposed-value)
    ;; only allow degradation of converted module set from owner
    (if (eq source owner)
        source-proposed-value
        (union (slot-value subject 'module-names) source-proposed-value))))

(defmethod print-object ((o distributor) stream)
  (format stream "~@<#D(~;~A~{ ~<~S ~A~:@>~}~
                            ~{ ~S~}~
                            ~;)~:@>"
          (string (name o))
          (when (eq *self* o)
            (list `(:identity-p t)))
          (sort (copy-list (distributor-remotes o)) #'string< :key (compose #'string #'name))))

(defun read-distributor (name &rest properties-and-remotes)
  (multiple-value-bind (properties remotes)
      (if-let ((keyposn (position-if #'keywordp properties-and-remotes :from-end t)))
        (values (subseq properties-and-remotes 0 (+ keyposn 2))
                (subseq properties-and-remotes (+ keyposn 2)))
        (values nil properties-and-remotes))
    (destructuring-bind (&key identity-p) properties
      (let* ((owner (distributor name :if-does-not-exist :continue))
             (subject owner))
        (lret* ((d (or subject (make-instance 'distributor :name name
                                              :last-sync-time *read-universal-time*
                                              :synchronised-p t)))
                (*read-time-enclosing-distributor* d)
                (*desirable-interpreter-dispatch-table* '((remote . read-remote))))
          (when identity-p
            (when *read-time-definitions-author*
              (definition-error "~@<Inconsistent DEFINITIONS: ~
                                     two distributors claim authorship.~:@>"))
            (setf *read-time-definitions-author* d))
          (setf (distributor-remotes d)
                (iter (for remote-form in remotes)
                      (collect (interpret-desirable remote-form))))
          (when (wishmasterp d)
            (setf (gate d) (find-if (of-type 'gate) (distributor-remotes d)))))))))

(defun system-simple-p (system)
  "Determine whether SYSTEM meets the requirements for a simple system."
  (and (null (system-definition-pathname-name system))))

(defun module-simple-p (module)
  "Determine whether MODULE meets the requirements for a simple module."
  (and (eq (name module) (module-umbrella module))
       (not (module-system-path-whitelist module))
       (not (module-system-path-blacklist module))
       (or (null (module-systems module)) ; printed as "(foo)"
           (destructuring-bind (first-system &rest others) (module-systems module) ; printed as "foo"
             (and (null others)
                  (eq (name module) (name first-system))
                  (system-simple-p first-system))))))

(defmethod print-object ((o remote) stream &aux (default-remote-name (with-standard-io-syntax (default-remote-name (name (remote-distributor o)) (vcs-type o) (transport o)))))
  (let ((*print-case* :downcase))
    (format stream "~@<#R(~;~A~:[~; ~:*~(~A~)~] ~S~
                            ~:[~; ~:*~<~S ~S~:@>~]~
                            ~{ ~<~S ~A~:@>~}~
                            ~:[~; ~:*~<~S ~S~:@>~]~
                          ~;)~:@>"
            (symbol-name (if (eq *self* (remote-distributor o))
                             (etypecase o
                               (http 'gate-combined-remote)
                               (git  'gate-native-remote))
                             (type-of o)))
            (unless (equal default-remote-name (name o))
              (string (name o)))
            (slot-or-abort-print-object stream o 'path)
            (when-let ((http-path (and (typep o 'combined)
                                       (slot-or-abort-print-object stream o 'http-path))))
              `(:http-path ,http-path))
            (let ((module-names (iter (for m in (sort (remove nil
                                                              (mapcar (rcurry #'module :if-does-not-exist :continue)
                                                                      (slot-or-abort-print-object stream o 'module-names)))
                                                      #'string< :key #'name))
                                      (collect (if (find (name m) (module-systems m) :key #'name)
                                                   (down-case-name m)
                                                   (list (down-case-name m)))))))
              (append (when-let ((port (remote-distributor-port o)))
                        (list `(:distributor-port ,port)))
                      (when (remote-domain-name-takeover o)
                        (list `(:domain-name-takeover t)))
                      (when module-names
                        (list `(:modules ,module-names)))
                      (when-let ((converted-names (and (typep o 'gate)
                                                       (slot-or-abort-print-object stream o 'converted-module-names))))
                        (list `(:converted-module-names ,(sort (mapcar #'downstring converted-names) #'string<))))
                      (when-let ((initial-version (and (typep o 'tarball) (slot-or-abort-print-object stream o 'initial-version))))
                        (list `(:initial-version ,initial-version)))
                      (when-let ((credentials (remote-module-credentials o)))
                        (list `(:credentials ,credentials)))))
            (when-let ((wrinkles (and (typep o 'wrinkle-mixin) (slot-or-abort-print-object stream o 'wrinkles))))
              `(:wrinkles ,wrinkles)))))

(defgeneric merge-remote-type (source owner subject-remote source-proposed-type)
  (:documentation
   "Merge value of SUBJECT-REMOTE's type, as defined by OWNER distributor,
in the context of SOURCE distributor proposing SOURCE-PROPOSED-TYPE.
The value returned is the merged type for SUBJECT-REMOTE.")
  (:method :around ((source distributor) owner subject-remote source-proposed-type)
    (if *read-time-force-source*
        source-proposed-type
        (call-next-method)))
  (:method :around (source owner (subject-remote remote) source-proposed-type)
    (if (eq (type-of subject-remote) source-proposed-type)
        (type-of subject-remote)
        (call-next-method)))
  (:method (source owner subject-remote source-proposed-type)
    (definition-error "~@<Fell through to default method in MERGE-REMOTE-TYPE: distributor ~S wants type ~S for remote ~S of distributor ~S.~:@>"
        (when source (name source)) source-proposed-type (when subject-remote (name subject-remote)) (when owner (name owner))))
  (:method ((s null) (o distributor) (r null) source-proposed-type)
    "Special case for initial load of DEFINITIONS."
    source-proposed-type)
  (:method ((s distributor) (o distributor) (r null) source-proposed-type)
    "Special case for newly appearing remotes."
    source-proposed-type)
  (:method ((s distributor) (o distributor) (r gate-remote) source-proposed-type)
    "Only allow gate remote type changes from an authoritative entity -- the owner."
    (if (eq s o)
        source-proposed-type
        (type-of r)))
  (:method ((s distributor) (o distributor) (r remote) source-proposed-type)
    source-proposed-type))

(defun read-remote (type name path-components &key http-path distributor-port domain-name-takeover modules converted-module-names credentials wrinkles initial-version)
  (let* ((source *read-time-merge-source-distributor*)
         (owner *read-time-enclosing-distributor*)
         ;; compute name, the object and type
         (predicted-name (or name (default-remote-name (name owner) (vcs-type type) (transport type)))) ; note that the vcs type doesn't change due to type merging
         (subject (find predicted-name (distributor-remotes owner) :key #'name))
         (merged-type (merge-remote-type source owner subject type))
         ;; compute modules
         (selfless-modules (mapcar #'car (remove-if-not #'consp modules)))
         (modules (remove-if #'consp modules))
         (merged-module-names (merge-slot-value source owner subject 'module-names (append modules selfless-modules)))
         (modules-for-disconnection (when subject (set-difference merged-module-names (location-module-names subject))))
         (merged-converted-modules (merge-slot-value source owner subject 'converted-module-names converted-module-names)))
    (lret ((*read-time-enclosing-remote*
            (or (when subject (change-class subject merged-type))
                (apply #'make-instance merged-type :distributor owner :distributor-port distributor-port :domain-name-takeover domain-name-takeover
                       :path path-components :module-names merged-module-names
                       :last-sync-time *read-universal-time* :synchronised-p t
                       (append (when name `(:name ,name))
                               (when http-path `(:http-path ,http-path))
                               (when credentials `(:credentials ,credentials))
                               (when wrinkles `(:wrinkles ,wrinkles))
                               (when initial-version `(:initial-version ,initial-version)))))))
      (setf (slot-value *read-time-enclosing-remote* 'module-names) merged-module-names)
      (when (typep *read-time-enclosing-remote* 'gate)
        (setf (slot-value *read-time-enclosing-remote* 'converted-module-names) merged-converted-modules))
      (when subject
        (dolist (m modules-for-disconnection)
          (when-let ((m (module m :if-does-not-exist :continue)))
            (remote-unlink-module subject m))))
      (let ((amended-modules          (intersection merged-module-names modules))
            (amended-selfless-modules (intersection merged-module-names selfless-modules)))
        (dolist (m-name (append amended-selfless-modules amended-modules))
          (let ((m (or (module m-name :if-does-not-exist :continue)
                       ;; Go ahead and opportunistically create a simple module.
                       ;; If there are any amendments, the latter explicit module declaration will take over.
                       (make-instance 'module :name m-name :umbrella m-name
                                      :last-sync-time *read-universal-time* :synchronised-p t))))
            (remote-link-module *read-time-enclosing-remote* m)
            (let ((selfless-p (member m-name amended-selfless-modules)) ; the declaration
                  (has-self-p (system m-name :if-does-not-exist :continue))) ; current status
              (cond ((and selfless-p has-self-p)
                     (remove-system has-self-p))
                    ((not (or selfless-p has-self-p))
                     (make-instance *default-system-type* :name m-name :module m
                                    :last-sync-time *read-universal-time* :synchronised-p t))))))))))

(defun print-gate-local-definitions (gate stream)
  (let ((*print-case* :downcase))
    (block print-object
      (format stream "~@<#L(~;~A~
                              ~{ ~<~S ~A~:@>~}~
                            ~;)~:@>"
              (string (name gate))
              (append
               (when-let ((unpublished-names (slot-or-abort-print-object stream gate 'unpublished-module-names)))
                 (list `(:unpublished ,(sort (mapcar #'downstring unpublished-names) #'string<))))
               (when-let ((hidden-names (slot-or-abort-print-object stream gate 'hidden-module-names)))
                 (list `(:hidden ,(sort (mapcar #'downstring hidden-names) #'string<)))))))))

(defun read-gate-local (name &key unpublished hidden)
  (let ((gate (gate *self*)))
    (unless (eq name (name gate))
      (definition-error "~@<Local definitions claim to apply to gate ~A, whereas we are ~A.~:@>" name (name gate)))
    (unless (every #'symbolp unpublished)
      (definition-error "~@<Non-symbols in list of unpublished modules: ~S~:@>" unpublished))
    (unless (every #'symbolp hidden)
      (definition-error "~@<Non-symbols in list of hidden modules: ~S~:@>" hidden))
    (setf (gate-unpublished-module-names gate) unpublished
          (gate-hidden-module-names gate) hidden)))

(defun module-can-omit-simple-systems-p (module simple-systems complex-systems)
  (and (first simple-systems)
       (endp (cdr simple-systems))
       (null complex-systems)
       (string= (name (first simple-systems)) (name module))))

(defmethod print-object ((o module) stream)
  (format stream "~@<#M(~;~A~{ ~<~(~S ~S~)~:@>~}~;)~:@>" (symbol-name (name o))
          (remove nil (multiple-value-call #'list
                        (unless (eq (name o) (module-umbrella o))
                          (list :umbrella (module-umbrella o)))
                        (destructuring-bind (&optional first &rest other-systems) (module-systems o)
                          (unless (and first (null other-systems) (system-simple-p first) (eq (name first) (name o)))
                            (multiple-value-bind (simple complex) (unzip #'system-simple-p (module-systems o))
                              (values (unless (module-can-omit-simple-systems-p o simple complex)
                                        (list :systems (sort (mapcar #'name simple) #'string< :key #'string)))
                                      (when complex
                                        (list :complex-systems (sort (mapcar #'name complex) #'string< :key #'string)))))))
                        (when-let ((whitelist (module-system-path-whitelist o)))
                          (list :path-whitelist whitelist))
                        (when-let ((blacklist (module-system-path-blacklist o)))
                          (list :path-blacklist blacklist))))))

(defun read-simple-system (type module-name name)
  (or (system name :if-does-not-exist :continue)
      (make-instance type :name name :last-sync-time *read-universal-time* :synchronised-p t :module (module module-name))))

;;; XXX: we need to carry over information, because we /do/ read DEFINITIONS routinely
(defun read-module (name &key (umbrella name) (systems nil systems-specified-p) complex-systems path-whitelist path-blacklist)
  (lret ((m (let* ((preexisting-module (module name :if-does-not-exist :continue))
                   (scan-positive-localities (when preexisting-module
                                               (module-scan-positive-localities preexisting-module))))
              (when preexisting-module
                (remove-module preexisting-module :keep-locations t))
              (apply #'make-instance 'module :name name :last-sync-time *read-universal-time* :synchronised-p t :umbrella umbrella
                     :scan-positive-localities scan-positive-localities
                     (append (when path-whitelist `(:path-whitelist ,path-whitelist))
                             (when path-blacklist `(:path-blacklist ,path-blacklist)))))))
    (do-remotes (r)
      (when (location-defines-module-p r m)
        (pushnew r (module-remotes m))))
    (if systems-specified-p
        (mapcar (curry #'read-simple-system *default-system-type* name) systems)
        ;; Existence of complex systems implies absence of simple systems, by default.
        (unless complex-systems
          (read-simple-system *default-system-type* name name)))))

(defmethod print-object ((o system) stream)
  (format stream "~@<#S(~;~A~{ ~S~}~;)~:@>" (symbol-name (name o))
          (append (list :module (when-let ((module (slot-or-abort-print-object stream o 'module)))
                                  (name module)))
                  (let ((definition-pathname-name (slot-or-abort-print-object stream o 'definition-pathname-name)))
                    (and definition-pathname-name (list :definition-pathname-name definition-pathname-name)))
                  (let ((applications (slot-or-abort-print-object stream o 'applications)))
                   (and applications (list :applications (sort (copy-list applications) #'string< :key (compose #'string #'name))))))))

(defun read-system (name &key module relativity search-restriction definition-pathname-name)
  (or (system name :if-does-not-exist :continue)
      (make-instance *default-system-type* :name name :last-sync-time *read-universal-time* :synchronised-p t :module (module module)
                     :relativity relativity
                     :definition-pathname-name definition-pathname-name
                     :search-restriction search-restriction)))

(defmethod print-object ((o application) stream)
  (format stream "~@<#A(~;~A~{ ~S ~S~}~;)~:@>" (symbol-name (name o))
          (list :system (name (app-system o)) :package (app-package o) :function (app-function o)
                :default-parameters (app-default-parameters o))))

(defun read-application (name &key system package function default-parameters)
  (or (app name :if-does-not-exist :continue)
      (make-instance 'application :name name :last-sync-time *read-universal-time* :synchronised-p t
                     :system (system system) :package package :function function :default-parameters default-parameters)))

;;;
;;; READ path
;;;
(defmacro with-definition-read-context (&body body)
  `(let ((*readtable* (copy-readtable))
         (*read-eval* nil)
         (*read-universal-time* (get-universal-time))
         (*package* #.*package*))
     ,@body))

(defun distributor-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  `(distributor ,@(read stream nil nil t)))

(defun remote-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (let* ((remote-form (read stream nil nil t))
         (type (first remote-form)))
    (multiple-value-bind (name path-components more-args)
        (let ((maybe-name (second remote-form)))
          (if (listp maybe-name)
              (values nil maybe-name (cddr remote-form))
              (values maybe-name (third remote-form) (cdddr remote-form))))
      `(remote ,type ,name ,path-components ,@more-args))))

(defun gate-local-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  `(gate-local ,@(read stream nil nil t)))

(defun module-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  `(module ,@(read stream nil nil t)))

(defun system-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  `(system ,@(read stream nil nil t)))

(defun application-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  `(application ,@(read stream nil nil t)))

(defun read-definitions-from-stream (stream &optional local)
  "Unserialise global definitions from STREAM."
  (with-definition-read-context
    (cond (local
           (set-dispatch-macro-character #\# #\L 'gate-local-reader *readtable*)
           (when-let ((local-gate-def (read stream nil nil)))
             (let ((*desirable-interpreter-dispatch-table*
                    `((gate-local . read-gate-local))))
               (interpret-desirable local-gate-def))))
          (t
           (set-dispatch-macro-character #\# #\D 'distributor-reader *readtable*)
           (set-dispatch-macro-character #\# #\M 'module-reader *readtable*)
           (set-dispatch-macro-character #\# #\S 'system-reader *readtable*)
           (set-dispatch-macro-character #\# #\A 'application-reader *readtable*)
           (set-dispatch-macro-character #\# #\R 'remote-reader *readtable*)
           (let ((*desirable-interpreter-dispatch-table*
                  `((distributor . read-distributor)
                    (module . read-module)
                    (system . read-system)
                    (application . read-application))))
             (iter (for spec = (read stream nil nil))
                   (while spec)
                   (interpret-desirable spec)))))))

(defgeneric read-definitions (&key source force-source metastore)
  (:method (&key (source *self*) (force-source (eq source *self*)) (metastore (meta *self*)))
    "Load definitions of the world from METASTORE."
    (lret ((*read-time-merge-source-distributor* source)
           (*read-time-force-source* force-source)
           *read-time-definitions-author*)
      (with-open-metafile (definitions 'definitions metastore)
        (read-definitions-from-stream definitions))
      (mapc #'ensure-host-system *implementation-provided-system-names*)
      (values))))

(defgeneric read-local-definitions (&key metastore)
  (:method (&key (metastore (localmeta *self*)))
    "Load local definitions from METASTORE."
    (with-open-metafile (definitions 'definitions metastore)
      (read-definitions-from-stream definitions t))
    (values)))

;;;
;;; PRINT path
;;;
(defun serialise-definitions (&optional stream)
  "Serialise global definitions to STREAM."
  (let ((*print-case* :downcase)
        (*package* #.*package*))
    (flet ((sorted-hash-table-entries (hash-table)
             (sort (hash-table-values hash-table) #'string< :key (compose #'string #'name))))
      (format stream "~&;;; -*- Mode: Lisp -*-~%;;;~%;;; Distributors~%;;;")
      (iter (for d in (sorted-hash-table-entries *distributors*)) (print d stream))
      (format stream "~%~%;;;~%;;; Modules~%;;;")
      (iter (for m in (sorted-hash-table-entries *modules*)) (unless (module-simple-p m) (print m stream)))
      (format stream "~%~%;;;~%;;; Systems~%;;;")
      (iter (for s in (sorted-hash-table-entries *systems*)) (unless (system-simple-p s) (print s stream)))
      (format stream "~%~%;;;~%;;; Applications~%;;;")
      (iter (for a in (sorted-hash-table-entries *apps*)) (print a stream)))))

(defun serialise-local-definitions (&optional stream)
  (let ((*print-case* :downcase)
        (*package* #.*package*))
    (format stream "~&;;; -*- Mode: Lisp -*-~%;;;~%;;; Gate unpublished & hidden:~%;;;~%")
    (print-gate-local-definitions (gate *self*) stream)))

(defgeneric save-definitions (&key seal commit-message)
  (:method (&key seal (commit-message "Updated DEFINITIONS"))
    "Save current model of the world within METASTORE.
When SEAL-P is non-NIL, the changes are committed."
    (let ((meta (meta *self*))
          (localmeta (localmeta *self*)))
      (with-output-to-new-metafile (definitions 'definitions meta :commit-p seal :commit-message commit-message)
        (serialise-definitions definitions)
        (terpri definitions))
      (with-output-to-new-metafile (definitions 'definitions localmeta :commit-p seal :commit-message commit-message)
        (serialise-local-definitions definitions)
        (terpri definitions))
      (git-repository-update-for-dumb-servers meta)
      (setf *unsaved-definition-changes-p* nil)
      (values))))

(defun unsaved-definition-changes-p ()
  "See whether there are unsaved changes to DEFINITIONS."
  (git-repository-changes-p (meta *self*)))

;;;
;;; Automated desire bootstrap generation
;;;
(defparameter *impl-components*
  (alist-hash-table
   `((:sbcl :sb-grovel :sb-posix)
     (:ecl  :cmp))))

(define-root-container *impl-components* impl-components :key-type :keyword :type list)

(defun this-impl-components ()
  (impl-components
   #+ecl  :ecl
   #+sbcl :sbcl))

(defun self-spectacle ()
  "What is needed to be done, in order to load self?"
  #+asdf
  (mapcar #'cdr
          (remove-if-not (of-type 'asdf:load-op)
                         (asdf::traverse (make-instance 'asdf:load-op :force :all)
                                         (asdf:find-system :desire))
                         :key #'car)))

(defun component-parent      (x) #+asdf (asdf:component-parent x))
(defun component-name        (x) #+asdf (asdf:component-name x))
(defun component-filename    (x) #+asdf (asdf:component-pathname x))
(defun component-source-p    (x) #+asdf (typep x 'asdf:cl-source-file))
(defun component-system-p    (x) #+asdf (typep x 'asdf:system))
(defun impl-component-p      (x) #+asdf (member (string-upcase (asdf:component-name x)) (this-impl-components)
                                                :test #'string=))

(defun relevant-source-component-p (x)
  (and (component-source-p x)
       (not (impl-component-p (component-parent x)))))

(defparameter *boot-forms* nil
  "A list of boot forms to be prepended in the linearised,
bootstrap-geared, single-file form of desire.")

(defmacro defboot (&rest forms)
  (if (endp (cdr forms))
      `(push ,(first forms) *boot-forms*)
      `(dolist (f (list ,@forms))
         (push f *boot-forms*))))

(defmacro defbootvar (name value &optional (documentation ""))
  (declare (ignore documentation))
  `(defboot '(defvar ,name ,value)))

(defmacro defbootfun (name (&rest lambda-list) &body body)
  `(defboot '(defun ,name ,lambda-list ,@body)))

(defmacro defbootmacro (name (&rest lambda-list) &body body)
  `(defboot '(defmacro ,name ,lambda-list ,@body)))

;;;
;;; The linearised desire's bootstrap sequence:
;;;
(defboot "#+sbcl"
         "(declaim (sb-ext:muffle-conditions sb-ext:code-deletion-note sb-ext:compiler-note style-warning))"
         "#+ecl"  `(mapcar #'require ',(impl-components :ecl))
         "#+sbcl" `(mapcar #'require ',(impl-components :sbcl))
         #+asdf
         '(require :asdf)
         '(write-string "; desire boot: (                    )")
         '(finish-output))

(defbootvar *completed* 0)

(defbootfun debugp ()
  #+asdf (asdf::getenv "DESIRE_DEBUG"))

(defbootfun update-progress (total &aux (width 20.0))
  (write-char #\Return)
  (write-string "; desire boot: ")
  (write-char #\()
  (incf *completed*)
  (let ((barrier (/ (* width *completed*) total)))
    (dotimes (i (floor (* width *completed*) total))
      (write-char #\.))
    (write-char (if (= total *completed*) #\: #\>))
    (dotimes (i (ceiling (* width (- total *completed*)) total))
      (write-char #\Space)))
  (write-char #\))
  (finish-output))

(defboot "#-(or ecl)")
(defbootmacro with-no-noise-impl-dependent (() &body body)
  `(progn ,@body))

(defboot "#+ecl
 (defmacro with-no-noise-impl-dependent (() &body body)
   `(handler-bind ((c::compiler-note #'muffle-warning)) ,@body))")

(defbootmacro with-no-noise ((noisep) &body body)
  `(flet ((body () ,@body))
     (if ,noisep
         (body)
         (let (*compile-verbose* *compile-print* *load-verbose*)
           (handler-bind ((style-warning #'muffle-warning))
             (with-no-noise-impl-dependent ()
               (body)))))))

(defbootfun load-as-compilation-unit (file-name/content-pairs &aux (files-nr (length file-name/content-pairs)))
  (let ((temp-file #p"temp.lisp")
        (verbosep (debugp)))
    (with-no-noise (verbosep)
      (with-compilation-unit ()
        (loop :for (orig-name content) :in file-name/content-pairs :do
           (with-open-file (f temp-file :direction :output
                              :if-does-not-exist :create :if-exists :supersede)
             (when verbosep
               (format f "~S~%" `(progn
                                   (format t "~%; desire boot, loading ~S~%" ;
                                           (namestring ,orig-name))
                                   (finish-output))))
             (write-string content f)
             (finish-output f))
           (multiple-value-bind (fasl warningsp errorsp) (compile-file temp-file)
             (declare (ignore warningsp))
             (if errorsp
                 (error "~@<Caught an error, while compiling ~S.~:@>" orig-name)
                 (load fasl)))
           (update-progress files-nr))))))

(defun linearise-self (filename)
  (let* ((*package* (find-package :desire))
         (*print-case* :downcase)
         (spectacle (self-spectacle))
         (spectacle-source-components (remove-if-not #'relevant-source-component-p spectacle)))
    (with-output-to-file (f filename)
      (flet ((to-stringer (xs)
               (mapcar (lambda (x) (list (stringp x) x)) xs)))
        (format f ";;; -*- Mode: Lisp -*-~%~
                   ;;;~%~%~
                   ~:{~:[~S~;~A~]~%~}~
                   (load-as-compilation-unit~%~
                   `(~:{(~S~%~S)~%~}))~%~%~
                   ~:{~:[~S~;~A~]~%~}~%"
                (append
                 (to-stringer (reverse *boot-forms*))
                 ;; should we need to upgrade ASDF, it's here..
                 ;; NOTE: remember to fetch the thing from the wishmaster!
                 `(#+(and nil asdf)
                     ,(write-to-string `(when (char= #\1 (char asdf::*asdf-version* 0))
                                          (load-system
                                           `((#p"src/asdf2/asdf.lisp"
                                                ,,(file-as-string (merge-pathnames "asdf.lisp" (module-pathname :asdf))))))))))
                (iter (for c in spectacle-source-components)
                      (let ((filename (component-filename c)))
                        (collect (list filename (file-as-string filename)))))
                (to-stringer nil)))
      (let ((*package* (find-package :common-lisp-user)))
        (syncformat
         f "~@{~S~%~%~}"
         `(setf *bootstrap-time-component-names*
                ',(mapcar (compose #'make-keyword #'string-upcase #'component-name)
                          (remove-if-not (lambda (x) (and (component-system-p x)
                                                          (not (impl-component-p x))))
                                         spectacle)))
         '(fresh-line)
         '(init))))))
