;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2009 by
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
(defvar *read-time-enclosing-distributor*)
(defvar *read-time-enclosing-remote*)
(defvar *read-time-force-source* nil)
(defvar *read-time-merge-source-distributor*)

(defgeneric merge-slot-value (source owner subject subject-slot source-proposed-value)
  (:documentation
   "Merge value of SUBJECT's SUBJECT-SLOT, as defined by OWNER distributor,
in the context of SOURCE distributor proposing SOURCE-PROPOSED-VALUE.
The value returned is the mergeed value of SUBJECT-SLOT in SUBJECT.")
  (:method (source owner subject subject-slot source-proposed-value)
    (error "~@<Fell through to default method in MERGE-SLOT-VALUE: ~S wants ~S => into slot ~S of ~S/~S.~:@>"
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
  (format stream "~@<#D(~;~A~{ ~S~}~;)~:@>"
          (string (name o)) (sort (copy-list (distributor-remotes o)) #'string< :key (compose #'string #'name))))

(defun distributor-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (let ((input-form (read stream nil nil t)))
    (destructuring-bind (name &rest remotes) input-form
      `(let* ((owner (distributor ',name :if-does-not-exist :continue))
              (subject owner))
         (lret* ((d (or subject (make-instance 'distributor :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t)))
                 (*read-time-enclosing-distributor* d))
           ,@remotes
           (when (wishmasterp d)
             (setf (gate d) (find-if (of-type 'gate) (distributor-remotes d)))))))))

(defun system-simple-p (system)
  "Determine whether SYSTEM meets the requirements for a simple system."
  (and (null (system-search-restriction system))
       (null (system-definition-pathname-name system))))

(defun module-simple-p (module)
  "Determine whether MODULE meets the requirements for a simple module."
  (and (eq (name module) (module-umbrella module))
       (not (module-essential-p module))
       (not (module-system-path-whitelist module))
       (not (module-system-path-blacklist module))
       (or (null (module-systems module))
           (and (endp (rest (module-systems module)))
                (system-simple-p (first (module-systems module)))))))

(defmethod print-object ((o remote) stream &aux (default-remote-name (with-standard-io-syntax (default-remote-name (name (remote-distributor o)) (vcs-type o) (transport o)))))
  (let ((*print-case* :downcase))
    (format stream "~@<#R(~;~A ~S~
                            ~{ ~<~S ~A~:@>~}~
                            ~:[~; ~:*~<~S ~S~:@>~]~
                          ~;)~:@>"
            (symbol-name
             (or (when (eq (remote-distributor o) *self*)
                   *original-self-gate-class-name*)
                 (type-of o)))
            (slot-or-abort-print-object stream o 'path)
            (multiple-value-bind (systemful systemless) (unzip #'module-systems (slot-or-abort-print-object stream o 'module-names) :key #'module)
              (append (unless (equal default-remote-name (name o))
                        (list `(:name ,(string (name o)))))
                      (when-let ((port (remote-distributor-port o)))
                        (list `(:distributor-port ,port)))
                      (when (remote-domain-name-takeover o)
                        (list `(:domain-name-takeover t)))
                      (when systemful
                        (list `(:modules ,(sort (mapcar #'downstring systemful) #'string<))))
                      (when systemless
                        (list `(:systemless-modules ,(sort (mapcar #'downstring systemless) #'string<))))
                      (when-let ((converted-names (and (typep o 'gate) (slot-or-abort-print-object stream o 'converted-module-names))))
                        (list `(:converted-module-names ,(sort (mapcar #'downstring converted-names) #'string<))))))
            (when-let ((cvs-module-modules (and (typep o 'cvs) (slot-or-abort-print-object stream o 'module-modules))))
              `(:cvs-module-modules ,cvs-module-modules)))))

(defun system-implied-p (system)
  "See it the definition of SYSTEM is implied, and is therefore subject 
to omission from DEFINITIONS."
  (system-simple-p system))

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
    (error "~@<Fell through to default method in MERGE-REMOTE-TYPE: distributor ~S wants type ~S for remote ~S of distributor ~S.~:@>"
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

(defun remote-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (type path-components &key name distributor-port domain-name-takeover modules systemless-modules converted-module-names cvs-module-modules) (read stream nil nil)
    `(let* ((source *read-time-merge-source-distributor*)
            (owner *read-time-enclosing-distributor*)
            (predicted-name (or ',name (default-remote-name (name owner) ',(vcs-type type) ',(transport type)))) ; note that the vcs type doesn't change due to type merging
            (subject (find predicted-name (distributor-remotes owner) :key #'name))
            (type (merge-remote-type source owner subject ',type))
            (module-names (merge-slot-value source owner subject 'module-names ',(append modules systemless-modules)))
            (modules-for-disconnection (when subject (set-difference module-names (location-module-names subject))))
            (converted-module-names (merge-slot-value source owner subject 'converted-module-names ',converted-module-names)))
       (lret ((*read-time-enclosing-remote* (or (when subject (change-class subject type))
                                                (make-instance type ,@(when name `(:name ',name)) :distributor owner :distributor-port ,distributor-port :domain-name-takeover ',domain-name-takeover
                                                               :path ',path-components :module-names module-names
                                                               :last-sync-time ,*read-universal-time* :synchronised-p t
                                                               ,@(when cvs-module-modules `(:module-modules ',cvs-module-modules))))))
         (setf (slot-value *read-time-enclosing-remote* 'module-names) module-names)
         (when (typep *read-time-enclosing-remote* 'gate)
           (setf (slot-value *read-time-enclosing-remote* 'converted-module-names) converted-module-names))
         (when subject
           (dolist (m modules-for-disconnection)
             (when-let ((m (module m :if-does-not-exist :continue)))
               (remote-unlink-module subject m))))
         (let ((amended-modules (intersection module-names ',modules))
               (amended-systemless-modules (intersection module-names ',systemless-modules)))
           (dolist (m-name (intersection module-names (append amended-modules amended-systemless-modules)))
             (lret ((m (or (module m-name :if-does-not-exist :continue)
                           (make-instance 'module :name m-name :umbrella m-name
                                          :last-sync-time ,*read-universal-time* :synchronised-p t))))
               (remote-link-module *read-time-enclosing-remote* m)
               (unless (or (system m-name :if-does-not-exist :continue)
                           (member m-name amended-systemless-modules))
                 (make-instance *default-system-type* :name m-name :module m
                                :last-sync-time ,*read-universal-time* :synchronised-p t)))))))))

#+(or)
(defun locality-master-p (o)
  (error "~@<Not implemented.~:@>"))

#+(or)
(defmethod print-object ((o locality) stream)
  (format stream "~@<#L(~;~A ~A ~S~{ ~<~S ~S~:@>~}~;)~:@>"
          (symbol-name (type-of o)) (string (name o)) (locality-pathname o)
          (append (when (locality-master-p o) (list (list :master-p t)))
                  (when (locality-scan-p o) (list (list :scan-p t))))))

#+(or)
(defun locality-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (type name path &rest initargs &key &allow-other-keys) (read stream nil nil)
    `(make-instance ',type :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :pathname ,path ,@(remove-from-plist initargs :path :modules))))

(defmethod print-object ((o module) stream)
  (format stream "~@<#M(~;~A~{ ~<~(~S ~S~)~:@>~}~;)~:@>" (symbol-name (name o))
          (remove nil (multiple-value-call #'list
                        (unless (eq (name o) (module-umbrella o))
                          (list :umbrella (module-umbrella o)))
                        (destructuring-bind (&optional first &rest other-systems) (module-systems o)
                          (unless (and first (null other-systems) (system-simple-p first) (eq (name first) (name o)))
                            (multiple-value-bind (simple complex) (unzip #'system-simple-p (module-systems o))
                              (values (when simple
                                        (list :systems (sort (mapcar #'name simple) #'string< :key #'string)))
                                      (when complex
                                        (list :complex-systems (sort (mapcar #'name complex) #'string< :key #'string)))))))
                        (when-let ((whitelist (module-system-path-whitelist o)))
                          (list :path-whitelist whitelist))
                        (when-let ((blacklist (module-system-path-blacklist o)))
                          (list :path-blacklist blacklist))
                        (when (module-essential-p o)
                          (list :essential-p (module-essential-p o)))))))

(defun emit-make-simple-system-form (type module-name name)
  `(or (system ',name :if-does-not-exist :continue)
       (make-instance ',type :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :module (module ',module-name))))

(defun module-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (name &rest initargs &key (umbrella name) (systems nil systems-specified-p) complex-systems path-whitelist path-blacklist &allow-other-keys)
      (read stream nil nil t)
    `(lret ((m (or (when-let ((existing-module (module ',name :if-does-not-exist :continue)))
                     (remove-module existing-module :keep-localities t)
                     nil)                   
                   (make-instance 'module :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :umbrella ',umbrella
                                  ,@(when path-whitelist `(:path-whitelist ',path-whitelist))
                                  ,@(when path-blacklist `(:path-blacklist ',path-blacklist))
                                  ,@(remove-from-plist initargs :umbrella :systems :complex-systems :path-whitelist :path-blacklist)))))
       (do-remotes (r)
         (when (remote-defines-module-p r m)
           (pushnew r (module-remotes m))))
       ,@(if systems-specified-p
             (mapcar (curry #'emit-make-simple-system-form *default-system-type* name) systems)
             ;; Existence of complex systems implies absence of simple systems, by default.
             (unless complex-systems
               `(,(emit-make-simple-system-form *default-system-type* name name)))))))

(defmethod print-object ((o system) stream)
  (format stream "~@<#S(~;~A~{ ~S~}~;)~:@>" (symbol-name (name o))
          (append (list :module (name (system-module o)))
                  (and (system-relativity o) (list :relativity (system-relativity o)))
                  (and (system-search-restriction o) (list :search-restriction (system-search-restriction o)))
                  (and (system-definition-pathname-name o) (list :definition-pathname-name (system-definition-pathname-name o)))
                  (and (system-applications o) (list :applications (sort (copy-list (system-applications o)) #'string< :key (compose #'string #'name)))))))

(defun system-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (name &rest initargs &key module relativity search-restriction &allow-other-keys) (read stream nil nil t)
    `(or (system ',name :if-does-not-exist :continue)
         (make-instance *default-system-type* :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :module (module ',module) :relativity ',relativity
                        ,@(when search-restriction `(:search-restriction ',search-restriction))
                        ,@(remove-from-plist initargs :module :applications :relativity :search-restriction)))))

(defmethod print-object ((o application) stream)
  (format stream "~@<#A(~;~A~{ ~S ~S~}~;)~:@>" (symbol-name (name o))
          (list :system (name (app-system o)) :package (app-package o) :function (app-function o)
                :default-parameters (app-default-parameters o))))

(defun application-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (name &rest initargs &key system package function default-parameters &allow-other-keys) (read stream nil nil t)
    `(or (app ',name :if-does-not-exist :continue)
         (make-instance 'application :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t
                        :system (system ',system) :package ',package :function ',function :default-parameters ',default-parameters
                        ,@(remove-from-plist initargs :system :package :function :default-parameters)))))

(defmacro with-definition-read-context (&body body)
  `(let ((*readtable* (copy-readtable))
         (*read-eval* nil)
         (*read-universal-time* (get-universal-time))
         (*package* #.*package*))
     ,@body))

(defun read-definitions (stream)
  "Unserialise global definitions from STREAM."
  (with-definition-read-context
    (set-dispatch-macro-character #\# #\D 'distributor-reader *readtable*)
    (set-dispatch-macro-character #\# #\M 'module-reader *readtable*)
    (set-dispatch-macro-character #\# #\S 'system-reader *readtable*)
    (set-dispatch-macro-character #\# #\A 'application-reader *readtable*)
    (set-dispatch-macro-character #\# #\R 'remote-reader *readtable*)
    #+(or) (set-dispatch-macro-character #\# #\L 'locality-reader *readtable*)
    (load stream)))

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
      (iter (for s in (sorted-hash-table-entries *systems*)) (unless (system-implied-p s) (print s stream)))
      (format stream "~%~%;;;~%;;; Applications~%;;;")
      (iter (for a in (sorted-hash-table-entries *apps*)) (print a stream)))))

(defmethod load-definitions (&key (source *self*) (force-source (eq source *self*)) (metastore (meta-path)))
  "Load definitions of the world from METASTORE."
  (let ((*read-time-merge-source-distributor* source)
        (*read-time-force-source* force-source))
    (with-open-metafile (definitions 'definitions metastore)
      (read-definitions definitions))
    (values)))

(defmethod save-current-definitions (&key seal (commit-message "Updated DEFINITIONS") (metastore (meta-path)))
  "Save current model of the world within METASTORE.
When SEAL-P is non-NIL, the changes are committed."
  (with-output-to-new-metafile (definitions 'definitions metastore :commit-p seal :commit-message commit-message)
    (serialise-definitions definitions)
    (terpri definitions))
  (setf *unsaved-definition-changes-p* nil)
  (values))
