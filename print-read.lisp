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

(defgeneric merge-slot-value (source owner subject subject-slot proposed-source-value)
  (:documentation
   "Merge value of SUBJECT's SUBJECT-SLOT, as defined by OWNER distributor,
in the context of SOURCE distributor proposing PROPOSED-SOURCE-VALUE.
The value returned is the mergeed value of SUBJECT-SLOT in SUBJECT.")
  (:method :around (source (owner null) subject subject-slot proposed-source-value)
    "Special case for newly appearing distributors."
    proposed-source-value)
  (:method :around ((source null) (owner distributor) subject subject-slot proposed-source-value)
    "Special case for initial load of DEFINITIONS."
    proposed-source-value)
  (:method (source owner subject subject-slot proposed-source-value)
    (error "~@<Fell through to default method in MERGE-SLOT-VALUE: ~S wants ~S => into slot ~S of ~S/~S.~:@>" source proposed-source-value subject-slot owner subject))
  (:method :around ((source distributor) owner subject subject-slot proposed-source-value)
    (if *read-time-force-source*
        proposed-source-value
        (call-next-method)))
  (:method ((source distributor) (owner distributor) (subject distributor) (subject-slot (eql 'wishmaster)) proposed-source-value)
    (let ((oval (slot-value subject 'wishmaster)))
      ;; only allow degradation of wishmaster status from himself
      (if (and oval (null proposed-source-value))
          (not (eq source owner))
          proposed-source-value))))

(defmethod print-object ((o distributor) stream)
  (format stream "~@<#D(~;~A~:[~; :WISHMASTER t~] ~@<~S ~S~:@>~;)~:@>"
          (string (name o)) (wishmasterp o) :remotes (distributor-remotes o)))

(defun distributor-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (let ((input-form (read stream nil nil t)))
    (destructuring-bind (name &key wishmaster remotes) input-form
      `(let* ((source *read-time-merge-source-distributor*)
              (owner (distributor ',name :if-does-not-exist :continue))
              (subject owner)
              (wishmaster (merge-slot-value source owner subject 'wishmaster ,wishmaster)))
         (let ((*read-time-enclosing-distributor* (or subject (make-instance 'distributor :name ',name
                                                                             :last-sync-time ,*read-universal-time* :synchronised-p t))))
           (setf (slot-value *read-time-enclosing-distributor* 'wishmaster) wishmaster)
           ,@remotes)))))

(defun system-simple-p (system)
  "Determine whether SYSTEM meets the requirements for a simple system."
  (and (null (system-search-restriction system))
       (null (system-definition-pathname-name system))))

(defun module-simple-p (module)
  "Determine whether MODULE meets the requirements for a simple module."
  (and (eq (name module) (module-umbrella module))
       (not (module-essential-p module))
       (or (null (module-systems module))
           (and (endp (rest (module-systems module)))
                (system-simple-p (first (module-systems module)))))))

(defmethod print-object ((o remote) stream &aux (default-remote-name (with-standard-io-syntax (default-remote-name (name (remote-distributor o)) (vcs-type o)))))
  (let ((*print-case* :downcase))
    (format stream "~@<#R(~;~A ~S~{ ~<~S ~A~:@>~}~;)~:@>"
            (symbol-name
             (if (eq (remote-distributor o) *self*)
                 *original-self-gate-class-name*
                 (type-of o)))
            (remote-path o)
            (multiple-value-bind (simple complex) (unzip #'module-simple-p (location-module-names o) :key #'module)
              (multiple-value-bind (systemful systemless) (unzip #'module-systems simple :key #'module)
                (append (unless (equal default-remote-name (name o))
                          (list `(:name ,(string (name o)))))
                        (when-let ((port (remote-distributor-port o)))
                          (list `(:distributor-port ,port)))
                        (when complex
                          (list `(:complex-modules ,(mapcar #'downstring complex))))
                        (when systemful
                          (list `(:simple-modules ,(mapcar #'downstring systemful))))
                        (when systemless
                          (list `(:systemless-modules ,(mapcar #'downstring systemless))))
                        (when-let ((converted-names (and (typep o 'gate) (gate-converted-module-names o))))
                          (list `(:converted-module-names ,(mapcar #'downstring converted-names))))))))))

(defun system-implied-p (system)
  "See it the definition of SYSTEM is implied, and is therefore subject 
to omission from DEFINITIONS."
  (system-simple-p system))

(defun emit-make-simple-module-form (name)
  `(or (module ',name :if-does-not-exist :continue)
       (make-instance 'module :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :umbrella ',name)))

(defun emit-make-simple-system-form (type module-name name)
  `(or (system ',name :if-does-not-exist :continue)
       (make-instance ',type :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :module (module ',module-name))))

(defun remote-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (type path-components &key name distributor-port complex-modules simple-modules systemless-modules converted-module-names) (read stream nil nil)
    `(let* ((source *read-time-merge-source-distributor*)
            (owner *read-time-enclosing-distributor*)
            (subject (find ',name (distributor-remotes owner) :key #'name))
            (module-names (merge-slot-value source owner subject 'module-names ',(append complex-modules simple-modules systemless-modules)))
            (modules-for-disconnection (when subject (set-difference module-names (location-module-names subject))))
            (converted-module-names (merge-slot-value source owner subject 'converted-module-names ',converted-module-names)))
       (lret ((*read-time-enclosing-remote* (or subject (make-instance ',type ,@(when name `(:name ',name)) :distributor owner :distributor-port ,distributor-port
                                                                       :path ',path-components :module-names module-names
                                                                       ,@(when converted-module-names `(:converted-module-names converted-module-names))
                                                                       :last-sync-time ,*read-universal-time* :synchronised-p t))))
         (setf (slot-value *read-time-enclosing-remote* 'module-names) module-names)
         (when (typep *read-time-enclosing-remote* 'gate)
           (setf (slot-value *read-time-enclosing-remote* 'converted-module-names) converted-module-names))
         (when subject
           (dolist (m modules-for-disconnection)
             (when-let ((m (module m :if-does-not-exist :continue)))
               (remote-unlink-module subject m))))
         (let ((amended-simple-modules (intersection module-names ',simple-modules))
               (amended-systemless-modules (intersection module-names ',systemless-modules)))
           (flet ((maybe-make-system-for-module (m name)
                    (unless (member name amended-systemless-modules)
                      (make-instance *default-system-type* :name name :module m
                                     :last-sync-time ,*read-universal-time* :synchronised-p t))))
             (dolist (m-name (intersection module-names (append amended-simple-modules amended-systemless-modules)))
               (if-let ((m (module m-name :if-does-not-exist :continue)))
                 (maybe-make-system-for-module m m-name)
                 (let ((m (make-instance 'module :name m-name :umbrella m-name
                                         :last-sync-time ,*read-universal-time* :synchronised-p t)))
                   (maybe-make-system-for-module m m-name))))))))))

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
  (format stream "~@<#M(~;~A~{ ~<~S ~A~:@>~}~;)~:@>" (if (eq (name o) (module-umbrella o))
                                                          (symbol-name (name o))
                                                          (list (symbol-name (name o)) (module-umbrella o)))
          (remove nil (multiple-value-call #'list
                        (destructuring-bind (&optional first &rest other-systems) (module-systems o)
                          (unless (and first (null other-systems) (system-simple-p first))
                            (multiple-value-bind (simple complex) (unzip #'system-simple-p (module-systems o))
                              (values (when simple
                                        (list :systems (mapcar #'down-case-name simple)))
                                      (when complex
                                        (list :complex-systems (mapcar #'down-case-name complex)))))))
                        (when (module-essential-p o)
                          (list :essential-p (module-essential-p o)))))))

(defun module-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (name &rest initargs &key (systems nil systems-specified-p) complex-systems &allow-other-keys) (read stream nil nil t)
    (destructuring-bind (name umbrella) (if (consp name) name (list name name))
      `(or (module ',name :if-does-not-exist :continue)
           (prog1 (make-instance 'module :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :umbrella ',umbrella
                                 ,@(remove-from-plist initargs :systems :complex-systems))
             ;; The simple system -- this case is used only when the module is not simple itself, i.e. when it's got a non-default umbrella name.
             ,@(if systems-specified-p
                   (mapcar (curry #'emit-make-simple-system-form *default-system-type* name) systems)
                   ;; Existence of complex systems implies absence of simple systems, by default.
                   (unless complex-systems
                     `(,(emit-make-simple-system-form *default-system-type* name name)))))))))

(defmethod print-object ((o system) stream)
  (format stream "~@<#S(~;~A~{ ~S~}~;)~:@>" (symbol-name (name o))
          (append (list :module (name (system-module o)))
                  (and (system-relativity o) (list :relativity (system-relativity o)))
                  (and (system-search-restriction o) (list :search-restriction (system-search-restriction o)))
                  (and (system-definition-pathname-name o) (list :definition-pathname-name (system-definition-pathname-name o)))
                  (and (system-applications o) (list :applications (system-applications o))))))

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
      (read-definitions definitions))))

(defmethod save-current-definitions (&key seal-p (commit-message "Updated DEFINITIONS") (metastore (meta-path)))
  "Save current model of the world within METASTORE.
When SEAL-P is non-NIL, the changes are committed."
  (with-output-to-new-metafile (definitions 'definitions metastore :commit-p seal-p :commit-message commit-message)
    (serialise-definitions definitions)
    (terpri definitions)))
