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

(defmethod print-object ((o distributor) stream)
  (format stream "~@<#D(~;~A ~:[~;:wishmaster t~] ~@<~S ~S~:@>~;)~:@>"
          (string (name o)) (wishmasterp o) :remotes (distributor-remotes o)))

(defun distributor-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (name &rest initargs &key wishmaster remotes &allow-other-keys) (read stream nil nil t)
    `(or (distributor ',name :if-does-not-exist :continue)
         (lret ((*read-time-enclosing-distributor* (make-instance 'distributor :name ',name :wishmaster ,wishmaster :last-sync-time ,*read-universal-time* :synchronised-p t ,@(remove-from-plist initargs :remotes))))
           ,@remotes))))

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
            (symbol-name (type-of o)) (remote-path o)
            (multiple-value-bind (simple complex) (unzip #'module-simple-p (location-module-names o) :key #'module)
              (multiple-value-bind (systemful systemless) (unzip #'module-systems simple :key #'module)
                (append (unless (equal default-remote-name (name o))
                          (list `(:name ,(string (name o)))))
                        (when-let ((port (remote-distributor-port o)))
                          (list `(:distributor-port ,port)))
                        (when complex
                          (list `(:modules ,(mapcar #'downstring complex))))
                        (when systemful
                          (list `(:simple-modules ,(mapcar #'downstring systemful))))
                        (when systemless
                          (list `(:simple-systemless-modules ,(mapcar #'downstring systemless))))))))))

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

(defun emit-remote-form (type name modules simple-modules simple-systemless-modules converted-module-names path-components remote-initargs)
  `(prog1
       (make-instance ',type ,@(when name `(:name ',name)) :last-sync-time ,*read-universal-time* :synchronised-p t :distributor *read-time-enclosing-distributor*
                      :path ',path-components :modules ',(append modules simple-modules simple-systemless-modules)
                      ,@(when converted-module-names `(:converted-module-names ',converted-module-names))
                      ,@remote-initargs)
     ,@(mapcar #'emit-make-simple-module-form (append simple-modules simple-systemless-modules))
     ,@(mapcar (lambda (name) (emit-make-simple-system-form *default-system-type* name name)) simple-modules)))

(defun remote-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (type path-components &rest initargs &key modules simple-modules simple-systemless-modules converted-module-names name &allow-other-keys) (read stream nil nil)
    (emit-remote-form type name modules simple-modules simple-systemless-modules converted-module-names path-components
                      (remove-from-plist initargs :name :distributor :type :modules :simple-modules :simple-systemless-modules))))

#+(or)
(defun locality-master-p (o)
  (error "~@<Not implemented.~:@>"))

#+(or)
(defmethod print-object ((o locality) stream)
  (format stream "~@<#L(~;~A ~A ~S~{ ~<~S ~S~:@>~}~;)~:@>"
          (symbol-name (type-of o)) (string (name o)) (locality-path o)
          (append (when (locality-master-p o) (list (list :master-p t)))
                  (when (locality-scan-p o) (list (list :scan-p t))))))

#+(or)
(defun locality-reader (stream &optional char sharp)
  (declare (ignore char sharp))
  (destructuring-bind (type name path &rest initargs &key &allow-other-keys) (read stream nil nil)
    `(make-instance ',type :name ',name :last-sync-time ,*read-universal-time* :synchronised-p t :path ,path ,@(remove-from-plist initargs :path :modules))))

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

(defun serialize-definitions (&optional stream)
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

(defmethod load-definitions (&optional (metastore (meta-path)))
  "Load definitions of the world from METASTORE."
  (with-open-metafile (definitions 'definitions metastore)
    (read-definitions definitions)))

(defmethod save-current-definitions (&key seal-p (commit-message "Updated DEFINITIONS") (metastore (meta-path)))
  "Save current model of the world within METASTORE.
When SEAL-P is non-NIL, the changes are committed."
  (with-output-to-new-metafile (definitions 'definitions metastore :commit-p seal-p :commit-message commit-message)
    (serialize-definitions definitions)
    (terpri definitions)))
