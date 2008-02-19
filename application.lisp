(in-package :cling)

(defun define-application (name system package function &rest default-parameters)
  (make-instance 'application :name name :system (system system) :package package :function function :default-parameters default-parameters))

(defun run (os &rest parameters &aux (o (coerce-to-application os)))
  (cling (system-module (app-system o)) :skip-loadable t)
  (apply (symbol-function (find-symbol (string (app-function o)) (app-package o))) (or parameters (app-default-parameters o))))

(defmethod purge-fasls ((o application))
  (mapc #'purge-fasls (module-full-dependencies (system-module (app-system o)))))