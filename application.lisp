(in-package :cling)

(defparameter *applications* (make-hash-table :test 'eq))

(defun defapplication (name module-name package-name function-name &rest default-parameters)
  (setf (gethash name *applications*) (make-instance 'application :module (module module-name)
                                                     :package-name package-name :function-name function-name :default-parameters default-parameters)))

(defun app (name)
  (gethash name *applications*))

(defun run (app &rest parameters)
  (let ((module (app-module app)))
    (unless (asdfly-okay module)
      (update module))
    (asdf:oos 'asdf:load-op (module-asdf-name module))
    (apply (symbol-function (find-symbol (string (app-function-name app)) (app-package-name app))) (or parameters (app-default-parameters app)))))

(defmethod purge-fasls ((o application))
  (mapc #'purge-fasls (module-full-dependencies (app-module o))))