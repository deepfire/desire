(defpackage :cling.system
  (:use :cl :asdf))

(in-package :cling.system)

(defsystem :cling
  :depends-on (:alexandria :iterate :pergamum :semi-precious)
  :components ((:file "package")
               (:file "filesystem-utils" :depends-on ("package"))
               (:file "filesystem" :depends-on ("filesystem-utils"))
               (:file "cling" :depends-on ("filesystem"))
               (:file "definitions" :depends-on ("cling"))))