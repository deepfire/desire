;;; -*- Mode: Lisp -*-

(defpackage :cling.system
  (:use :cl :asdf))

(in-package :cling.system)

(defsystem :cling
  :depends-on (:alexandria :iterate :pergamum :semi-precious)
  :components ((:file "package")
               (:file "filesystem-utils" :depends-on ("package"))
               (:file "types" :depends-on ("package"))
               (:file "legacy-format" :depends-on ("types"))
;;                (:file "filesystem" :depends-on ("filesystem-utils" "types"))
;;                (:file "asdf" :depends-on ("types" "filesystem"))
               (:file "cling" :depends-on ("types" ;; "asdf"
                                                    )
                      )
;;                (:file "application" :depends-on ("cling"))
               ))