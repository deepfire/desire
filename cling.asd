;;; -*- Mode: Lisp -*-

(defpackage :cling.system
  (:use :cl :asdf))

(in-package :cling.system)

(defsystem :cling
  :depends-on (:alexandria :iterate :pergamum :semi-precious :cl-fad)
  :components ((:file "package")
               (:file "filesystem-utils" :depends-on ("package"))
               (:file "types" :depends-on ("package"))
               (:file "legacy-format" :depends-on ("types"))
               (:file "filesystem" :depends-on ("filesystem-utils" "types"))
;;                (:file "asdf" :depends-on ("types" "filesystem"))
               (:file "gittage" :depends-on ("filesystem"))
               (:file "cling" :depends-on ("gittage"))
               
;;                (:file "application" :depends-on ("cling"))
               ))
  