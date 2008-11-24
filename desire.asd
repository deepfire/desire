;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :desire.system
  (:use :cl :asdf))

(in-package :desire.system)

(defsystem :desire
  :depends-on (:alexandria :iterate :pergamum :semi-precious :cl-fad)
  :components ((:file "package")
               (:file "filesystem-utils" :depends-on ("package"))
               (:file "types" :depends-on ("package"))
               (:file "filesystem" :depends-on ("filesystem-utils" "types"))
;;                (:file "asdf" :depends-on ("types" "filesystem"))
               (:file "gittage" :depends-on ("filesystem"))
               (:file "desire" :depends-on ("gittage"))
;;                (:file "application" :depends-on ("desire"))
               ))
  