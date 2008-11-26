;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :desire.system
  (:use :cl :asdf))

(in-package :desire.system)

(defsystem :desire
  :depends-on (:alexandria :iterate :pergamum :semi-precious :cl-fad)
  :components ((:file "package")
               ;; Tier 0
               (:file "filesystem-utils" :depends-on ("package"))
               (:file "types" :depends-on ("package"))
               ;; Tier 1
               (:file "filesystem" :depends-on ("filesystem-utils" "types"))
               ;; Tier 2
               (:file "gittage" :depends-on ("filesystem"))
               (:file "asdf" :depends-on ("filesystem"))
               ;; Tier 3
               (:file "desire" :depends-on ("gittage" "asdf"))
;;                (:file "application" :depends-on ("desire"))
               ))
  