;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :desire.system
  (:use :cl :asdf))

(in-package :desire.system)

(defsystem :desire
  :depends-on (:alexandria :iterate :pergamum :semi-precious :cl-fad)
  :components ((:file "package")
               ;; Tier #
               (:file "dependencies" :depends-on ("package"))
               ;; Tier 0
               (:file "filesystem-utils" :depends-on ("dependencies"))
               ;; Tier 1
               (:file "gittage" :depends-on ("filesystem-utils"))
               ;; Tier 2
               (:file "metastore" :depends-on ("gittage"))
               ;; Tier 3
               (:file "types" :depends-on ("metastore"))
               ;; Tier 4
               (:file "system-loadability" :depends-on ("types"))
               ;; Tier 5
               (:file "desire" :depends-on ("system-loadability"))
;;                (:file "application" :depends-on ("desire"))
               ))
  