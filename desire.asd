;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :desire.system
  (:use :cl :asdf))

(in-package :desire.system)

(defsystem :desire
  :depends-on (:alexandria :iterate :cl-fad
               :pergamum :executor)
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
               (:file "add-module" :depends-on ("types"))
               (:file "apropos" :depends-on ("types"))
               (:file "print-read" :depends-on ("types"))
               ;; Tier 5
               (:file "xcvb" :depends-on ("system-loadability"))
               (:file "desire" :depends-on ("system-loadability"))
               (:file "application" :depends-on ("system-loadability"))
               (:module "system-quirks" :depends-on ("system-loadability")
                        :components ((:file "cl-launch")
                                     (:file "xcvb")))
               ))
  