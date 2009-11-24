;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :desire.system
  (:use :cl :asdf))

(in-package :desire.system)

(defsystem :desire
  :depends-on (:alexandria :iterate :cl-fad
               :pergamum :executor)
  :components ((:file "dependencies")
               ;; Tier #
               (:file "package" :depends-on ("dependencies"))
               ;; Tier 0
               (:file "gittage" :depends-on ("package"))
               ;; Tier 1
               (:file "metastore" :depends-on ("gittage"))
               ;; Tier 2
               (:file "types" :depends-on ("metastore"))
               ;; Tier 3
               (:file "add-module" :depends-on ("types"))
               (:file "apropos" :depends-on ("types"))
               (:file "print-read" :depends-on ("types"))
               (:file "system-loadability" :depends-on ("types"))
               ;; Tier 4
               (:file "application" :depends-on ("system-loadability"))
               (:file "clbuild" :depends-on ("add-module"))
               (:file "desire" :depends-on ("system-loadability"))
               (:file "libcl" :depends-on ("add-module"))
               (:file "xcvb" :depends-on ("system-loadability"))
               (:module "system-quirks" :depends-on ("system-loadability")
                        :components ((:file "cl-launch")
                                     (:file "xcvb")))
               ))
  