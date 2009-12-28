;;; -*- Mode: Lisp; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
(asdf:defsystem :desire
  :depends-on (:alexandria :iterate :cl-fad :pergamum :executor)
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
               (:file "branching" :depends-on ("types"))
               (:file "export" :depends-on ("types"))
               (:file "print-read" :depends-on ("types"))
               (:file "systems" :depends-on ("types"))
               ;; Tier 4
               (:file "modules" :depends-on ("branching" "systems"))
               ;; Tier 5
               (:file "import" :depends-on ("modules"))
               (:file "init" :depends-on ("print-read" "modules" "export"))
               ;; Tier 6
               (:file "recursor" :depends-on ("import" "init"))
               ;; Tier at-the-end-of-it-all
               (:file "add-module" :depends-on ("types"))
               (:file "apropos" :depends-on ("types"))
               (:file "application" :depends-on ("systems"))
               ;; It's probably too bad that the buildslave doesn't exercise the recursor..
               (:file "buildslave" :depends-on ("import" "recursor"))
               (:file "clbuild" :depends-on ("add-module"))
               (:file "libcl" :depends-on ("add-module"))
               (:file "xcvb" :depends-on ("systems"))
               (:module "system-quirks" :depends-on ("systems")
                        :components ((:file "cl-launch")
                                     (:file "xcvb")))
               ))
  