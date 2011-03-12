;;; -*- Mode: Lisp; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
(asdf:defsystem :desire
  :depends-on (#+asdf
               :asdf
               #+xcvb
               :xcvb
               ;;
               :alexandria
               :iterate
               :cl-fad
               :split-sequence
               :command-line-arguments
               :com.informatimago.common-lisp.lisp-reader
               ;;
               :pergamum
               :executor)
  :components ((:file "dependencies")
               ;; Tier #
               (:file "package" :depends-on ("dependencies"))
               ;; Tier 0
               (:file "state" :depends-on ("package"))
               ;; Tier 1
               (:file "gittage" :depends-on ("state"))
               ;; Tier 2
               (:file "metastore" :depends-on ("gittage"))
               ;; Tier 3
               (:file "types" :depends-on ("metastore"))
               ;; Tier 4
               (:file "branching" :depends-on ("types"))
               (:file "export" :depends-on ("types"))
               (:file "names" :depends-on ("types"))
               (:file "print-read" :depends-on ("types"))
               (:file "systems" :depends-on ("types"))
               ;; Tier 5
               (:file "modules" :depends-on ("branching" "systems"))
               ;; Tier 6
               (:file "import" :depends-on ("modules"))
               (:file "init" :depends-on ("print-read" "names" "modules" "export"))
               ;; Tier 7
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
