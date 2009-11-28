;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :desire-buildbot.system
  (:use :cl :asdf))

(in-package :desire-buildbot.system)

(defsystem :desire-buildbot
  :depends-on (:alexandria :iterate :pergamum :cl-who :executor :desire)
  :components ((:file "buildbot-base")
               (:file "buildbot-result")
               (:file "buildbot" :depends-on ("buildbot-base" "buildbot-result"))
               (:file "buildbot-presentation" :depends-on ("buildbot"))))
