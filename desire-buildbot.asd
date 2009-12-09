;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :desire-buildbot.system
  (:use :cl :asdf))

(in-package :desire-buildbot.system)

(defsystem :desire-buildbot
  :depends-on (:alexandria :iterate :pergamum :cl-who :hunchentoot :executor :desire)
  :components ((:file "buildbot-base")
               (:file "buildbot-result" :depends-on ("buildbot-base"))
               (:file "buildbot" :depends-on ("buildbot-result"))
               (:file "buildbot-presentation" :depends-on ("buildbot"))))
