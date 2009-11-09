;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :desire-buildbot.system
  (:use :cl :asdf))

(in-package :desire-buildbot.system)

(defsystem :desire-buildbot
  :depends-on (:alexandria :iterate :pergamum :executor :desire)
  :components ((:file "buildbot")))
