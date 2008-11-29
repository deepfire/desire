;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :desire-tests.system
  (:use :cl :asdf))

(in-package :desire-tests.system)

(defsystem :desire-tests
  :depends-on (:alexandria :iterate :pergamum :desire)
  :components ((:file "test")))
  