(defpackage cling
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver)
  (:export
   #:*perspective* #:perspective #:derive-user-perspective
   #:default-world-readable
   #:defdistributor #:define-module-dependencies #:define-program
   #:module #:repo #:path #:url #:pull #:update #:purge-fasls))

(in-package :cling)
