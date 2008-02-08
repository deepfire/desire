(defpackage cling
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver)
  (:export
   #:*perspective* #:perspective #:gateway-perspective #:user-perspective #:derive-perspective #:switch-perspective
   #:default-world-readable
   #:map-modules #:map-repositories #:map-master-repositories
   #:defdistributor #:define-module-dependencies #:define-program
   #:module #:repo #:path #:url #:pull #:update #:purge-fasls))

(in-package :cling)
