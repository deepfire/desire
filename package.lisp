(defpackage cling
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver)
  (:export
   #:*perspective* #:perspective #:gateway-perspective #:user-perspective #:derive-perspective #:switch-perspective
   #:default-world-readable
   #:map-modules #:map-repositories
   #:defdistributor #:define-module-dependencies #:define-program
   #:module #:repo #:path #:url #:pull #:update #:purge-fasls
   #:asdfly-okay #:ensure-asdfly-okay #:world-readable-p))

(in-package :cling)
