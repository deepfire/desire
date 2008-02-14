(defpackage cling
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver)
  (:export
   #:*perspective* #:perspective #:module #:repo #:system #:app #:path #:url
   #:gateway-perspective #:user-perspective #:derive-perspective #:switch-perspective #:default-world-readable
   #:map-modules #:map-repositories #:map-systems #:map-applications
   #:module-direct-dependencies #:module-full-dependencies
   #:defdistributor #:define-module-dependencies #:define-application 
   #:fetch #:update #:load-system #:cling #:purge-fasls #:run
   #:loadable #:ensure-loadable #:world-readable-p
   #:init))

(in-package :cling)

(require :sb-posix)
