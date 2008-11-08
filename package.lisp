(defpackage cling
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver)
  (:shadow #:*modules*)
  (:export
   ;; types.lisp
   #:distributor #:location #:remote #:locality #:module #:origin-module #:imported-module #:system #:application
   #:git #:darcs #:cvs #:svn #:rsync #:http
   #:map-distributors #:map-locations #:map-modules #:map-systems #:map-applications
   #:serialize-definitions #:load-definitions
   #:*default-world-readable* #:*desires* #:*force-modules-essential*
   #:desire
   ;; cling.lisp
   #:app #:path #:url
   #:module-direct-dependencies #:module-full-dependencies
   #:defdistributor #:define-module-dependencies #:define-application 
   #:fetch #:update #:load-system #:cling #:purge-fasls #:run
   #:loadable-p #:ensure-loadable #:world-readable-p
   #:init))

(in-package :cling)

(require :sb-posix)
