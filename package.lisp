(defpackage cling
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver)
  (:export
   ;; types.lisp
   #:perspective #:gateway-perspective #:user-perspective #:local-perspective #:*perspective* #:default-world-readable
   #:distributor #:location #:remote-location #:locality #:module #:essential-module #:system #:application
   #:repository #:git #:darcs #:cvs #:svn #:remote #:local
   #:git-repository #:darcs-repository #:cvs-repository #:svn-repository #:remote-repostitory #:local-repository
   #:map-distributors #:map-modules #:map-systems #:map-applications #:map-repositories
   #:serialize-perspective #:load-perspective
   #:*force-modules-essential*
   ;; cling.lisp
   #:repo #:system #:app #:path #:url
   #:derive-perspective #:switch-perspective
   #:module-direct-dependencies #:module-full-dependencies
   #:defdistributor #:define-module-dependencies #:define-application 
   #:fetch #:update #:load-system #:cling #:purge-fasls #:run
   #:loadable-p #:ensure-loadable #:world-readable-p
   #:init))

(in-package :cling)

(require :sb-posix)
