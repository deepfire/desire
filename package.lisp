(defpackage cling
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver)
  (:import-from :cl-fad #:directory-exists-p #:file-exists-p)
  (:shadow #:*modules*)
  (:export
   ;; types.lisp
   #:distributor #:location #:remote #:locality #:module #:origin-module #:imported-module #:system #:application
   #:git #:hg #:darcs #:cvs #:svn #:rsync #:http
   #:map-distributors #:map-locations #:map-modules #:map-systems #:map-applications
   #:module-dependencies #:module-full-dependencies
   #:serialize-definitions #:read-definitions
   #:*default-world-readable* #:*desires* #:*force-modules-essential*
   #:module-desired-p #:module-desired-remote #:module-remote #:module-distributors #:module-distributor
   #:add-desire #:desire-satisfaction #:desire
   ;; filesystem-utils.lisp
   #:with-dryly-ran-externals
   ;; cling.lisp
   #:app #:path #:url
   #:defdistributor #:define-module-dependencies #:define-application 
   #:fetch #:update #:load-system #:cling #:purge-fasls #:run
   #:loadable-p #:ensure-loadable #:world-readable-p
   #:init))

(in-package :cling)

(require :sb-posix)
