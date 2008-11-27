;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-

(defpackage desire
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver)
  (:import-from :cl-fad #:directory-exists-p #:file-exists-p)
  (:shadow #:*modules*)
  (:export
   ;; types.lisp
   #:distributor #:location #:remote #:locality #:master #:module #:system #:application #:app
   #:git #:hg #:darcs #:cvs #:svn #:rsync #:http
   #:map-distributors #:map-locations #:map-remotes #:map-modules #:map-systems #:map-apps
   #:do-distributors #:do-remotes
   #:serialize-definitions #:read-definitions
   #:module-dependencies #:module-full-dependencies
   #:module-desired-p #:module-desired-remote #:module-remote #:module-distributors #:module-distributor
   #:add-desire #:desire-satisfaction #:desire
   #:*default-world-readable* #:*desires*
   ;; filesystem-utils.lisp
   #:with-dryly-ran-externals
   ;; filesystem.lisp
   #:within-module-repository
   ;; asdf.lisp
   #:system-loadable-p #:system-definition-path #:ensure-system-loadable #:ensure-module-systems-loadable
   ;; gittage.lisp
   #:module-bare-p #:module-world-readable-p
   #:repo-var #:module-gitremotes #:module-add-gitremote #:ensure-module-gitremote
   ;; desire.lisp
   #:define-application 
   #:fetch #:desire
   #:purge-module-binaries))

(in-package :desire)

(require :sb-posix)
