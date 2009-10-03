;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-

(defpackage desire
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver :executor)
  (:import-from :cl-fad #:directory-exists-p #:file-exists-p)
  (:shadow #:*modules*)
  (:export
   ;; types.lisp
   #:distributor #:location #:remote #:locality #:master #:module #:system #:application #:app
   #:git #:hg #:darcs #:cvs #:svn #:rsync #:http
   #:map-distributors #:map-locations #:map-remotes #:map-modules #:map-systems #:map-apps
   #:do-distributors #:do-remotes #:do-distributor-modules
   #:clear-definitions #:serialize-definitions #:read-definitions #:save-current-definitions #:load-definitions
   #:init
   #:remote-disabled-p
   #:locality-path
   #:module-desired-p #:module-enabled-remote #:module-path
   #:module-status #:module-public-packages #:module-hidden-p
   #:add-desire
   #:*default-wishmaster* #:*default-world-readable* #:*desires* #:*self* #:*combined-remotes-prefer-native-over-http*
   #:desire-condition #:desire-error #:insatiable-desire #:module-systems-unloadable-error
   ;; asdf.lisp
   #:system-loadable-p #:ensure-system-loadable
   #:ensure-module-systems-loadable #:module-systems-unloadable-error
   ;; gittage.lisp
   #:repo-var #:module-gitbranches #:module-gitremotes #:module-add-gitremote #:ensure-module-gitremote
   #:module-bare-p #:module-present-p #:do-present-modules #:module-world-readable-p
   ;; desire.lisp
   #:*register-happy-matches* #:*register-all-martians*
   #:desire #:desire*
   #:define-application 
   #:fetch #:desire
   #:purge-module-binaries))

(in-package :desire)

(require :sb-posix)
