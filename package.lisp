;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-

(defpackage desire
  (:nicknames :desr)
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver :executor)
  (:import-from :cl-fad #:directory-exists-p #:file-exists-p)
  (:shadow #:*modules*)
  (:export
   ;; types.lisp
   #:distributor #:location #:remote #:locality #:master #:module #:system #:application #:app
   #:git #:hg #:darcs #:cvs #:svn #:rsync #:http
   #:map-distributors #:map-locations #:map-remotes #:map-modules #:map-systems #:map-apps
   #:do-distributors #:do-remotes #:do-distributor-modules
   #:remove-distributor #:remove-remote #:remove-module #:remove-system #:remove-app
   #:clear-definitions #:serialize-definitions #:read-definitions #:save-current-definitions #:load-definitions
   #:init
   #:remote-disabled-p
   #:locality-path
   #:module-desired-p #:module-enabled-remote #:module-path
   #:module-status #:module-public-packages #:module-hidden-p
   #:url
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
   #:*register-happy-matches* #:*fetch-errors-serious* #:*register-all-martians*
   #:desire #:lust
   #:define-application 
   #:fetch #:desire
   #:purge-module-binaries
   ;; add-module.lisp
   #:add-module #:add-module-reader #:install-add-module-reader
   ;; apropos.lisp
   #:apropor-desr-list #:apropos-desr #:list-modules))

(in-package :desire)

(require :sb-posix)
