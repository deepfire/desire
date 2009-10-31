;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-

(defpackage desire
  (:nicknames :desr)
  (:use :common-lisp :alexandria :pergamum :iterate :depsolver :executor)
  (:import-from :cl-fad #:directory-exists-p #:file-exists-p)
  (:shadow #:*modules*)
  (:export
   ;; types.lisp
   #:distributor #:location #:remote #:locality #:gate #:module #:system #:application #:app
   #:git #:hg #:darcs #:cvs #:svn #:rsync #:http
   ;;   types.lisp :: direct knowledge base manipulation
   #:map-distributors #:map-locations #:map-remotes #:map-modules #:map-systems #:map-apps
   #:do-distributors #:do-remotes #:do-distributor-modules
   #:coerce-to-distributor #:coerce-to-remote #:coerce-to-module #:coerce-to-system #:coerce-to-application
   #:remove-distributor #:remove-remote #:remove-module #:remove-system #:remove-app
   ;;   types.lisp :: distributors
   #:wishmasterp #:do-wishmasters #:do-distributor-remotes #:do-distributor-modules #:compute-distributor-modules
   ;;   types.lisp :: remote
   #:url #:parse-remote-namestring #:remote-disabled-p
   #:credentials #:cred-name #:cred-username #:cred-password #:cred
   ;;   types.lisp :: global UI
   #:clear-definitions #:init #:vcs-enabled-p #:*unsaved-definition-changes-p*
   ;;   types.lisp :: locality
   #:locality-pathname #:module-pathname #:locality-register-with-asdf
   ;;   types.lisp :: module
   #:remote-link-module #:remote-unlink-module #:remote-defines-module-p #:module-best-remote #:module-best-distributor #:module-fetch-url
   #:distributor-module-enabled-remote #:module-locally-present-p
   ;;   types.lisp :: system
   #:system-hidden-p
   ;;   types.lisp :: desires
   #:add-desire #:module-desired-p
   ;;   types.lisp :: knobs
   #:*default-system-type* #:*default-wishmaster* #:*default-world-readable* #:*desires* #:*self* #:*combined-remotes-prefer-native-over-http* #:*desire-root*
   ;;   types.lisp :: conditions
   #:desire-condition #:desire-error #:insatiable-desire #:module-systems-unloadable-error
   ;;   types.lisp :: origin-module
   #:module-status #:module-public-packages #:module-hidden-p
   ;; print-read.lisp
   #:serialise-definitions #:read-definitions #:save-current-definitions #:load-definitions
   ;; system-loadability.lisp
   #:system-loadable-p #:ensure-system-loadable
   #:ensure-module-systems-loadable #:module-systems-unloadable-error
   ;; gittage.lisp
   #:repo-var #:module-gitbranches #:module-gitremotes #:module-add-gitremote #:ensure-module-gitremote
   #:module-bare-p #:module-locally-present-p #:do-present-modules #:module-world-readable-p
   ;; desire.lisp
   #:*register-happy-matches* #:*fetch-errors-serious* #:*register-all-martians*
   #:desire #:lust
   #:define-application 
   #:fetch #:desire
   #:purge-module-binaries
   ;; add-module.lisp
   #:add-distributor #:add-module #:add-module-reader #:install-add-module-reader #:steal-clbuild-projects-file
   ;; apropos.lisp
   #:apropos-desr-list #:apropos-desr #:list-modules))

(in-package :desire)

(require :sb-posix)
