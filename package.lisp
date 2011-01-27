;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-

(defpackage desire
  (:nicknames :desr)
  (:use :common-lisp :alexandria :pergamum :iterate :portable-spawn :executor :elsewhere.0)
  (:import-from :cl-fad #:directory-exists-p #:file-exists-p)
  (:shadow #:*modules*)
  (:export
   ;; dependencies.lisp
   #:name
   ;;   knobs
   #:*verbose-repository-maintenance*
   ;; types.lisp
   #:desirable
   #:distributor
   #:location
   #:remote
   #:locality
   #:gate
   #:module
   #:system
   #:application
   #:app
   #:git
   #:hg
   #:darcs
   #:cvs
   #:svn
   #:rsync
   #:http
   #:host-provided
   #:unknown
   #:asdf
   #:mudballs
   #:xcvb
   #:git-native-remote
   #:git-http-remote
   #:git-combined-remote
   #:hg-http-remote
   #:darcs-http-remote
   #:cvs-rsync-remote
   #:cvs-native-remote
   #:svn-rsync-remote
   #:svn-http-remote
   #:svn-native-remote
   #:tarball-http-remote
   #:gate-native-remote
   #:gate-http-remote
   #:git-locality
   #:hg-locality
   #:darcs-locality
   #:cvs-locality
   #:svn-locality
   #:tarball-locality
   #:git-gate-locality
   #:origin-module
   #:host-system
   #:unknown-system
   #:asdf-system
   #:mudballs-system
   #:xcvb-system
   ;;
   #:canonicalise-name
   ;;   types.lisp :: direct knowledge base manipulation
   #:map-distributors
   #:map-locations
   #:map-remotes
   #:map-modules
   #:map-systems
   #:map-apps
   #:do-distributors
   #:do-remotes
   #:do-modules
   #:do-distributor-modules
   #:coerce-to-distributor
   #:coerce-to-remote
   #:coerce-to-module
   #:coerce-to-system
   #:coerce-to-application
   #:remove-distributor
   #:remove-remote
   #:remove-module
   #:remove-system
   #:remove-app
   ;;   types.lisp :: distributors
   #:wishmasterp
   #:do-wishmasters
   #:do-distributor-remotes
   #:do-distributor-modules
   #:compute-distributor-modules
   ;;   types.lisp :: location
   #:location-module-names
   #:location-defines-module-p
   ;;   types.lisp :: remote
   #:gate-converted-module-names
   #:gate-unpublished-module-names
   #:gate-hidden-module-names
   #:url
   #:parse-remote-namestring
   #:remote-disabled-p
   #:credentials
   #:host-access
   #:cred-name
   #:cred-hostname
   #:cred-username
   #:cred-password
   #:cred
   #:coerce-to-credentials
   #:defcred
   #:defhostaccess
   ;;   types.lisp :: metastore
   #:reset-metastore
   ;;   types.lisp :: global UI
   #:clear-definitions
   #:init
   #:reload-definitions
   #:vcs-enabled-p
   #:*unsaved-definition-changes-p*
   ;;   types.lisp :: locality
   #:loc
   #:locality-pathname
   #:module-pathname
   #:locality-register-with-asdf
   #:hide-module
   #:make-module-unpublished
   #:declare-module-converted
   ;;   types.lisp :: module
   #:remote-link-module
   #:remote-unlink-module
   #:module-best-remote
   #:module-best-distributor
   #:module-fetch-url
   #:module-wrinkle
   #:touch-module
   #:distributor-module-enabled-remote
   #:module-locally-present-p
   #:module-publishable-p
   #:module-hidden-p
   #:find-module-system-definitions
   #:discover-and-register-module-systems
   #:do-present-modules
   ;;   types.lisp :: system
   #:system-module
   #:system-hidden-p
   #:system-host-p
   #:system-known-p
   #:system-locally-present-p
   #:system-makunpresent
   #:system-definition-pathname
   #:system-definition-write-date
   #:do-present-systems
   ;;   types.lisp :: desires
   #:add-desire
   #:module-desired-p
   ;;   types.lisp :: knobs
   #:*default-system-type*
   #:*default-wishmaster*
   #:*default-world-readable*
   #:*default-publishable*
   #:*desires*
   #:*self*
   #:root
   #:*combined-remotes-prefer-native-over-http*
   ;;   types.lisp :: conditions
   #:desire-condition
   #:desire-error
   #:definition-condition
   #:recursor-condition
   #:definition-error
   #:recursor-error
   #:simple-definition-error
   #:simple-recursor-error
   #:distributor-condition
   #:remote-condition
   #:locality-condition
   #:module-condition
   #:system-condition
   #:application-condition
   #:repository-condition
   #:distributor-error
   #:remote-error
   #:locality-error
   #:module-error
   #:system-error
   #:application-error
   #:repository-error
   #:simple-distributor-error
   #:simple-remote-error
   #:simple-locality-error
   #:simple-module-error
   #:simple-system-error
   #:simple-application-error
   #:simple-repository-error 
   #:recursor-progress-halted
   #:counterproductive-system-definition
   #:insatiable-desire
   #:module-systems-unloadable-error
   ;;   types.lisp :: origin-module
   #:module-status
   #:module-public-packages
   ;; print-read.lisp
   #:serialise-definitions
   #:read-definitions
   #:save-definitions
   #:read-definitions
   #:read-local-definitions
   #:unsaved-definition-changes-p
   ;; systems.lisp
   #:undeclared-system-dependency
   #:condition-guilty-set
   #:system-loadable-p
   #:ensure-system-loadable
   #:module-systems-unloadable-error
   ;; modules.lisp
   #:system-name-conflict
   #:module-central-system
   #:module-post-install
   #:sync-module
   ;; gittage.lisp
   #:repo-var
   #:module-git-branches
   #:module-git-remotes
   #:module-add-git-remote
   #:ensure-module-git-remote
   #:module-bare-p
   #:module-locally-present-p
   #:module-world-readable-p
   ;; branching.lisp
   #:*follow-upstream*
   #:*drive-git-masters*
   #:*drive-git-masters-matching-trackers*
   #:*dirty-repository-behaviour*
   ;; import.lisp
   #:update
   ;; recursor.lisp
   #:*fetch-errors-serious*
   #:desire
   #:loadsys
   ;; desire.lisp :: tests
   #:run-module-test
   #:run-test-phase-with-markers
   #:run-test-phases-with-markers
   #:with-output-markers
   #:*beginning-of-output-marker*
   #:*end-of-output-marker*
   #:*beginning-of-result-marker*
   #:*end-of-result-marker*
   ;; add-module.lisp
   #:add-distributor
   #:add-module
   #:add-module-reader
   #:install-add-module-reader
   #:steal-clbuild-projects-file
   #:add-module-local
   ;; apropos.lisp
   #:apropos-desr-list
   #:apropos-desr
   #:list-modules
   #:local-summary))
