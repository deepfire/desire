;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
;;;
;;;  (c) copyright 2007-2011 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :desire)

(define-variable-set knobs
  (*default-bootstrap-wishmaster-name*        "git.feelingofgreen.ru")
  (*bootstrap-wishmaster*                     nil
    "This variable, during bootstrap (only if applicable), is set to the
wishmaster we've bootstrapped from.")
  (*default-bootstrap-wishmaster-http-suffix* "shared/src/")
  (*desires*                                  nil "List of import descriptions.")
  (*default-world-readable*                   t   "Whether to make GIT repositories anonymously accessible by default.")
  (*default-publishable*                      t   "Whether to publish GIT repositories by default.")
  (*self*                                     nil "Possibly unknown distributor whom we identify as.")
  (*combined-remotes-prefer-native-over-http* t   "Whether multi-protocol Git remotes prefer native git protocol to HTTP.")
  (*default-system-type*                      'asdf-system)
  (*merge-remote-wishmasters*                 t   "Whether to merge definitions from remote wishmasters.")
  (*verbose-repository-maintenance*           nil)
  (*verbose-internalisation*                  nil
    "Whether to comment on the progress of matching provided URLs vs. candidate remotes.")
  (*fetch-errors-serious*                     nil
    "Whether to raise an error when external executables fail to fetch modules during DESIRE or UPDATE.  Defaults to NIL."))

(define-variable-set domain
  (*distributors*          (make-hash-table :test 'equal) "Map distributor names to remotes.")
  (*remotes*               (make-hash-table :test 'equal) "Map remote names to remotes.")
  (*localities*            (make-hash-table :test 'equal) "Map names to localities.")
  (*localities-by-path*    (make-hash-table :test 'equal) "Map paths to localities.")
  (*modules*               (make-hash-table :test 'equal) "Map module names to modules.")
  (*leaves*                (make-hash-table :test 'equal) "Map module names to leaf modules.")
  (*nonleaves*             (make-hash-table :test 'equal) "Map module names to nonleaf modules.")
  (*systems*               (make-hash-table :test 'equal) "Map system names to remotes.")
  (*apps*                  (make-hash-table :test 'equal) "Map application names to remotes.")
  (*class-slot-store*      (make-hash-table :test 'equal))
  (*internal-module-names* (mapcar #'canonicalise-name '(".META" ".LOCAL-META"))
    "The list of module names reserved to desire itself.")
  (*credentials*           (alist-hash-table
                            `((anonymous-anonymous . ,(make-cred 'anonymous-anonymous
                                                                 :username "anonymous" :password "anonymous"))
                              (anonymous-empty     . ,(make-cred 'anonymous-empty
                                                                 :username "anonymous" :password nil))
                              (cvspublic-cvspublic . ,(make-cred 'cvspublic-cvspublic
                                                                 :username "cvspublic" :password "cvspublic")))
                            :test 'equal)
                           "Credentials, by name. Not intended to be secure."))

(define-variable-set (constants :apply-defaults t)
  (*desire-version*                 "10.1.1")
  (*supported-vcs-types*            '(git hg darcs cvs svn tarball))
  (*gate-vcs-type*                  'git)
  (*vcs-appendage-types*            '(darcs hg))
  (*system-pathname-typemap*        '(("asd" . asdf-system) ("mb" . mudball-system) ("xcvb" . xcvb-system))
    "The mapping between SYSTEM subclasses and definition pathname types.")
  (*asdf-system-blacklist*          '("cffi-tests" "trivial-features-tests"))
  (*implementation-provided-system-names*
   #+sbcl '("ASDF-INSTALL" "SB-ACLREPL" "SB-BSD-SOCKETS" "SB-CLTL2" "SB-COVER" "SB-EXECUTABLE" "SB-GROVEL"
            "SB-INTROSPECT" "SB-MD5" "SB-POSIX" "SB-QUEUE" "SB-ROTATE-BYTE" "SB-RT" "SB-SIMPLE-STREAMS" "SB-SPROF")
   #-sbcl nil)
  (*libcl-project-index*            "http://libcl.com/libcl-current/index.html")
  (*hg-to-git-location*             #p"/usr/share/doc/git-core/contrib/hg-to-git/hg-to-git.py"))

(define-variable-set assorted-globals
  (*bootstrap-time-component-names* nil
    "Populated by the linearised bootstrap (see LINEARISE-SELF),
for the purpose of INIT-time download and registration of already-loaded components.")
  (*unsaved-definition-changes-p*   nil
    "Whether the idea about the world changed, since INIT was performed,
or SAVE-DEFINITIONS was called.")
  (*source-registry-update-pending* nil)
  (*http-proxy*                     nil)
  (*repository-policies*            (alist-hash-table
                                     `((:default .  ,(make-repository-policy
                                                      :default
                                                      :unsaved-changes           :stash
                                                      :unsaved-changes-postwrite :stash
                                                      :missing-master            :create-on-head
                                                      :preexisting-gitless       :take-over
                                                      :drive-head                nil
                                                      :drive-head-branch         nil
                                                      :reapply-stash             nil))
                                       (:new-repo . ,(make-repository-policy
                                                      :new-repo
                                                      :unsaved-changes           :error
                                                      :unsaved-changes-postwrite :stash
                                                      :missing-master            :error
                                                      :preexisting-gitless       :error
                                                      :drive-head                t
                                                      :drive-head-branch         t
                                                      :reapply-stash             nil))
                                       (:cautious . ,(make-repository-policy
                                                      :cautious
                                                      :unsaved-changes           :error
                                                      :unsaved-changes-postwrite :stash
                                                      :missing-master            :error
                                                      :preexisting-gitless       :error
                                                      :drive-head                nil
                                                      :drive-head-branch         nil
                                                      :reapply-stash             t)))
                                     :test 'eq))
  (*default-repository-policy*      (repository-policy :default))
  (*repository-policy*              *default-repository-policy*))

(defun setup-default-global-state ()
  (initialize-knobs)
  (initialize-domain)
  (initialize-constants)
  (initialize-assorted-globals))