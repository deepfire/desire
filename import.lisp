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


(define-reported-condition fetch-failure (module-error remote-error)
  ((execution-error :reader condition-execution-error :initarg :execution-error))
  (:report (remote module execution-error)
           "~@<An attempt to fetch module ~S from ~S has failed.~@:_~S~:@>"
           (coerce-to-name module) (string-id remote) execution-error))
(define-reported-condition repository-not-clean-during-fetch (repository-error executable-failure) ()
  (:report (locality module)
           "~@<Repository for ~S in ~S has uncommitted changes during fetch.~:@>" (coerce-to-name module) locality))
(define-reported-condition dirt-files-in-repository (repository-error)
  ((dirt-files :accessor condition-dirt-files :initarg :dirt-files))
  (:report (dirt-files module locality)
           "~@<Dirt files ~S prevented from importing ~A in ~S.~@:>" dirt-files (coerce-to-name module) (module-pathname module locality)))
(define-reported-condition empty-repository (repository-error)
  ()
  (:report (pathname) "~@<Repository in ~S has no objects.~:@>" pathname))

(progn
  (define-executable darcs)
  (define-executable darcs-fast-export)
  (define-executable hg)
  (define-executable hg-fast-export)
  (define-executable python)            ; this is for hg-to-git.py
  (define-executable rsync)
  (define-executable cvs)
  ;; these are needed for XCVB stack's postinstall
  (define-executable cp)
  (define-executable make))

(defgeneric touch-remote-module (remote module)
  (:method :around ((o remote) name)
    (with-executable-options (:explanation `("attempting to touch module ~A in ~S" ,(coerce-to-name name) ,(url o name)))
      (call-next-method)))
  (:method ((o git-remote) name)
    (with-valid-exit-codes ((128 nil)) (git "." "peek-remote" (url o name))))
  (:method ((o darcs-http-remote) name)
    (or (touch-www-file `(,(url o name) "_darcs/hashed_inventory"))
        (touch-www-file `(,(url o name) "_darcs/inventory"))))
  (:method ((o hg-http-remote) name)
    (touch-www-file `(,(url o name) "?cmd=heads")))
  (:method ((o rsync) name)
    (with-valid-exit-codes ((23 nil)) (rsync "--list-only" (url o name))))
  (:method ((o cvs-native-remote) name)
    (with-valid-exit-codes ((1 nil)) (cvs "-d" (url o name) "history")))
  (:method ((o tarball-http-remote) name)
    (not (null (determine-available-module-tarball-version-starting-after (url o name) (initial-tarball-version o)))))
  (:method ((o svn-http-remote) name)
    (touch-www-file (url o name))))

(defvar *new-repository-p*)
(defvar *source-remote*
  "ISSUE:LOCALITY-SOURCE-REMOTE-TRACKING")

(defgeneric fetch-module-using-remote (remote module-name url final-gate-repo-pathname)
  (:documentation
   "Update the local repository, maybe creating it first.  Note that
the provided directory is the final directory in the gate locality.")
  ;; ========================== branch model aspect =============================
  (:method ((o git-remote) name url repo-dir)
    "ISSUE:IMPLICIT-VS-EXPLICIT-PULLS
Note that this method doesn't affect working tree, instead deferring that
to the above :AROUND method."
    (when *new-repository-p*
      (with-explanation ("initialising git repository of module ~A in ~S" name *repository*)
        (init-git-repo repo-dir))
      (ensure-gitremote (name o) (url o name)))
    (git-fetch-remote o name))
  (:method :around ((o nongit-mixin) name url repo-dir)
    (unless *new-repository-p*
      (git-set-head-index-tree :master)) ; ISSUE:FREE-THE-MASTER-BRANCH-IN-CONVERTED-REPOSITORIES-FOR-THE-USER
    (call-next-method); must operate on the local master
    (let ((master-val (ref-value '("master") nil)))
      (git *repository* "update-ref" `("refs/remotes/" ,(down-case-name o) "/master") (cook-ref-value master-val))))
  ;; ====================== end of branch model aspect ==========================
  ;; direct fetch, non-git
  (:method ((o cvs-native-remote) name url repo-dir)
    (multiple-value-bind (url cvs-module-name) (url o (module name))
      (direct-import-cvs url repo-dir nil (or cvs-module-name (downstring name)))))
  (:method ((o svn-direct) name url repo-dir)
    (multiple-value-bind (url svn-module-name) (url *source-remote* name)
      (direct-import-svn url repo-dir nil (or svn-module-name (downstring name)) *new-repository-p*)))
  (:method ((o tarball-http-remote) name url-template repo-dir)
    (direct-import-tarball url-template repo-dir nil (gate-temp-directory (gate *self*)) (when *new-repository-p*
                                                                                           (initial-tarball-version o))))
  (:method ((o cvs-locality) name url repo-dir)
    (multiple-value-bind (url cvs-module-name) (url *source-remote* name)
      (indirect-import-cvs url repo-dir nil (module-pathname name (locality o))
                           (or cvs-module-name (downstring name)) (cvs-locality-lock-path o))))
  (:method ((o svn-locality) name url repo-dir)
    (multiple-value-bind (url svn-module-name) (url *source-remote* name)
      (indirect-import-svn url repo-dir nil (module-pathname name (locality o))
                           (or svn-module-name (downstring name)) *new-repository-p*))))

(defun update-module-using-remote (module-name remote url repo-dir)
  "Update the repository in REPO-DIR for the module with MODULE-NAME,
using URL within the REMOTE to the latest version available from it."
  (with-error-resignaling
      ((executable-failure ((cond) fetch-failure :remote remote :module module-name :execution-error (format nil "~A" cond)))
       (missing-executable ((cond) fetch-failure :remote remote :module module-name :execution-error (format nil "~A" cond))))
    (with-git-repository-write-access (*new-repository-p*) repo-dir
      (ensure-clean-repository (repository-policy-value :unsaved-changes))
      (let* ((old-head (get-head))
             (desire-op-ref '("desire" "op"))
             (remote-ref (make-remote-ref remote "master"))
             (drive-head-branch-p (repository-policy-value :drive-head-branch))
             (operating-ref (if drive-head-branch-p
                                old-head
                                desire-op-ref)))
        (let ((*source-remote* remote))
          (with-explanation ("on behalf of module ~A, fetching from remote ~A to ~S"
                             module-name (transport remote) (vcs-type remote) url repo-dir)
            (fetch-module-using-remote remote module-name url repo-dir)))
        ;; HEAD option summary:            unsch  HEAD  head  wtree
        ;;  - restore HEAD&head
        ;;    f-m-u-r
        ;;    - and reapply stash          stash  head  stay  stapp
        ;;    - restore only HEAD          ???    head  stay  reset
        ;;  - update HEAD&head
        ;;    f-m-u-r and apply
        ;;    - try merge unsaved changes  stash  head  ffor  stapp  ; reasonable?
        ;;    - ignore unsaved changes     ???    head  ffor  reset
        (git *repository* "update-ref" desire-op-ref remote-ref)
        (ensure-clean-repository (repository-policy-value :unsaved-changes-postwrite))
        ;; stay here, move with branch, move to desir0op
        (when (repository-policy-value :drive-head)
          (if drive-head-branch-p
              (git-set-branch-index-tree remote-ref)
              (git-set-head-index-tree desire-op-ref)))
        (when (repository-policy-value :reapply-stash)
          (git-stash-apply))
        (setf (git-repository-world-readable-p) *default-world-readable*)
        (let ((*executable-standard-output* nil))
          (git-repository-update-for-dumb-servers repo-dir))))))

(defun update (module &key (locality (gate *self*)) pass-output (if-update-fails nil) &aux
               (module (coerce-to-module module)))
  "Decide on a best location to obtain the MODULE from,
fetch it from there, possibly mark the result as redistributed and
analyse its contents for constituent systems.

Unless the module was already present, failure to find a remote,
when IF-UPDATE-FAILS is :ERROR, causes an error to be signalled."
  (if-let ((best-remote (or (module-best-remote module :if-does-not-exist :continue)
                            (module-best-remote module :if-does-not-exist :continue :allow-self t))))
    (if (eq *self* (remote-distributor best-remote))
        (syncformat t ";; Module ~A is local, skipping update~%" (name module))
        (let* ((url (url best-remote module))
               (module-name (name module))
               (repo-dir (module-pathname module-name locality)))
          ;; handle buildbot's stashing shenanigans
          (when (module-stashed-repo-present-p module locality)
            (syncformat t ";; Unstashing module ~A~%" module-name)
            (unstash-module module locality))
          ;; do fetch
          (with-maybe-just-printing-conditions (t fetch-failure) (not *fetch-errors-serious*)
            (restart-bind ((retry (lambda ()
                                    (git repo-dir "gui")
                                    (invoke-restart (find-restart 'retry)))
                             :test-function (of-type 'repository-not-clean-during-fetch)
                             :report-function (formatter "Launch git gui to fix the issue, then retry the operation.")))
              (let ((*executable-standard-output* (if pass-output t *executable-standard-output*)))
                (format t ";; Fetching module ~A from ~A remote ~A, ~A~%"
                        module-name (vcs-type best-remote) (name best-remote) url)
                (update-module-using-remote module-name best-remote url repo-dir)
                (format t ";; Done fetching ~A~%" module-name)
                (when *default-publishable*
                  (declare-module-converted module-name locality)))))
          ;; alright, fetch went good, now tie in changes
          (notice-module-repository module nil locality)))
    ;; no acceptable remote found..
    (unless (and (module-locally-present-p module)
                 (ecase if-update-fails
                   ((nil)  t)
                   (:error nil)))
      (error 'insatiable-desire :desire module)))
  (values))
