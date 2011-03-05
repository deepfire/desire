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


(defvar *fetch-errors-serious* nil
  "Whether to raise an error when external executables fail to fetch modules during LUST, DESIRE or UPDATE.
Defaults to NIL.")

(defvar *hg-to-git-location* #p"/usr/share/doc/git-core/contrib/hg-to-git/hg-to-git.py")

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
    (or (touch-www-file `(,(url o name) "_darcs/inventory"))
        (touch-www-file `(,(url o name) "_darcs/hashed_inventory"))))
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

(defgeneric clone-transit-module-using-remote (remote module-name url new-local-repo-pathname)
  (:documentation
   "Create the local repository by performing an initial clone of the remote.
Only for remotes of type SEPARATE-CLONE.")
  (:method :around ((o separate-clone) name url repo-dir)
    (declare (ignore name url))
    (check-pathname-not-occupied repo-dir)
    (call-next-method))
  (:method ((o darcs-http-remote) name url repo-dir)
    (darcs "get" url repo-dir))
  (:method ((o hg-http-remote) name url repo-dir)
    (hg "clone" url repo-dir)))

(defvar *new-repository-p*)
(defvar *source-remote*
  "ISSUE:LOCALITY-SOURCE-REMOTE-TRACKING")

(defun determine-available-module-tarball-version-starting-after (url-template version &optional (search-depth 3))
  ;; XXX: security implications: URL-TEMPLATE comes from DEFINITIONS
  (iter (for depth below search-depth)
        (with current-depth-version = version)
        (for current-depth-variants = (next-version-variants current-depth-version))
        (iter (for next-version-variant in current-depth-variants)
              (for url = (string-right-trim '(#\/) (format nil url-template (princ-version-to-string next-version-variant))))
              (when (with-explanation ("touching URL ~S" url)
                      (touch-www-file url))
                (return-from determine-available-module-tarball-version-starting-after (values url next-version-variant))))
        (setf current-depth-version (first current-depth-variants))))

(defgeneric convert-transit-module-using-locality (source-locality name source-repository)
  (:documentation
   "Update conversion of module with NAME within the git repository at
*REPOSITORY* using SOURCE-REPOSITORY within SOURCE-LOCALITY.  Can only
be called from FETCH-MODULE-USING-REMOTE, due to the *SOURCE-REMOTE*
variable.")
  (:method :around ((o locality) name from-repo-dir)
           (with-explanation ("on behalf of module ~A, converting from ~A to ~A: ~S => ~S"
                              name (vcs-type o) *gate-vcs-type* from-repo-dir *repository*)
      (call-next-method)))
  (:method ((o darcs-locality) name from-repo-dir)
    (if (git-nonbare-repository-present-p *repository*)
        (multiple-value-bind (staged-mod staged-del staged-new unstaged-mod unstaged-del untracked) (git-repository-status)
          (when untracked
            (format t "~@<;;; ~@;before conversion ~S -> ~S: untracked files ~A in the target repository.  Purging.~:@>~%"
                    from-repo-dir *repository* untracked)
            (mapc #'delete-file untracked))
          (when (or staged-mod staged-del staged-new unstaged-mod unstaged-del)
            (ensure-clean-repository :error)))
        (init-git-repo *repository*))
    ;; We ignore exit status, as, sadly, it's not informative.
    ;; Thankfully, git-fast-import is pretty reliable.
    (let ((*executable-standard-output* nil))
      (pipe (darcs-fast-export from-repo-dir)
            (git *repository* "fast-import"))))
  (:method ((o hg-locality) name from-repo-dir)
    (unless (git-nonbare-repository-present-p *repository*)
      (init-git-repo *repository*))
    (let ((*executable-standard-output* nil)
          (*executable-error-output* nil))
      (pipe (hg-fast-export "-r" from-repo-dir)
            (git *repository* "fast-import"))))
  (:method ((o cvs-locality) name from-repo-dir)
    (multiple-value-bind (url cvs-module-name) (url *source-remote* name)
      (declare (ignore url))
      (with-output-to-file (stream (subfile* from-repo-dir "CVSROOT" "config") :if-exists :supersede)
        (format stream "LockDir=~A~%" (cvs-locality-lock-path o)))
      (let ((final-cvs-module-name (or cvs-module-name (downstring name))))
        (unless (directory-exists-p (subdirectory* from-repo-dir final-cvs-module-name))
          (iter (for guess in '("src"))
                (when (directory-exists-p (subdirectory* from-repo-dir guess))
                  (format t "~@<;;; ~@;During import of ~A from ~S: CVS module ~S does not exist, guessed an alternative: ~S.  Recording that as the new wrinkle.~:@>~%"
                          name from-repo-dir final-cvs-module-name guess)
                  (setf final-cvs-module-name guess)
                  (set-remote-module-wrinkle *source-remote* name guess)
                  (leave))
                (finally
                 (definition-error "~@<During import of ~A from ~S: CVS module ~S does not exist, and it's name couldn't be guessed.~:@>" name from-repo-dir final-cvs-module-name))))
        (with-exit-code-to-error-translation ((9 'repository-not-clean-during-fetch :module name :locality (gate *self*)))
          (git *repository* "cvsimport" "-v" "-C" *repository* "-d" (format nil ":local:~A" (string-right-trim "/" (namestring from-repo-dir))) final-cvs-module-name)))))
  (:method ((o svn-locality) name from-repo-dir)
    (when *new-repository-p*
      (multiple-value-bind (url wrinkle) (url *source-remote* name)
        (declare (ignore url))
        (with-executable-options (:explanation `("on behalf of module ~A, setting up svn to git conversion: ~S => ~S"
                                                 name from-repo-dir *repository*))
          (git *repository* "svn" "init" `("file://" ,from-repo-dir ,wrinkle))))) ;; 'file://' -- gratuitious SVN complication
    (git *repository* "svn" "fetch")))

(defgeneric fetch-module-using-remote (remote module-name url final-gate-repo-pathname &optional branch)
  (:documentation
   "Update the local repository, maybe creating it first.  Note that
the provided directory is the final directory in the gate locality.")
  ;; ========================== branch model aspect =============================
  (:method ((o git-remote) name url repo-dir &optional branch)
    "ISSUE:IMPLICIT-VS-EXPLICIT-PULLS
Note that this method doesn't affect working tree, instead deferring that
to the above :AROUND method."
    (when *new-repository-p*
      (with-explanation ("initialising git repository of module ~A in ~S" name *repository*)
        (init-git-repo repo-dir))
      (ensure-gitremote (name o) (url o name)))
    (git-fetch-remote o name)
    (let ((remote-master-val (ref-value (make-remote-ref name "master") nil)))
      (git-set-branch branch *repository* remote-master-val (not head-in-clouds-p))))
  (:method :around ((o nongit-mixin) name url repo-dir &optional branch)
    (unless *new-repository-p*
      (git-set-head-index-tree :master)) ; ISSUE:FREE-THE-MASTER-BRANCH-IN-CONVERTED-REPOSITORIES-FOR-THE-USER
    (call-next-method); must operate on the local master
    (let ((master-val (ref-value '("master") nil)))
      (git-set-branch :tracker nil master-val t)
      (git *repository* "update-ref" `("refs/remotes/" ,(down-case-name o) "/master") (cook-refval master-val))))
  ;; ====================== end of branch model aspect ==========================
  ;; direct fetch, non-git
  (:method ((o cvs-native-remote) name url repo-dir &optional branch)
    (multiple-value-bind (url cvs-module-name) (url o (module name))
      (git *repository* "cvsimport" "-d" url (or cvs-module-name (downstring name)))))
  (:method ((o svn-direct) name url repo-dir &optional branch)
    (multiple-value-bind (url wrinkle) (url o (module name))
      (when *new-repository-p*
        (with-explanation ("on behalf of module ~A, initialising import to git repository from SVN ~S in ~S"
                           name url repo-dir)
          (git repo-dir "svn" "init" url wrinkle)))
      (git repo-dir "svn" "fetch")))
  (:method ((o tarball-http-remote) name url-template repo-dir &optional branch)
    (with-explanation ("initialising git repository of module ~A in ~S" name *repository*)
      (init-git-repo *repository*))
    (iter (with last-version = (if *new-repository-p*
                                   (initial-tarball-version o)
                                   (git-repository-last-version-from-tag)))
          (for (values url next-version) = (determine-available-module-tarball-version-starting-after url-template last-version))
          (setf last-version next-version)
          (while url)
          (let* ((slash-pos (or (position #\/ url :from-end t)
                                (remote-error o "~@<Error while calculating URL for module ~A in ~S: ~S has no slashes.~:@>"
                                              name (name o) url)))
                 (localised-tarball (merge-pathnames (subseq url (1+ slash-pos)) (gate-temp-directory (gate *self*)))))
            (with-file-from-www (localised-tarball url)
              (with-executable-options (:explanation `("on behalf of module ~A, importing tarball version ~A"
                                                       ,name ,(princ-version-to-string next-version)))
                (git *repository* "import-orig" localised-tarball))))))
  ;; indirect-fetch
  (:method :around ((o separate-clone) name url repo-di &optional branch)
    "Note that the fetches will be done later anyway."
    (let ((transit-repo-dir (module-pathname name (locality o))))
      (unless (directory-exists-p transit-repo-dir)
        (clone-transit-module-using-remote o name url transit-repo-dir)))
    (call-next-method))
  (:method :before ((o darcs-http-remote) name url repo-dir &optional branch)
    (darcs "pull" "--all" "--repodir" (module-pathname name (locality o)) url))
  (:method :before ((o hg-http-remote) name url repo-dir &optional branch)
    (declare (ignore url))
    (hg "pull" "-R" (module-pathname name (locality o))))
  (:method :before ((o rsync) name url repo-di &optional branch)
    (rsync "-ravPz" url (module-pathname name (locality o))))
  (:method ((o indirect-fetch) name url repo-dir &optional branch)
    (convert-transit-module-using-locality (locality o) name (module-pathname name (locality o)))))

(defun update-module-using-remote (module-name remote url repo-dir &aux
                                   (ref '("desire" "op")))
  "Update the repository in REPO-DIR for the module with MODULE-NAME,
using URL within the REMOTE to the latest version available from it."
  (with-error-resignaling
      ((executable-failure ((cond) fetch-failure :remote remote :module module-name :execution-error (format nil "~A" cond)))
       (missing-executable ((cond) fetch-failure :remote remote :module module-name :execution-error (format nil "~A" cond))))
    (with-git-repository-write-access (*new-repository-p*) repo-dir
      (ensure-clean-repository (repository-policy-value :unsaved-changes))
      (let ((old-head (switch-to-op)))
        (let ((*source-remote* remote))
          (with-explanation ("on behalf of module ~A, fetching from remote ~A to ~S"
                             module-name (transport remote) (vcs-type remote) url repo-dir)
            (fetch-module-using-remote remote module-name url repo-dir ref)))
        ;; HEAD option summary:            unsch  HEAD  head  wtree
        ;;  - restore HEAD&head
        ;;    f-m-u-r
        ;;    - and reapply stash          stash  head  stay  stapp
        ;;    - restore only HEAD          ???    head  stay  reset
        ;;  - update HEAD&head
        ;;    f-m-u-r and apply
        ;;    - try merge unsaved changes  stash  head  ffor  stapp
        ;;    - ignore unsaved changes     ???    head  ffor  reset
        (git-set-head-index-tree ref (repository-policy-value :unsaved-changes-postwrite))
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
