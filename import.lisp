;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2009 by
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
           "~@<An attempt to fetch module ~S from ~S has failed.~@:_~S~:@>" (coerce-to-name module) remote execution-error))
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
  (define-executable darcs-to-git)
  (define-executable hg)
  (define-executable python)        ; this is for hg-to-git.py
  (define-executable rsync)
  (define-executable cvs)
  ;; these are needed for XCVB stack's postinstall
  (define-executable cp)
  (define-executable make))

(defgeneric touch-remote-module (remote module)
  (:method :around ((o remote) name)
    (with-unaffected-executable-output ()
      (with-explanation ("attempting to touch module ~A in ~S" (coerce-to-name name) (url o name))
        (call-next-method))))
  (:method ((o git-remote) name)
    (with-valid-exit-codes ((128 nil)) (git "peek-remote" (url o name))))
  (:method ((o darcs-http-remote) name)
    (or (touch-www-file `(,(url o name) "_darcs/inventory"))
        (touch-www-file `(,(url o name) "_darcs/hashed_inventory"))))
  (:method ((o hg-http-remote) name)
    (touch-www-file `(,(url o name) ".hg/inventory")))
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
(defvar *we-drive-master-p*)
(defvar *source-remote*
  "ISSUE:LOCALITY-SOURCE-REMOTE-TRACKING")

(defun init-db-when-new-repository (name)
  (when *new-repository-p*
    (with-explanation ("initialising git repository of module ~A in ~S" name *default-pathname-defaults*)
      (git "init-db"))))

(defgeneric fetch-module-using-remote (remote module-name url final-gate-repo-pathname)
  (:documentation
   "Update the local repository, maybe creating it first.
Note that the provided directory is the final directory in the gate locality.")
  (:method :around ((o remote) name url repo-dir)
    (with-error-resignaling (executable-failure
                             ((cond) 'fetch-failure :remote o :module name :execution-error (format nil "~A" cond)))
      (with-error-resignaling (missing-executable
                               ((cond) 'fetch-failure :remote o :module name :execution-error (format nil "~A" cond)))
        (within-directory (repo-dir :if-does-not-exist :create)
          (handler-case (progn
                          (unless (directory-created-p)
                            (unless (git-repository-has-objects-p nil)
                              (error 'empty-repository :pathname repo-dir))
                            (when *follow-upstream*
                              (ensure-clean-repository *dirty-repository-behaviour*)))
                          (let ((*new-repository-p* (directory-created-p))
                                (*source-remote* o))
                            ;; at this point several facts apply:
                            ;; - the directory of the module's repository within the gate exists, maybe without .git, and we're in that directory
                            ;; - above variables are bound
                            (with-explanation ("on behalf of module ~A, fetching from remote ~A to ~S" name (transport o) (vcs-type o) url repo-dir)
                              (call-next-method)))
                          (git-set-head-index-tree :master (cond ((or *follow-upstream* (directory-created-p)) :reset)
                                                                 (t :continue)))
                          (setf (git-repository-world-readable-p) *default-world-readable*))
            (error (c)
              (when (and (directory-created-p)
                         (not (git-repository-has-objects-p repo-dir)))
                (fad:delete-directory-and-files repo-dir))
              (error c)))))))
  ;; ========================== branch model aspect =============================
  (:method ((o git-remote) name url repo-dir)
    "ISSUE:IMPLICIT-VS-EXPLICIT-PULLS
Note that this method doesn't affect working tree, instead deferring that
to the above :AROUND method."
    (cond (*new-repository-p*
           (init-db-when-new-repository name)
           (ensure-gitremote (name o) (url o name)))
          (t
           (ensure-tracker-branch)))
    (let ((we-drive-master-p (or *new-repository-p* *drive-git-masters* (and *drive-git-masters-matching-trackers*
                                                                             (not (master-detached-p))))))
      (git-fetch-remote o name)
      (let ((remote-master-val (ref-value `("remotes" ,(down-case-name o) "master") nil))
            (head-in-clouds-p (head-in-clouds-p)))
        (git-set-branch :tracker nil remote-master-val (not head-in-clouds-p))
        (when we-drive-master-p
          (git-set-branch :master nil remote-master-val (not head-in-clouds-p))))))
  (:method :around ((o nongit-mixin) name url repo-dir)
    (unless *new-repository-p*
      (git-set-head-index-tree :master)) ; ISSUE:FREE-THE-MASTER-BRANCH-IN-CONVERTED-REPOSITORIES-FOR-THE-USER
    (call-next-method); must operate on the local master
    (let ((master-val (ref-value '("master") nil)))
      (git-set-branch :tracker nil master-val t)
      (git "update-ref" `("refs/remotes/" ,(down-case-name o) "/master") (cook-refval master-val))))
  ;; ====================== end of branch model aspect ==========================
  ;; direct fetch, non-git
  (:method ((o cvs-native-remote) name url repo-dir)
    (multiple-value-bind (url cvs-module-name) (url o (module name))
      (git "cvsimport" "-d" url (or cvs-module-name (downstring name)))))
  (:method ((o svn-direct) name url repo-dir)
    (multiple-value-bind (url wrinkle) (url o (module name))
      (when *new-repository-p*
        (with-explanation ("on behalf of module ~A, initialising import to git repository from SVN ~S in ~S" name url *default-pathname-defaults*)
          (git "svn" "init" url wrinkle)))
      (git "svn" "fetch")))
  (:method ((o tarball-http-remote) name url-template repo-dir)
    (init-db-when-new-repository name)
    (iter (with last-version = (if *new-repository-p*
                                   (initial-tarball-version o)
                                   (git-repository-last-version-from-tag)))
          (for (values url next-version) = (determine-available-module-tarball-version-starting-after url-template last-version))
          (setf last-version next-version)
          (while url)
          (let* ((slash-pos (or (position #\/ url :from-end t)
                                (remote-error o "~@<Error while calculating URL for module ~A in ~S: resulting URL ~S has no slashes.~:@>" name (name o) url)))
                 (localised-tarball (concatenate 'string "../../tmp/" (subseq url (1+ slash-pos)))))
            (with-file-from-www (localised-tarball url)
              (with-explanation ("on behalf of module ~A, importing tarball version ~A" name (princ-version-to-string next-version))
                (git "import-orig" localised-tarball))))))
  ;; indirect-fetch
  (:method :around ((o separate-clone) name url repo-dir)
    "Note that the fetches will be done later anyway."
    (let ((transit-repo-dir (module-pathname name (locality o))))
      (unless (directory-exists-p transit-repo-dir)
        (clone-transit-module-using-remote o name url transit-repo-dir)))
    (call-next-method))
  (:method :before ((o darcs-http-remote) name url repo-dir)
    (darcs "pull" "--all" "--repodir" (module-pathname name (locality o)) url))
  (:method :before ((o hg-http-remote) name url repo-dir)
    (declare (ignore url))
    (hg "pull" "-R" (module-pathname name (locality o))))
  (:method :before ((o rsync) name url repo-dir)
    (rsync "-ravPz" url (module-pathname name (locality o))))
  (:method ((o indirect-fetch) name url repo-dir)
    (convert-transit-module-using-locality (locality o) name (module-pathname name (locality o)))))

(defgeneric convert-transit-module-using-locality (source-locality name source-repository)
  (:documentation
   "Update conversion of module with NAME within the git repository at *DEFAULT-PATHNAME-DEFAULTS*,
using SOURCE-REPOSITORY within SOURCE-LOCALITY.
Can only be called from FETCH-MODULE-USING-REMOTE, due to the *SOURCE-REMOTE* variable.")
  (:method :around ((o locality) name from-repo-dir)
    (with-explanation ("on behalf of module ~A, converting from ~A to ~A: ~S => ~S" name (vcs-type o) *gate-vcs-type* from-repo-dir *default-pathname-defaults*)
      (call-next-method)))
  (:method ((o darcs-locality) name from-repo-dir)
    (when (git-repository-present-p)
      (multiple-value-bind (staged-mod staged-del staged-new unstaged-mod unstaged-del untracked) (git-repository-status)
        (when untracked
          (format t "~@<;;; ~@;before conversion ~S -> ~S: untracked files ~A in the target repository.  Purging.~:@>~%"
                  from-repo-dir *default-pathname-defaults* untracked)
          (mapc #'delete-file untracked))
        (when (or staged-mod staged-del staged-new unstaged-mod unstaged-del)
          (ensure-clean-repository :error))))
    (with-condition-recourses dirt-files-in-repository
        (multiple-value-bind (successp output) (with-shell-predicate (darcs-to-git from-repo-dir))
          (unless successp
            (error 'dirt-files-in-repository :locality (gate *self*) :module name
                   :dirt-files (extract-delimited-substrings output "Only in .: " #\Newline))))
      (remove-dirt-files (c)
                         (format t "~@<;;; ~@;dirt files ~S prevent darcs-to-git from proceeding. Removing them and retrying...~:@>~%" (condition-dirt-files c))
                         (mapc #'delete-file (condition-dirt-files c))))
    (when (git-repository-bare-p)
      (setf (git-repository-bare-p) nil)))
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
          (git "cvsimport" "-v" "-C" *default-pathname-defaults* "-d" (format nil ":local:~A" (string-right-trim "/" (namestring from-repo-dir))) final-cvs-module-name)))))
  (:method ((o svn-locality) name from-repo-dir)
    (when *new-repository-p*
      (multiple-value-bind (url wrinkle) (url *source-remote* name)
        (declare (ignore url))
        (with-explanation ("on behalf of module ~A, setting up svn to git conversion: ~S => ~S" name from-repo-dir *default-pathname-defaults*)
          (git "svn" "init" `("file://" ,from-repo-dir ,wrinkle))))) ;; 'file://' -- gratuitious SVN complication
    (git "svn" "fetch")))

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

(defun update (module &optional locality &key pass-output)
  (let* ((module (coerce-to-module module))
         (locality (or locality (gate *self*)))
         (best-remote (module-best-remote module :if-does-not-exist :continue)))
    (cond ((null best-remote)
           (if (module-best-remote module :allow-self t)
               (syncformat t ";; Module ~A is local, skipping update~%" (name module))
               (error 'insatiable-desire :desire module)))
          (t
           (let* ((url (url best-remote module))
                  (name (name module))
                  (repo-dir (module-pathname name locality)))
             (syncformat t ";; Fetching module ~A from ~A remote ~A, ~A~%" name (vcs-type best-remote) (name best-remote) url)
             (with-maybe-just-printing-conditions (t fetch-failure) (not *fetch-errors-serious*)
               (restart-bind ((retry (lambda () 
                                       (maybe-within-directory repo-dir
                                         (git "gui"))
                                       (invoke-restart (find-restart 'retry)))
                                :test-function (of-type 'repository-not-clean-during-fetch)
                                :report-function (formatter "Launch git gui to fix the issue, then retry the operation.")))
                 (with-maybe-unaffected-executable-output (pass-output)
                   (fetch-module-using-remote best-remote name url repo-dir)
                   (when *default-publishable*
                     (declare-module-converted name locality)))))
             (syncformat t ";; Done fetching module ~A~%" name))))
    (notice-module-repository module nil locality)
    (values)))
