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


(defvar *http-proxy*                               nil)

(define-executable git :may-want-display t
                   :fixed-environment ("HOME=/tmp" "PAGER=/bin/cat" (when *http-proxy*
                                                                      (strconcat* "http_proxy=" *http-proxy*))))

(define-condition vcs-condition ()
  ((vcs :reader condition-vcs :initarg :vcs)))

(define-condition git-condition (vcs-condition)
  ()
  (:default-initargs :vcs 'git))

(define-condition git-error (error git-condition)
  ())

(define-simple-error git-error)

(defun missing-remote-error (name &optional directory &aux
                             (path (or directory *default-pathname-defaults*)))
  (repository-error path "~@<At ~S: remote ~A not found, and no URL was provided.~:@>"
                    path name))

(defun extort-remote-url (name &optional directory)
  (restart-case (missing-remote-error name directory)
    (specify (specified-url)
      :report "Specify URL for the missing remote."
      specified-url)))

;;;
;;; Repositories
;;;
(defvar *repository*)

(define-binder with-repository *repository*)

(defun git-predicate (directory explanation-format-control git-arguments)
  (with-executable-options (:explanation `(,explanation-format-control ,directory) :output nil)
    (within-directory (directory)
      (not (with-shell-predicate (apply #'git git-arguments))))))

(defmacro defgitpredicate (name (explanation-format-control) &body git-arguments)
  `(defun ,name (&optional (directory *repository*))
     (git-predicate directory ,explanation-format-control '(,@git-arguments))))

;; directory level
(defun directory-has-git-objects-p (directory)
  (not (null (or (directory (subfile directory '("objects" "pack" :wild) :type :wild))
                 (find-if (lambda (x) (= 2 (length (lastcar (pathname-directory x)))))
                          (directory (merge-pathnames ".git/objects/*/" directory)))))))

(defun dotgit (directory)
  "Given a repository DIRECTORY, return the path to its git storage
directory."
  (merge-pathnames ".git/" directory))

;; repository level
(defun git-nonbare-repository-present-p (&optional (directory *repository*) &aux
                                         (dotgit (dotgit directory)))
  "See if repository in DIRECTORY and source code is available at LOCALITY."
  (and (directory-exists-p directory)
       (directory-exists-p dotgit)
       (directory-has-git-objects-p dotgit)))

(defun git-repository-bare-p (&optional (directory *repository*))
  "See if the DIRECTORY, which is known to contain a git repository is
bare or not."
  (null (directory-exists-p (dotgit directory))))

(defun (setf git-repository-bare-p) (val &optional (directory *repository*) &aux
                                     (dotgit (dotgit directory)))
  (when val
    (git-error "~@<Couldn't make git repository at ~S bare: not implemented.~:@>" directory))
  (make-directory dotgit #+unix #o755)
  (dolist (filename (directory (merge-pathnames "*" directory)))
    (rename-to-directory filename dotgit))
  (setf (gitvar 'core.bare directory) "false")
  (git-set-branch-index-tree nil directory)
  nil)

(defun git-repository-world-readable-p (&optional (directory *repository*))
  "See, whether or not repository within DIRECTORY allows itself to be
exported for the purposes of git-daemon."
  (file-exists-p (merge-pathnames ".git/git-daemon-export-ok" directory)))

(defun (setf git-repository-world-readable-p) (val &optional (directory *repository*))
  "Change git-daemon's idea about world-readability of DIRECTORY."
  (let ((path (merge-pathnames ".git/git-daemon-export-ok" directory)))
    (if val
        (open path :direction :probe :if-does-not-exist :create)
        (when (file-exists-p path)
          (delete-file path)))
    val))

(defgitpredicate git-repository-unstaged-changes-p ("determining whether git repository at ~S has unstaged changes")
  "diff" "--exit-code")

(defgitpredicate git-repository-staged-changes-p ("determining whether git repository at ~S has staged changes")
  "diff" "--exit-code" "--cached")

(defgitpredicate git-repository-changes-p ("determining whether git repository at ~S has staged or unstaged changes")
  "diff" "--exit-code" "--summary" "HEAD" "--")

(defun git-repository-status (&optional (directory *repository*))
  "Examine status of the git repository within DIRECTORY and return lists of pathnames as multiple values.
The lists of pathnames returned have following semantics:
    - staged modified,
    - staged deleted,
    - staged new,
    - unstaged modified,
    - unstaged deleted."
  (multiple-value-bind (status output)
      (with-executable-options (:explanation `("determining status of git repository at ~S" ,directory)
                                :output :capture)
        (within-directory (directory)
          (with-shell-predicate (git "status"))))
    (declare (ignore status))
    (with-input-from-string (s output)
      (flet ((seek-past-marker ()
               (iter (for line = (read-line s nil nil))
                     (unless line
                       (error "~@<Premature EOF while reading from 'git status'.~:@>"))
                     (while (not (string= line "#")))))
             (collect-prefixed ()
               (iter (for line = (read-line s nil nil))
                     (unless (starts-with-subseq "#	" line)
                       (finish))
                     (unless (starts-with-subseq #(#\# #\Tab) line)
                       (error "~@<Bad constituent ~S while reading from 'git status'.~:@>" line))
                     (collect (subseq line 2))))
             (process-entries (list)
               (iter (for entry in list)
                     (multiple-value-bind (modifid-p mod-suffix) (starts-with-subseq "new file:   " entry :return-suffix t)
                       (multiple-value-bind (deled-p del-suffix) (starts-with-subseq "deleted:    " entry :return-suffix t)
                         (multiple-value-bind (new-p new-suffix) (starts-with-subseq "modified:   " entry :return-suffix t)
                           (cond (modifid-p (collect mod-suffix into new))
                                 (deled-p   (collect del-suffix into deleted))
                                 (new-p     (collect new-suffix into modified))
                                 (t (error "~@<Bad constituent ~S while reading from 'git status'.~:@>" entry))))))
                     (finally (return (values modified deleted new))))))
        (iter outer
              (with staged-modified) (with staged-deleted) (with staged-new)
              (with unstaged-modified) (with unstaged-deleted)
              (with untracked)
              (for line = (read-line s nil nil))
              (while line)
              (switch (line :test #'string=)
                ("# Changes to be committed:"
                 (seek-past-marker)
                 (setf (values staged-modified staged-deleted staged-new)
                       (process-entries (collect-prefixed))))
                ("# Changed but not updated:"
                 (seek-past-marker)
                 (setf (values unstaged-modified unstaged-deleted)
                       (process-entries (collect-prefixed))))
                ("# Untracked files:"
                 (seek-past-marker)
                 (setf untracked (collect-prefixed)))
                (t
                 (unless (or (starts-with-subseq "# On branch " line)
                             (string= line "nothing to commit (working directory clean)")
                             (string= line "nothing added to commit but untracked files present (use \"git add\" to track)"))
                   (error "~@<Unrecognised header ~S while reading from 'git status'.~:@>" line))))
              (finally (return-from outer (values staged-modified staged-deleted staged-new unstaged-modified unstaged-deleted untracked))))))))

(defun git-repository-update-for-dumb-servers (&optional (directory *repository*))
  (within-directory (directory)
    (git "update-server-info")))

;;;
;;; Policies
;;;
;; policy atom:
;;   - an attribute set
;;     - on update, what to do with unsaved changes?
;;   - default, or path-specific
;; policy:
;;   - a set of policy atoms
;;
(defstruct policy-atom
  (unsaved-changes nil :type (or null (member :stash :reset :error)))
  (missing-master  nil :type (or null (member :create-on-head :error))))

(defclass repository-policy (registered)
  ((atoms :initargs :atoms))
  (:default-initargs
   :atoms (make-hash-table :test 'equalp)))

(define-subcontainer policy-atom :key-type t :container-slot atoms)

(defparameter *repository-policies*
  (alist-hash-table
   `((:default . ,(make-policy-atom :unsaved-changes :stash
                                    :missing-master  :create-on-head)))
   :test 'eq))

(define-root-container *repository-policies* repository-policy :key-type symbol :coercer t)

(defvar *default-repository-policy* (repository-policy :default))
(defvar *repository-policy*         *default-repository-policy*)

(defun invoke-with-repository-policy (policy fn)
  (let ((*repository-policy* (coerce-to-repository-policy policy)))
    (funcall fn)))

(defmacro with-repository-policy (policy &body body)
  `(invoke-with-repository-policy ,policy (lambda () ,@body)))

(defun repository-policy-value (value-name)
  (or (slot-value *repository-policy* value-name)
      (slot-value *default-repository-policy* value-name)))

;;;;
;;;; Commits
;;;;
(defclass commit ()
  ((id :reader commit-id :initarg :id)
   (date :reader commit-date :initarg :date)
   (author :reader commit-author :initarg :author)
   (message :reader commit-message :initarg :message)))

(defclass git-commit (commit)
  ())

(defun make-commit (id date author message)
  (make-instance 'git-commit :id id :date date :author author :message message))

;;;
;;; Config variables
;;;
(defun gitvar (var &optional directory)
  (declare (type symbol var))
  (maybe-within-directory directory
    (with-executable-options (:explanation `("getting value of git variable ~A" ,(symbol-name var))
                              :output :capture)
      (multiple-value-bind (setp output)
          (with-shell-predicate
              (git "config" (string-downcase (symbol-name var))))
        (when setp
          (string-right-trim '(#\Return #\Newline) output))))))

(defun (setf gitvar) (val var &optional directory globalp)
  (declare (type symbol var) (type string val))
  (maybe-within-directory directory
    (with-explanation ("setting git variable ~A to ~A" (symbol-name var) val)
      (apply #'git "config" (xform globalp (curry #'cons "--global") (list "--replace-all" (string-downcase (symbol-name var)) val))))))

;;;
;;; Remotes
;;;
(defun gitremotes (&optional directory)
  (maybe-within-directory directory
    (multiple-value-bind (status output)
        (with-executable-options (:explanation `("listing git remotes in ~S" ,*default-pathname-defaults*)
                                  :output :capture)
          (git "remote" "-v"))
      (declare (ignore status))
      (iter (for line in (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output)))
            (while (plusp (length line)))
            (destructuring-bind (name url &optional fetch-or-push)
                (iter (for str in (split-sequence #\Space line :remove-empty-subseqs t))
                      (nconcing
                       (split-sequence #\Tab str :remove-empty-subseqs t)))
              (when (or (not fetch-or-push)
                        (string= "(fetch)" fetch-or-push))
                (collect (canonicalise-name name) into names)
                (collect url into urls)))
            (finally (return (values names urls)))))))

(defun find-gitremote (name &optional directory)
  (multiple-value-bind (remote-names urls) (gitremotes directory)
    (iter (for remote-name in remote-names)
          (for remote-url in urls)
          (finding remote-url such-that (eq name remote-name)))))

(defun add-gitremote (name url &optional directory)
  (maybe-within-directory directory
    (with-explanation ("adding a git remote ~A (~A) in ~S" name url *default-pathname-defaults*)
      (git "remote" "add" (downstring name) url))))

(defun remove-gitremote (name &optional directory)
  (maybe-within-directory directory
    (with-explanation ("removing git remote ~A in ~S" name *default-pathname-defaults*)
      (git "remote" "rm" (downstring name)))))

(defun ensure-gitremote (name &optional url directory)
  (let* ((found-url (find-gitremote name directory))
         (found-good-url-p (and found-url (if url
                                              (string/= url found-url)
                                              t))))
    (unless (or found-good-url-p url)
      (setf url (extort-remote-url name (or directory *default-pathname-defaults*))))
    (when (and found-url (not found-good-url-p))
      (remove-gitremote name directory))
    (unless found-good-url-p
      (add-gitremote (downstring name) url directory))))

(defun fetch-gitremote (remote-name &optional directory)
  (maybe-within-directory directory
    (with-explanation ("fetching from git remote ~A in ~S" remote-name *default-pathname-defaults*)
      (git "fetch" (downstring remote-name)))))

;;;
;;; Raw refs
;;;
(defun ref-shortp (ref)
  (endp (cdr ref)))

(defun canonicalise-ref (ref)
  (declare (type (or string cons) ref))
  (let ((ref (ensure-cons ref)))
    (if (ref-shortp ref)
        (cons "heads" ref)
        ref)))

(defun ref-path (ref &optional directory)
  (subfile directory (list* ".git" "refs" (canonicalise-ref ref))))

(defun path-ref (pathname directory)
  (let ((translated (translate-pathname pathname (merge-pathnames #p".git/refs/**/*" directory) #p"/**/*")))
    (append (rest (pathname-directory translated)) (list (pathname-name translated)))))

(defun parse-commit-id (string)
  (parse-integer string :radix #x10))

(defun %read-ref (pathname)
  (lret ((refval (parse-commit-id (file-as-string pathname))))
    (unless (not (minusp refval))
      (git-error "~@<Bad value in ref ~S: ~S.~:@>" pathname refval))))

(defun parse-refval (string)
  (if (equalp (subseq string 0 5) "ref: ")
      (values (split-sequence #\/ (subseq string 5)))
      (values nil (parse-commit-id  string))))

(defun cook-refval (refvalue &optional prepend-refs)
  (etypecase refvalue
    (integer (format nil "~40,'0X" refvalue))
    (list (flatten-path-list (xform prepend-refs (curry #'cons "refs") refvalue)))))

(defun ref-headp (ref)
  (or (ref-shortp ref) (string= "heads" (first ref))))

(defun ref-remotep (ref)
  (and (not (ref-shortp ref)) (string= "remotes" (first ref))))

(defun make-remote-ref (remote-name branch)
  `("remotes" ,(downstring remote-name) ,(downstring branch)))

;;;
;;; Raw refs & iteration
;;;
(defun head-pathnames (directory)
  (remove nil (directory (subwild directory `(".git" "refs" "heads") :name :wild :type :wild)
                         #+sbcl :resolve-symlinks #+sbcl nil)
          :key #'pathname-name))

(defun all-remote-head-pathnames (directory)
  (remove-if (lambda (p &aux (name (pathname-name p))) (or (null name) (string= name "HEAD")))
             (directory (subwild directory `(".git" "refs" "remotes") :name :wild :type :wild)
                        #+sbcl :resolve-symlinks #+sbcl nil)))

(defun remote-head-pathnames (remote directory)
  (remove-if (lambda (p &aux (name (pathname-name p))) (or (null name) (string= name "HEAD")))
             (directory (subwild directory `(".git" "refs" "remotes" ,(downstring remote)) :name :wild :type :wild)
                        #+sbcl :resolve-symlinks #+sbcl nil)))

(defmacro do-packed-refs ((ref refval directory) &body body)
  (with-gensyms (packed-refs-path s line 1read-offt)
    `(let ((,packed-refs-path (subfile ,directory '(".git" "packed-refs"))))
       (when (probe-file ,packed-refs-path)
         (with-open-file (,s ,packed-refs-path)
           (let ((*read-eval* nil)
                 (*read-base* #x10))
             (iter (for ,line = (read-line ,s nil nil))
                   (while ,line)
                   (when (char= #\# (aref ,line 0)) (next-iteration))
                   (for (values ,refval ,1read-offt) = (read-from-string ,line nil nil))
                   (unless ,refval (next-iteration))
                   (for ,ref = (cdr (split-sequence #\/ (downstring (read-from-string ,line nil nil :start ,1read-offt)))))
                   ,@body)))))))

(defun map-packed-refs (predicate fn directory)
  (do-packed-refs (ref refval directory)
    (when (funcall predicate ref refval)
      (collect (funcall fn ref refval)))))

(defun map-pathnames-full-refs (fn pathnames directory)
  (iter (for pathname in pathnames)
        (collect (funcall fn (path-ref pathname directory) (%read-ref pathname)))))

(defun map-heads (fn directory)
  (append (map-pathnames-full-refs fn (head-pathnames directory) directory)
          (map-packed-refs (lambda (r v) (declare (ignore v)) (string= (first r) "heads")) fn directory)))

(defun map-all-remote-heads (fn directory)
  (append (map-pathnames-full-refs fn (all-remote-head-pathnames directory) directory)
          (map-packed-refs (lambda (r v) (declare (ignore v)) (string= (first r) "remotes")) fn directory)))

(defun map-remote-heads (fn remote directory)
  (append (map-pathnames-full-refs fn (remote-head-pathnames remote directory) directory)
          (map-packed-refs (lambda (r v) (declare (ignore v)) (and (string= (first r) "remotes") (string= (second r) remote))) fn directory)))

(defun refs-by-value (refval directory)
  (flet ((ref-if-= (r v) (when (= v refval) r)))
    (remove nil (append (map-heads #'ref-if-= directory)
                        (map-all-remote-heads #'ref-if-= directory)))))

(defun ref-value (ref directory &key (if-does-not-exist :error))
  (let* ((ref (canonicalise-ref ref))
         (path (ref-path ref directory)))
    (if (probe-file path)
        (%read-ref path)
        (or (car (remove nil (map-packed-refs (lambda (r v) (declare (ignore v)) (equal ref r))
                                              (lambda (r v) (declare (ignore r)) v)
                                              directory)))
            (ecase if-does-not-exist
              (:error (git-error "~@<Ref named ~S doesn't exist in git repository at ~S.~:@>" ref (or directory *default-pathname-defaults*)))
              (:continue nil))))))

(defun ref-coerce-to-value (ref-or-value &optional directory)
  (if (integerp ref-or-value)
      ref-or-value
      (ref-value ref-or-value directory)))

(defgeneric ref= (ref-x ref-y &optional directory)
  (:method (x y &optional directory)
    (ref= (ref-coerce-to-value x directory) (ref-coerce-to-value y directory)))
  (:method ((x integer) (y integer) &optional directory)
    (declare (ignore directory))
    (= x y)))

(defun symbolic-reffile-value (pathname &optional dereference directory)
  (multiple-value-bind (ref refval) (parse-refval (file-line pathname))
    (let ((normalised-ref (rest ref))) ; strip the "refs" component
      (if dereference
          (values (or refval
                      (ref-value normalised-ref directory :if-does-not-exist :continue)
                      (repository-error directory "~@<Reffile ~S references a missing ref ~S.~:@>"
                                        pathname normalised-ref))
                  normalised-ref)
          (or normalised-ref refval)))))

(defun set-symbolic-reffile-value (pathname value)
  (with-output-to-file (reffile pathname)
    (when (consp value)
      (write-string "ref: " reffile))
    (write-string (cook-refval value t) reffile)))
;;;
;;; HEAD operation
;;;
(defun head-pathname (&optional directory remote)
  (subfile directory `(".git" ,@(when remote `("refs" "remotes" ,(downstring remote))) "HEAD")))

(defun get-head (&optional directory remote (dereference t))
  (symbolic-reffile-value (head-pathname directory remote) dereference directory))

(defun set-head (new-value &optional directory remote)
  (declare (type (or cons (integer 0)) new-value))
  (set-symbolic-reffile-value (head-pathname directory remote) new-value))

(defun git-detach-head (&optional directory)
  (maybe-within-directory directory
    (set-head (get-head))))

(defun head-in-clouds-p (&optional directory remote)
  (let ((head (get-head directory remote nil)))
    (not (or (integerp head)
             (ref-value head directory :if-does-not-exist :continue)))))

(defun invoke-with-maybe-detached-head (directory detachp fn)
  (maybe-within-directory directory
    (if detachp
        (let ((current-head (get-head)))
          (unwind-protect (progn (git-detach-head)
                                 (funcall fn))
            (when current-head
              (set-head current-head))))
        (funcall fn))))

(defmacro with-maybe-detached-head ((&optional directory detachp) &body body)
  `(invoke-with-maybe-detached-head ,directory ,detachp  (lambda () ,@body)))

;;;
;;; Branches
;;;
(defun git-branches (&optional directory)
  (maybe-within-directory directory
    (multiple-value-bind (status output)
        (with-executable-options (:explanation `("listing git branches in ~S" ,*default-pathname-defaults*)
                                  :output :capture)
          (git "branch"))
      (declare (ignore status))
      (mapcar (compose #'make-keyword #'string-upcase)
              (remove-if
               (lambda (x) (or (zerop (length x)) (and (= 1 (length x)) (char= #\* (schar x 0)))))
               (mapcan (curry #'split-sequence #\Space)
                       (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output))))))))


(defun git-branch-present-p (name &optional directory)
  (or (not (null (probe-file (ref-path (downstring name) directory))))
      (member name (git-branches directory) :test #'string=)))

(defun git-remove-branch (name &optional directory)
  (maybe-within-directory directory
    (with-explanation ("removing git branch ~A in ~S" name *default-pathname-defaults*)
      (nth-value 0 (git "branch" "-d" (downstring name))))))

(defun git-set-noncurrent-branch (branchname &optional directory (refvalue (get-head directory)))
  (declare (type (or list (integer 0)) refvalue))
  (maybe-within-directory directory
    (with-explanation ("moving non-current ref ~A to ~:[~40,'0X~;~A~] in ~S" branchname (consp refvalue) refvalue *default-pathname-defaults*)
      (nth-value 0 (git "branch" "-f" (downstring branchname) (cook-refval refvalue))))))

(defun git-set-branch (name &optional directory (refvalue (get-head directory)) possibly-current-p)
  (with-maybe-detached-head (directory possibly-current-p)
    (git-set-noncurrent-branch name directory refvalue)))

(defun git-set-branch-index-tree (&optional ref directory)
  (maybe-within-directory directory
    (with-explanation ("hard-resetting repository in ~S~:[~; to ~:*~S~]" *default-pathname-defaults* ref)
      (apply #'git "reset" "--hard" (when ref (list (flatten-path-list ref) "--"))))))

(defun git-stash (&optional directory)
  "Same as GIT-SET-BRANCH-INDEX-TREE with no arguments, but saves changes
in a temporary pseudo-commit."
  (maybe-within-directory directory
    (with-explanation ("stashing changes in git repository at ~S" *default-pathname-defaults*)
      (git "stash"))))

(defun ensure-clean-repository (&optional (if-changes :stash) directory)
  (maybe-within-directory directory
    (with-retry-restarts ((hardreset-repository ()
                            :report "Clear all uncommitted changes, both staged and unstaged."
                            (git-set-branch-index-tree)))
      (when (git-repository-changes-p)
        (ecase if-changes
          (:continue)
          (:stash (git-stash))
          (:reset (git-set-branch-index-tree))
          (:error (git-error "~@<~:[Uns~;S~]taged changes in git repository ~S.~:@>"
                             (git-repository-staged-changes-p directory) (or directory *default-pathname-defaults*))))))))

(defun git-set-head-index-tree (ref &optional (if-changes :stash) directory)
  (let ((ref (mapcar #'downstring (ensure-cons ref))))
    (maybe-within-directory directory
      (ref-value ref nil)
      (ensure-clean-repository if-changes)
      (with-explanation ("checking out ~S in ~S" ref *default-pathname-defaults*)
        (git "checkout" (flatten-path-list ref))))))

(defun invoke-with-branch-change (before-branch after-branch if-changes fn)
  (unwind-protect (progn
                    (git-set-head-index-tree before-branch if-changes)
                    (funcall fn))
    (git-set-head-index-tree after-branch if-changes)))

(defmacro with-branch-change ((before-branch-form after-branch-form &key (if-changes :stash))
                              &body body)
  `(invoke-with-branch-change ,before-branch-form ,after-branch-form ,if-changes
                              (lambda () ,@body)))

(defun invoke-with-gitremote (remote-name fn
                              &key url (if-remote-does-not-exist :create))
  (unless (find-gitremote remote-name)
    (ecase if-remote-does-not-exist
      (:create (ensure-gitremote remote-name url))
      (:error  (missing-remote-error remote-name))))
  (funcall fn))

(defmacro with-gitremote ((remote &key url (if-remote-does-not-exist :create))
                          &body body)
  `(invoke-with-gitremote ,remote (lambda () ,@body)
                          ,@(maybe-prop* :url url)
                          :if-remote-does-not-exist ,if-remote-does-not-exist))

;;;;
;;;; Repository-level operations
;;;;
(defun invoke-with-git-repository-write-access (path fn &aux
                                                (dotgit (dotgit path)))
  (within-directory (path :if-does-not-exist :create)
    (handler-bind ((error (lambda (c)
                            (declare (ignore c))
                            ;; Maintain the gate-has-useful-directories-only invariant.
                            (when (and (directory-created-p)
                                       (not (directory-has-git-objects-p dotgit)))
                              (fad:delete-directory-and-files path))
                            #| Continue signalling. |#)))
      (unless (or (directory-created-p)
                  (directory-has-git-objects-p dotgit))
        (error 'empty-repository :pathname path))
      (funcall fn (directory-created-p)))))

(defmacro with-git-repository-write-access ((new-repo-p) path &body body)
  `(invoke-with-git-repository-write-access ,path (lambda (,new-repo-p) ,@body)))

;;;;
;;;; Queries
;;;;
(defun prefixp (prefix sequence)
  (nth-value 1 (starts-with-subseq prefix sequence :return-suffix t)))

(defun git-commit-log (ref &optional repository-dir)
  "Given a REF and an optional REPOSITORY-DIR, return the commit id, author, date
and commit message of the corresponding commit as multiple values."
  (maybe-within-directory repository-dir
    (multiple-value-bind (status output)
        (with-executable-options (:explanation `("querying commit log of ~S at ~S" ,ref ,*default-pathname-defaults*)
                                  :output :capture)
          (git "log" "-1" (cook-refval ref)))
      (declare (ignore status))
      (with-input-from-string (s output)
        (let (commit-id author date (posn 0))
          (iter (for line = (read-line s nil nil))
                (while (and line (plusp (length line))))
                (incf posn (1+ (length line)))
                (cond-let
                  ((suffix (prefixp "commit " line))  (setf commit-id suffix))
                  ((suffix (prefixp "Author: " line)) (setf author suffix))
                  ((suffix (prefixp "Date:   " line)) (setf date suffix))))
          (unless (and commit-id author date)
            (git-error "~@<Error parsing commit log of ~X at ~S.~:@>" ref *default-pathname-defaults*))
          (let ((message (string-right-trim '(#\Newline)
                                            (subseq output (+ 5 posn)))))
            (make-commit (parse-commit-id commit-id) date author message)))))))

(defun git-repository-last-version-from-tag (&optional repository-dir)
  (maybe-within-directory repository-dir
    (multiple-value-bind (successp output)
        (with-explanation ("determining version last recorded in ~S" *default-pathname-defaults*)
          (git "log" "-1" "--tags" "--decorate=short"))
      (declare (ignore successp))
      (when-let ((version (first (extract-delimited-substrings output "tag: upstream/" #\,))))
        (mapcar #'parse-integer (split-sequence #\. version))))))
