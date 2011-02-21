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

(define-executable (%git git)
    :may-want-display t
    :fixed-environment ("HOME=/tmp" "PAGER=/bin/cat" (when *http-proxy*
                                                       (strconcat* "http_proxy=" *http-proxy*))))

(defun git (work-tree &rest args)
  (declare (string work-tree))
  (apply #'%git (strconcat* "--work-tree=" work-tree) args))

(define-condition vcs-condition ()
  ((vcs :reader condition-vcs :initarg :vcs)))

(define-condition git-condition (vcs-condition)
  ()
  (:default-initargs :vcs 'git))

(define-condition git-error (error git-condition)
  ())

(define-simple-error git-error)

(defun missing-remote-error (name &optional (directory *repository*))
  (repository-error directory "~@<At ~S: remote ~A not found, and no URL was provided.~:@>"
                    directory name))

(defun extort-remote-url (name &optional (directory *repository*))
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
    (not (with-shell-predicate (apply #'git directory git-arguments)))))

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
  (let ((flag-path (merge-pathnames ".git/git-daemon-export-ok" directory)))
    (if val
        (open flag-path :direction :probe :if-does-not-exist :create)
        (when (file-exists-p flag-path)
          (delete-file flag-path)))
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
        (with-shell-predicate (git directory "status")))
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
  (git directory "update-server-info"))

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
  (unsaved-changes     nil :type (or null (member :error :reset :stash)))
  (missing-master      nil :type (or null (member :error :create-on-head)))
  ;; hmm, it's not entirely clear this is the correct answer for a multitude of relevant questions
  (operating-branch    nil :type (or (eql t) keyword))
  (preexisting-gitless nil :type (or null (member :error :take-over))))

(defclass repository-policy (named)
  ((atoms :initarg :atoms))
  (:default-initargs
   :atoms       (make-hash-table :test 'equalp)))

(define-subcontainer policy-atom :key-type t :container-slot atoms)

(defun make-repository-policy (name &rest atom-specs)
  (make-instance 'repository-policy :name name
                 :atoms (alist-hash-table (mapcar (lambda (spec) (cons (first spec) (second spec)))
                                                  atom-specs)
                                          :test 'equalp)))

(defun make-default-repository-policy (name &rest properties &key &allow-other-keys)
  (make-repository-policy name (list nil (apply #'make-policy-atom properties))))

(defparameter *repository-policies*
  (alist-hash-table
   `((:default .  ,(make-default-repository-policy
                    :default
                    :unsaved-changes     :stash
                    :missing-master      :create-on-head
                    :operating-branch    :master
                    :preexisting-gitless :take-over))
     (:new-repo . ,(make-default-repository-policy
                    :new-repo
                    :unsaved-changes     :error
                    :missing-master      :error
                    :operating-branch    :master
                    :preexisting-gitless :error))
     (:cautious . ,(make-default-repository-policy
                    :cautious
                    :unsaved-changes     :error
                    :missing-master      :error
                    :operating-branch    t ; use detached operation?
                    :preexisting-gitless :error)))
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
(defun gitvar (var &optional (directory *repository*))
  (declare (type symbol var))
  (with-executable-options (:explanation `("getting value of git variable ~A" ,(symbol-name var))
                            :output :capture)
    (multiple-value-bind (setp output) (with-shell-predicate (git directory "config" (string-downcase (symbol-name var))))
      (when setp
        (string-right-trim '(#\Return #\Newline) output)))))

(defun (setf gitvar) (val var &optional (directory *repository*) globalp)
  (declare (type symbol var) (type string val))
  (with-explanation ("setting git variable ~A to ~A" (symbol-name var) val)
    (apply #'git directory "config"
           (xform globalp (curry #'cons "--global") (list "--replace-all" (string-downcase (symbol-name var)) val)))))

;;;
;;; Remotes
;;;
(defun gitremotes (&optional (directory *repository*))
  (multiple-value-bind (status output)
      (with-executable-options (:explanation `("listing git remotes in ~S" ,directory)
                                :output :capture)
        (git directory "remote" "-v"))
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
          (finally (return (values names urls))))))

(defun find-gitremote (name &optional (directory *repository*))
  (multiple-value-bind (remote-names urls) (gitremotes directory)
    (iter (for remote-name in remote-names)
          (for remote-url in urls)
          (finding remote-url such-that (eq name remote-name)))))

(defun add-gitremote (name url &optional (directory *repository*))
  (with-explanation ("adding a git remote ~A (~A) in ~S" name url directory)
    (git directory "remote" "add" (downstring name) url)))

(defun remove-gitremote (name &optional (directory *repository*))
  (with-explanation ("removing git remote ~A in ~S" name directory)
    (git directory "remote" "rm" (downstring name))))

(defun ensure-gitremote (name &optional url (directory *repository*))
  (let* ((found-url (find-gitremote name directory))
         (found-good-url-p (and found-url (if url
                                              (string/= url found-url)
                                              t))))
    (unless (or found-good-url-p url)
      (setf url (extort-remote-url name directory)))
    (when (and found-url (not found-good-url-p))
      (remove-gitremote name directory))
    (unless found-good-url-p
      (add-gitremote (downstring name) url directory))))

(defun fetch-gitremote (remote-name &optional (directory *repository*))
  (with-explanation ("fetching from git remote ~A in ~S" remote-name directory)
    (git directory "fetch" (downstring remote-name))))

;;;
;;; Ref names
;;;
(defun ref-shortp (ref)
  (endp (cdr ref)))

(defun canonicalise-ref (ref)
  (declare (type (or string cons) ref))
  (let ((ref (ensure-cons ref)))
    (if (ref-shortp ref)
        (cons "heads" ref)
        ref)))

(defun ref-headp (ref)
  (or (ref-shortp ref) (string= "heads" (first ref))))

(defun ref-remotep (ref)
  (and (not (ref-shortp ref)) (string= "remotes" (first ref))))

(defun make-remote-ref (remote-name branch)
  `("remotes" ,(downstring remote-name) ,(downstring branch)))

;;;
;;; Ref content
;;;
(defun parse-commit-id (string)
  (parse-integer string :radix #x10))

(defun parse-ref-value (string)
  (if (equalp (subseq string 0 5) "ref: ")
      (values (split-sequence #\/ (subseq string 5)))
      (values nil (parse-commit-id  string))))

(defun cook-ref-value (refvalue &optional prepend-refs)
  (etypecase refvalue
    (integer (format nil "~40,'0X" refvalue))
    (list (flatten-path-list (xform prepend-refs (curry #'cons "refs") refvalue)))))

;;;
;;; Ref files
;;;
(defun file-path-ref (pathname &aux (dir (pathname-directory pathname)))
  (destructuring-bind (ref repo-dir-head) (split-sequence ".git" (rest dir) :test #'equal)
    (values ref
            (make-pathname :directory (list* (first dir) repo-dir-head)))))

(defun ref-file-path (ref &optional (directory *repository*))
  (subfile directory (list* ".git" "refs" (canonicalise-ref ref))))

(defun ref-file-present-p (name &optional (directory *repository*))
  (probe-file (ref-file-path (xform-if-not #'listp #'downstring name) directory)))

(defun ref-file-value (pathname)
  (lret ((refval (parse-commit-id (file-as-string pathname))))
    (unless (not (minusp refval))
      (git-error "~@<Bad value in ref ~S: ~S.~:@>" pathname refval))))

(defun set-ref-file-value (ref directory value)
  (let ((path (ref-file-path ref directory)))
    (ensure-directories-exist path)
    (with-output-to-file (s path)
      (write-string (cook-ref-value value t) s))))

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
;;;
;;; Raw refs & iteration
;;;
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

(defun map-pathnames-full-refs (fn pathnames)
  (iter (for pathname in pathnames)
        (collect (funcall fn (file-path-ref pathname) (ref-file-value pathname)))))

(defun map-heads (fn directory)
  (append (map-pathnames-full-refs fn (head-pathnames directory))
          (map-packed-refs (lambda (r v) (declare (ignore v)) (string= (first r) "heads")) fn directory)))

(defun map-all-remote-heads (fn directory)
  (append (map-pathnames-full-refs fn (all-remote-head-pathnames directory))
          (map-packed-refs (lambda (r v) (declare (ignore v)) (string= (first r) "remotes")) fn directory)))

(defun map-remote-heads (fn remote directory)
  (append (map-pathnames-full-refs fn (remote-head-pathnames remote directory))
          (map-packed-refs (lambda (r v) (declare (ignore v)) (and (string= (first r) "remotes") (string= (second r) remote))) fn directory)))

(defun refs-by-value (refval directory)
  (flet ((ref-if-= (r v) (when (= v refval) r)))
    (remove nil (append (map-heads #'ref-if-= directory)
                        (map-all-remote-heads #'ref-if-= directory)))))
;;;
;;; Refs
;;;
(defun ref-value (ref directory &key (if-does-not-exist :error))
  (let* ((ref (canonicalise-ref ref))
         (path (ref-file-path ref directory)))
    (if (probe-file path)
        (ref-file-value path)
        (or (car (remove nil (map-packed-refs (lambda (r v) (declare (ignore v)) (equal ref r))
                                              (lambda (r v) (declare (ignore r)) v)
                                              directory)))
            (ecase if-does-not-exist
              (:error (git-error "~@<Ref named ~S doesn't exist in git repository at ~S.~:@>" ref (or directory *default-pathname-defaults*)))
              (:continue nil))))))

(defun ref-coerce-to-value (ref-or-value &optional (directory *repository*))
  (if (integerp ref-or-value)
      ref-or-value
      (ref-value ref-or-value directory)))

(defgeneric ref= (ref-x ref-y &optional directory)
  (:method (x y &optional (directory *repository*))
    (ref= (ref-coerce-to-value x directory) (ref-coerce-to-value y directory)))
  (:method ((x integer) (y integer) &optional (directory *repository*))
    (declare (ignore directory))
    (= x y)))

(defun symbolic-reffile-value (pathname &optional dereference)
  (multiple-value-bind (ref directory) (file-path-ref pathname)
    (declare (ignore ref))
    (multiple-value-bind (ref refval) (parse-ref-value (file-line pathname))
      (let ((normalised-ref (rest ref))) ; strip the "refs" component
        (if dereference
            (values (or refval
                        (ref-value normalised-ref directory :if-does-not-exist :continue)
                        (repository-error directory "~@<Reffile ~S references a missing ref ~S.~:@>"
                                          pathname normalised-ref))
                    normalised-ref)
            (or normalised-ref refval))))))

(defun set-symbolic-reffile-value (pathname value)
  (with-output-to-file (reffile pathname)
    (when (consp value)
      (write-string "ref: " reffile))
    (write-string (cook-ref-value value t) reffile)))
;;;
;;; HEAD operation
;;;
(defun head-pathname (&optional (directory *repository*) remote)
  (subfile directory `(".git" ,@(when remote `("refs" "remotes" ,(downstring remote))) "HEAD")))

(defun get-head (&optional (directory *repository*) remote (dereference t))
  (symbolic-reffile-value (head-pathname directory remote) dereference))

(defun set-head (new-value &optional (directory *repository*) remote)
  (declare (type (or cons (integer 0)) new-value))
  (set-symbolic-reffile-value (head-pathname directory remote) new-value))

(defun git-detach-head (&optional (directory *repository*))
  (set-head (get-head directory) directory))

(defun head-detached-p (&optional (directory *repository*) remote)
  (let ((head (get-head directory remote nil)))
    (not (or (integerp head)
             (ref-value head directory :if-does-not-exist :continue)))))

(defun invoke-with-maybe-detached-head (directory detachp fn)
  (if detachp
      (let ((current-head (get-head directory)))
        (unwind-protect (progn (git-detach-head directory)
                               (funcall fn))
          (when current-head
            (set-head current-head directory))))
      (funcall fn)))

(defmacro with-maybe-detached-head ((&optional (directory '*repository) detachp) &body body)
  `(invoke-with-maybe-detached-head ,directory ,detachp  (lambda () ,@body)))

;;;
;;; Branches
;;;
(defun git-branches (&optional (directory *repository*))
  (multiple-value-bind (status output)
      (with-executable-options (:explanation `("listing git branches in ~S" ,*default-pathname-defaults*)
                                :output :capture)
        (git directory "branch"))
    (declare (ignore status))
    (mapcar (compose #'make-keyword #'string-upcase)
            (remove-if
             (lambda (x) (or (zerop (length x)) (and (= 1 (length x)) (char= #\* (schar x 0)))))
             (mapcan (curry #'split-sequence #\Space)
                     (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output)))))))

(defun git-branch-present-p (name &optional (directory *repository*))
  (declare (symbol name))
  (or (ref-file-present-p name directory)
      (member name (git-branches directory) :test #'string=)))

(defun git-remove-branch (name &optional (directory *repository*))
  (with-explanation ("removing git branch ~A in ~S" name directory)
    (nth-value 0 (git directory "branch" "-d" (downstring name)))))

(defun git-set-noncurrent-branch (branchname &optional (directory *repository*) (refvalue (get-head directory)))
  (declare (type (or list (integer 0)) refvalue))
  (with-explanation ("moving non-current ref ~A to ~:[~40,'0X~;~A~] in ~S"
                     branchname (consp refvalue) refvalue directory)
    (nth-value 0 (git directory "branch" "-f" (downstring branchname) (cook-ref-value refvalue)))))

(defun git-set-branch (name &optional (directory *repository*) (refvalue (get-head directory)) possibly-current-p)
  (with-maybe-detached-head (directory possibly-current-p)
    (git-set-noncurrent-branch name directory refvalue)))

(defun git-set-branch-index-tree (&optional ref (directory *repository*))
  "Set the current branch, the index and the working tree to the state
at REF.  When REF is NIL, set the index and the working tree to the
ref of the current branch."
  (with-explanation ("hard-resetting repository in ~S~:[~; to ~:*~S~]" directory ref)
    (apply #'git directory "reset" "--hard" (when ref (list (flatten-path-list ref) "--")))))

(defun git-stash (&optional (directory *repository*))
  "Same as GIT-SET-BRANCH-INDEX-TREE with no arguments, but saves changes
in a temporary pseudo-commit."
  (with-explanation ("stashing changes in git repository at ~S" directory)
    (git directory "stash")))

(defun ensure-clean-repository (if-changes &optional (directory *repository*))
  (with-retry-restarts ((hardreset-repository ()
                          :report "Clear all uncommitted changes, both staged and unstaged."
                          (git-set-branch-index-tree nil directory)))
    (when (git-repository-changes-p directory)
      (ecase if-changes
        (:stash (git-stash directory))
        (:reset (git-set-branch-index-tree nil directory))
        (:error (git-error "~@<~:[Uns~;S~]taged changes in git repository ~S.~:@>"
                           (git-repository-staged-changes-p directory) directory))))))

(defun git-set-head-index-tree (ref &optional (if-changes :stash) (directory *repository*))
  (let ((ref (mapcar #'downstring (ensure-cons ref))))
    (ref-value ref directory)
    (ensure-clean-repository if-changes directory)
    (with-explanation ("checking out ~S in ~S" ref directory)
      (git directory "checkout" (flatten-path-list ref)))))

(defun invoke-with-branch-change (directory before-branch after-branch if-changes fn)
  (unwind-protect (progn
                    (git-set-head-index-tree before-branch if-changes directory)
                    (funcall fn))
    (git-set-head-index-tree after-branch if-changes directory)))

(defmacro with-branch-change ((before-branch-form after-branch-form &key (if-changes :stash) (directory '*repository*))
                              &body body)
  `(invoke-with-branch-change ,directory ,before-branch-form ,after-branch-form ,if-changes
                              (lambda () ,@body)))

(defun invoke-with-gitremote (remote-name fn
                              &key url (if-remote-does-not-exist :create) (directory *repository*))
  (unless (find-gitremote remote-name directory)
    (ecase if-remote-does-not-exist
      (:create (ensure-gitremote remote-name url directory))
      (:error  (missing-remote-error remote-name directory))))
  (funcall fn))

(defmacro with-gitremote ((remote &key (url nil urlp) (if-remote-does-not-exist nil if-remote-does-not-exist-p)
                                  (directory nil directoryp))
                          &body body)
  `(invoke-with-gitremote ,remote (lambda () ,@body) ,@(pass-&key* url if-remote-does-not-exist directory)))

;;;;
;;;; Repository-level operations
;;;;
(defun invoke-with-git-repository-write-access (path fn &aux
                                                (dotgit (dotgit path))
                                                (*repository* path))
  (with-directory (path :if-does-not-exist :create)
    (handler-bind ((error (lambda (c)
                            (declare (ignore c))
                            ;; Maintain the no-useless-directories-added invariant.
                            ;; non-negotiable
                            (when (and (directory-created-p)
                                       (not (directory-has-git-objects-p dotgit)))
                              (fad:delete-directory-and-files path))
                            #| Continue signalling. |#)))
      (let ((effectively-new (or (directory-created-p)
                                 (directory-empty-p path))))
        (unless (or effectively-new
                    (directory-has-git-objects-p dotgit))
          (ecase (repository-policy-value :preexisting-gitless)
            (:error     (error 'empty-repository :pathname path))
            (:take-over nil)))
        (funcall fn effectively-new)))))

(defmacro with-git-repository-write-access ((new-repo-p) path &body body)
  `(invoke-with-git-repository-write-access ,path (lambda (,new-repo-p) ,@body)))

;;;;
;;;; Queries
;;;;
(defun prefixp (prefix sequence)
  (nth-value 1 (starts-with-subseq prefix sequence :return-suffix t)))

(defun git-commit-log (ref &optional (directory *repository*))
  "Given a REF, return the commit id, author, date and commit message
of the corresponding commit as multiple values."
  (multiple-value-bind (status output)
      (with-executable-options (:explanation `("querying commit log of ~S at ~S" ,ref ,directory)
                                :output :capture)
        (git directory "log" "-1" (cook-ref-value ref)))
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
          (git-error "~@<Error parsing commit log of ~X at ~S.~:@>" ref directory))
        (let ((message (string-right-trim '(#\Newline)
                                          (subseq output (+ 5 posn)))))
          (make-commit (parse-commit-id commit-id) date author message))))))

(defun git-repository-last-version-from-tag (&optional (directory *repository*))
  (multiple-value-bind (successp output)
      (with-explanation ("determining version last recorded in ~S" directory)
        (git directory "log" "-1" "--tags" "--decorate=short"))
    (declare (ignore successp))
    (when-let ((version (first (extract-delimited-substrings output "tag: upstream/" #\,))))
      (mapcar #'parse-integer (split-sequence #\. version)))))
