;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
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


(define-executable git :may-want-display t :fixed-environment ("HOME=/tmp" "PAGER=/bin/cat"))

(define-condition vcs-condition ()
  ((vcs :reader condition-vcs :initarg :vcs)))

(define-condition git-condition (vcs-condition)
  ()
  (:default-initargs :vcs 'git))

(define-condition git-error (error git-condition)
  ())

(define-simple-error git-error)

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

;;;;
;;;; Repositories
;;;;
(defun git-repository-has-objects-p (directory)
  (not (null (or (directory (subfile directory '(".git" "objects" "pack" :wild) :type :wild))
                 (find-if (lambda (x) (= 2 (length (lastcar (pathname-directory x)))))
                          (directory (subdirectory directory '(".git" "objects" :wild))))))))

(defun git-repository-present-p (&optional (directory *default-pathname-defaults*))
  "See if MODULE repository and source code is available at LOCALITY."
  (and (directory-exists-p directory)
       (within-directory (directory)
         (and (directory-exists-p (subdirectory* nil ".git"))
              (git-repository-has-objects-p nil)))))

(defun git-repository-bare-p (&optional directory)
  (maybe-within-directory directory
    (null (directory-exists-p (subdirectory* nil ".git")))))

(defun move-to-directory (pathname target-directory)
  (if (pathname-name pathname)
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (pathname-directory target-directory) :name (pathname-name pathname))))
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (append (pathname-directory target-directory) (list (lastcar (pathname-directory pathname)))))))))

(defun (setf git-repository-bare-p) (val &optional directory)
  (maybe-within-directory directory
    (if val
        (git-error "~@<Couldn't make git repository at ~S bare: not implemented.~:@>" directory)
        (progn
          (let ((git-files (directory (make-pathname :directory '(:relative) :name :wild))))
            (sb-posix:mkdir ".git" #o755)
            (dolist (filename git-files)
              (move-to-directory filename (make-pathname :directory '(:relative ".git") :name (pathname-name filename) :type (pathname-type filename)))))
          (setf (gitvar 'core.bare) "false")
          (git-set-branch-index-tree)
          nil))))

(defun git-repository-world-readable-p (&optional directory)
  "See, whether or not MODULE within LOCALITY is allowed to be exported
   for the purposes of git-daemon."
  (file-exists-p (subfile* directory ".git" "git-daemon-export-ok")))

(defun (setf git-repository-world-readable-p) (val &optional directory)
  "Change git-daemon's idea about world-readability of DIRECTORY."
  (let ((path (subfile* directory ".git" "git-daemon-export-ok")))
    (if val
        (open path :direction :probe :if-does-not-exist :create)
        (when (file-exists-p path)
          (delete-file path)))
    val))

(defun git-repository-unstaged-changes-p (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("determining whether git repository at ~S has unstaged changes" *default-pathname-defaults*)
      (with-avoided-executable-output ()
        (not (with-valid-exit-codes ((0 t)
                                     (1 nil)
                                     (128 nil))
               (git "diff" "--exit-code")))))))

(defun git-repository-staged-changes-p (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("determining whether git repository at ~S has staged changes" *default-pathname-defaults*)
      (with-avoided-executable-output ()
        (not (with-valid-exit-codes ((0 t)
                                     (1 nil)
                                     (128 nil))
               (git "diff" "--cached" "--exit-code")))))))

(defun git-repository-changes-p (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("determining whether git repository at ~S has staged or unstaged changes" *default-pathname-defaults*)
      (not (with-valid-exit-codes ((0 t)
                                   (1 nil)
                                   (128 nil))
             (git "diff" "--exit-code" "--summary" "HEAD" "--"))))))

(defun git-repository-status (&optional directory)
  "Examine status of the git repository within DIRECTORY and return lists of pathnames as multiple values.
The lists of pathnames returned have following semantics:
    - staged modified,
    - staged deleted,
    - staged new,
    - unstaged modified,
    - unstaged deleted."
  (maybe-within-directory directory
    (with-explanation ("determining status of git repository at ~S" *default-pathname-defaults*)
      (multiple-value-bind (status output) (with-captured-executable-output ()
                                             (with-shell-predicate
                                                 (git "status")))
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
                  (finally (return-from outer (values staged-modified staged-deleted staged-new unstaged-modified unstaged-deleted untracked))))))))))

(defun git-repository-update-for-dumb-servers (&optional directory)
  (maybe-within-directory directory
    (git "update-server-info")))

;;;
;;; Config variables
;;;
(defun gitvar (var &optional directory)
  (declare (type symbol var))
  (maybe-within-directory directory
    (with-captured-executable-output ()
      (multiple-value-bind (setp output)
          (with-explanation ("getting value of git variable ~A" (symbol-name var))
            (with-shell-predicate
              (git "config" (string-downcase (symbol-name var)))))
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
    (with-explanation ("listing git remotes in ~S" *default-pathname-defaults*)
      (multiple-value-bind (status output) (with-captured-executable-output ()
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
              (finally (return (values names urls))))))))

(defun git-remote-present-p (name &optional url directory)
  (multiple-value-bind (remote-names urls) (gitremotes directory)
    (iter (for remote-name in remote-names)
          (for remote-url in urls)
          (finding remote-name
                   such-that (and (eq name remote-name)
                                  (or (not url)
                                      (string= url remote-url)))))))

(defun git-add-remote (name url &optional directory)
  (maybe-within-directory directory
    (with-explanation ("adding a git remote ~A (~A) in ~S" name url *default-pathname-defaults*)
      (git "remote" "add" (downstring name) url))))

(defun ensure-gitremote (name url &optional directory)
  (let ((present-p (git-remote-present-p name url directory)))
    (unless present-p
      (when (git-remote-present-p name nil directory) ; exists, but with a different name?
        (git "remote" "rm" (downstring name)))        ; must clean..
      (git-add-remote (downstring name) url directory))))

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
          (values (or refval (ref-value normalised-ref directory)) normalised-ref)
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
    (with-explanation ("listing git branches in ~S" *default-pathname-defaults*)
      (multiple-value-bind (status output) (with-captured-executable-output ()
                                             (git "branch"))
        (declare (ignore status))
        (mapcar (compose #'make-keyword #'string-upcase) 
                (remove-if
                 (lambda (x) (or (zerop (length x)) (and (= 1 (length x)) (char= #\* (schar x 0)))))
                 (mapcan (curry #'split-sequence #\Space)
                         (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output)))))))))


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

;;;;
;;;; Queries
;;;;
(defmacro if-bind (var test &body then/else)
  "Anaphoric IF control structure.

VAR (a symbol) will be bound to the primary value of TEST. If
TEST returns a true value then THEN will be executed, otherwise
ELSE will be executed."
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro cond-bind (var &body clauses)
  "Just like COND but VAR will be bound to the result of the
  condition in the clause when executing the body of the clause."
  (if clauses
      (destructuring-bind ((test &rest body) &rest others)
          clauses
        `(if-bind ,var ,test
                  (progn ,@body)
                  (cond-bind ,var ,@others)))
      nil))

(defun prefixp (prefix sequence)
  (nth-value 1 (starts-with-subseq prefix sequence :return-suffix t)))

(defun git-commit-log (ref &optional repository-dir)
  "Given a REF and an optional REPOSITORY-DIR, return the commit id, author, date
and commit message of the corresponding commit as multiple values."
  (maybe-within-directory repository-dir
    (with-explanation ("querying commit log of ~S at ~S" ref *default-pathname-defaults*)
      (multiple-value-bind (status output) (with-captured-executable-output ()
                                             (git "log" "-1" (cook-refval ref)))
        (declare (ignore status))
        (with-input-from-string (s output)
          (let (commit-id author date (posn 0))
            (iter (for line = (read-line s nil nil))
                  (while (and line (plusp (length line))))
                  (incf posn (1+ (length line)))
                  (cond-bind suffix
                    ((prefixp "commit " line)  (setf commit-id suffix))
                    ((prefixp "Author: " line) (setf author suffix))
                    ((prefixp "Date:   " line) (setf date suffix))))
            (unless (and commit-id author date)
              (git-error "~@<Error parsing commit log of ~X at ~S.~:@>" ref *default-pathname-defaults*))
            (let ((message (string-right-trim '(#\Newline)
                                              (subseq output (+ 5 posn)))))
              (make-commit (parse-commit-id commit-id) date author message))))))))

(defun git-repository-last-version-from-tag (&optional repository-dir)
  (maybe-within-directory repository-dir
    (multiple-value-bind (successp output)
        (with-explanation ("determining version last recorded in ~S" *default-pathname-defaults*)
          (git "log" "-1" "--tags" "--decorate=short"))
      (declare (ignore successp))
      (when-let ((version (first (extract-delimited-substrings output "tag: upstream/" #\,))))
        (mapcar #'parse-integer (split-sequence #\. version))))))
