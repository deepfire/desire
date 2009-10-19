;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2008 by
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


(defun invoke-maybe-within-directory (fn &optional directory)
  (if directory
      (within-directory directory
        (funcall fn))
      (funcall fn)))

(defmacro maybe-within-directory (directory &body body)
  `(invoke-maybe-within-directory (lambda () ,@body) ,directory))

(define-condition vcs-condition ()
  ((vcs :accessor condition-vcs :initarg :vcs)))

(define-condition git-condition (vcs-condition)
  ()
  (:default-initargs :vcs 'git))

(define-condition git-error (error git-condition)
  ())

(define-simple-error git-error)

;;;
;;; Repositories
;;;
(defun git-repository-present-p (&optional directory)
  "See if MODULE repository and source code is available at LOCALITY."
  (and (if directory (directory-exists-p directory) t)
       (maybe-within-directory directory
         (and (directory-exists-p (subdirectory* nil ".git"))
              (not (null (gitbranches)))))))

(defun git-repository-bare-p (&optional directory)
  (maybe-within-directory directory
    (null (directory-exists-p (subdirectory* nil ".git")))))

(defun git-repository-reset-hard (&optional ref directory)
  (maybe-within-directory directory
    (with-explanation ("hard-resetting repository in ~S to ~:[master~;~:*~S~]" *default-pathname-defaults* ref)
      (apply #'git "reset" "--hard" (when ref (list (flatten-path-list ref) "--"))))))

(defun (setf git-repository-bare-p) (val directory)
  (within-directory directory
    (if val
        (git-error "~@<Couldn't make git repository at ~S bare: not implemented.~:@>" directory)
        (progn
          (let ((git-files (directory (make-pathname :directory '(:relative) :name :wild))))
            (sb-posix:mkdir ".git" #o755)
            (dolist (filename git-files)
              (move-to-directory filename (make-pathname :directory '(:relative ".git") :name (pathname-name filename) :type (pathname-type filename)))))
          (setf (gitvar 'core.bar) "false")
          (git-repository-reset-hard)
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
        (delete-file path))
    val))

(defun git-repository-unstaged-changes-p (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("determining whether git repository at ~S has unstaged changes" *default-pathname-defaults*)
      (with-avoided-executable-output
        (not (with-shell-predicate (git "diff" "--exit-code")))))))

(defun git-repository-staged-changes-p (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("determining whether git repository at ~S has unstaged changes" *default-pathname-defaults*)
      (with-avoided-executable-output
        (not (with-shell-predicate (git "diff" "--cached" "--exit-code")))))))

(defun git-repository-changes-p (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("determining whether git repository at ~S has staged or unstaged changes" *default-pathname-defaults*)
      (with-avoided-executable-output
        (not (with-shell-predicate (git "diff" "HEAD" "--exit-code")))))))

;;;
;;; Config variables
;;;
(defun gitvar (var &optional directory)
  (declare (type symbol var))
  (maybe-within-directory directory
    (multiple-value-bind (status output)
        (with-explanation ("getting value of git variable ~A" (symbol-name var))
          (git (list "config" (string-downcase (symbol-name var))) :valid-exit-codes `((0 . nil) (1 . :unset)) :output t))
      (or status (string-right-trim '(#\Return #\Newline) output)))))

(defun (setf gitvar) (val var &optional directory)
  (declare (type symbol var) (type string val))
  (maybe-within-directory directory
    (with-explanation ("setting git variable ~A to ~A" (symbol-name var) val)
      (git (list "config" "--replace-all" (string-downcase (symbol-name var)) val)))))

;;;
;;; Branches
;;;
(defun gitbranches (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("listing git branches in ~S" *default-pathname-defaults*)
      (let ((output (execution-output-string 'git "branch")))
        (remove :* (mapcar (compose #'make-keyword #'string-upcase) 
                           (mapcan (curry #'split-sequence #\Space)
                                   (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output)))))))))

(defun gitbranch-present-p (name &optional directory)
  (member name (gitbranches directory) :test #'string=))

(defun add-gitbranch (name ref &optional directory)
  (maybe-within-directory directory
    (with-explanation ("adding a git branch ~A tracking ~S in ~S" name ref *default-pathname-defaults*)
      (git "branch" (downstring name) (flatten-path-list ref)))))

(defun ensure-gitbranch (name ref &optional directory)
  (unless (gitbranch-present-p name directory)
    (add-gitbranch name ref directory)))

;;;
;;; Remotes
;;;
(defun gitremotes (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("listing git remotes in ~S" *default-pathname-defaults*)
      (let* ((output (execution-output-string 'git "remote"))
             (remote-names (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output))))
        (mapcar (compose #'intern #'string-upcase) remote-names)))))

(defun gitremote-present-p (name &optional directory)
  (member name (gitremotes directory) :test #'string=))

(defun add-gitremote (name url &optional directory)
  (maybe-within-directory directory
    (with-explanation ("adding a git remote ~A (~A) in ~S" name url *default-pathname-defaults*)
      (git "remote" "add" (downstring name) url))))

(defun ensure-gitremote (name url &optional directory)
  (unless (gitremote-present-p name directory)
    (add-gitremote name url directory)))

(defun fetch-gitremote (remote-name &optional directory)
  (maybe-within-directory directory
    (with-explanation ("fetching from git remote ~A in ~S" remote-name *default-pathname-defaults*)
      (git "fetch" (downstring remote-name)))))

;;;
;;; Refs
;;;
(defun ref-path (ref directory)
  (subfile directory (list* ".git" "refs" (if (ref-shortp ref)
                                              (list* "heads" ref)
                                              ref))))

(defun read-ref (pathname)
  (let ((*read-base* #x10)
        (*read-eval* nil))
    (lret ((refval (read-from-string (file-as-string pathname))))
      (unless (and (integerp refval) (not (minusp refval)))
        (git-error "~@<Bad value in ref ~S: ~S.~:@>" pathname refval)))))

(defun ref-shortp (ref)
  (endp (rest ref)))

(defun ref-headp (ref)
  (or (ref-shortp ref) (string= "heads" (first ref))))

(defun ref-remotep (ref)
  (and (not (ref-shortp ref)) (string= "remotes" (first ref))))

(defun full-ref-to-pathname (ref directory)
  (merge-pathnames (make-pathname :directory (list* :relative ".git" "refs" (butlast ref)) :name (lastcar ref)) directory))

(defun pathname-to-full-ref (pathname directory)
  (let ((translated (translate-pathname pathname (merge-pathnames #p".git/refs/**/*" directory) #p"/**/*")))
    (append (rest (pathname-directory translated)) (list (pathname-name translated)))))

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

(defun remote-head-ref (remote directory)
  (file-as-string (subfile directory `(".git" "refs" "remotes" ,(downstring remote) "HEAD"))
                  :position 5))

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

(defun ref-value (ref directory &key (if-does-not-exist :error))
  (let ((path (ref-path ref directory)))
    (if (probe-file path)
        (read-ref path)
        (or (car (remove nil (map-packed-refs (lambda (r v) (when (string= "master" (first r)) v)) #'identity directory)))
            (ecase if-does-not-exist
              (:error (git-error "~@<Ref named ~S doesn't exist in git repository at ~S.~:@>" ref directory))
              (:continue nil))))))

(defun map-pathnames-full-refs (fn pathnames directory)
  (iter (for pathname in pathnames)
        (let ((ref (pathname-to-full-ref pathname directory))
              (val (read-ref pathname)))
          (collect (funcall fn ref val)))))

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

(defun git-checkout-ref (ref &optional directory &key (if-changes :error))
  "This assumes that the local 'master' branch is present."
  (maybe-within-directory directory
    (with-retry-restarts ((hardreset-repository () (git-repository-reset-hard)))
      (unless (eq if-changes :ignore)
        (when (git-repository-changes-p directory)
          (ecase if-changes
            (:warn (warn "~@<WARNING: in git repository ~S: asked to check out ~S, but there were ~:[un~;~]staged changes. Proceeding, by request.~:@>"
                         (or directory *default-pathname-defaults*) ref (git-repository-staged-changes-p directory)))
            (:error (git-error "~@<In git repository ~S: asked to check out ~S, but there were ~:[un~;~]staged changes.~:@>"
                               (or directory *default-pathname-defaults*) ref (git-repository-staged-changes-p directory))))))
      (with-explanation ("checking out ~S in ~S" ref *default-pathname-defaults*)
        (git "checkout" (flatten-path-list ref))))))

(defmacro with-git-ref (ref &body body)
  `(unwind-protect (progn (git-checkout-ref ,ref)
                          ,@body)
     (git-checkout-ref '("master"))))

(defun checkout-gitbranch (name directory &optional reset-before-checkout &key (if-does-not-exist :error) default-ref (if-changes :error))
  (unless (gitbranch-present-p name directory)
    (ecase if-does-not-exist 
      (:error (git-error "~@<Asked to check out a nonexistent branch ~A in ~S~:@>" name directory))
      (:create (if default-ref
                   (add-gitbranch name default-ref directory)
                   (git-error "~@<While checking out branch ~A in ~S: branch doesn't exist and the default ref was not provided.~:@>" name directory)))))
  (when reset-before-checkout
    (git-repository-reset-hard nil directory))
  (git-checkout-ref `(,(downstring name)) directory :if-changes if-changes))

(defun reset-gitbranch-to-remote-branch (name qualified-remote-branch-name directory &optional reset-before-checkout)
  (checkout-gitbranch name directory reset-before-checkout)
  (git-repository-reset-hard (list* "remotes" qualified-remote-branch-name) directory))
