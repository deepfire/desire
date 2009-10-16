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
    (with-explanation ("hard-resetting repository in ~S to ~:[HEAD~;~:*~S~]" *default-pathname-defaults* ref)
      (apply #'git "reset" "--hard" (when ref (flatten-path-list ref))))))

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
      (not (with-shell-predicate (git "diff" "--exit-code"))))))

(defun git-repository-staged-changes-p (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("determining whether git repository at ~S has unstaged changes" *default-pathname-defaults*)
      (not (with-shell-predicate (git "diff" "--cached" "--exit-code"))))))

(defun git-repository-changes-p (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("determining whether git repository at ~S has staged or unstaged changes" *default-pathname-defaults*)
      (not (with-shell-predicate (git "diff" "HEAD" "--exit-code"))))))

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

(defun add-gitbranch (name &optional directory)
  (maybe-within-directory directory
    (with-explanation ("adding a git branch ~A in ~S" name *default-pathname-defaults*)
      (git "branch" (downstring name)))))

(defun ensure-gitbranch (name &optional directory)
  (unless (gitbranch-present-p name directory)
    (add-gitbranch name directory)))

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
(defun git-checkout-ref (ref &optional directory &key (if-changes :error))
  (maybe-within-directory directory
    (with-retry-restarts ((hardreset-repository () (git-repository-reset-hard)))
      (when (git-repository-changes-p directory)
        (ecase if-changes
          (:continue)
          (:warn (warn "~@<WARNING: in git repository ~S: asked to check out ~S, but there were ~:[un~;~]staged changes. Proceeding, by request.~:@>"
                       (or directory *default-pathname-defaults*) ref (git-repository-staged-changes-p directory)))
          (:error (git-error "~@<In git repository ~S: asked to check out ~S, but there were ~:[un~;~]staged changes.~:@>"
                             (or directory *default-pathname-defaults*) ref (git-repository-staged-changes-p directory)))))
      (with-explanation ("checking out ~S in ~S" ref *default-pathname-defaults*)
        (git "checkout" (flatten-path-list ref))))))

(defmacro with-git-ref (ref &body body)
  `(unwind-protect (progn (git-checkout-ref ,ref)
                          ,@body)
     (git-checkout-ref '("master"))))
