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

(defun gitvar (var &optional directory)
  (declare (type symbol var))
  (maybe-within-directory directory
    (multiple-value-bind (status output)
        (git (list "config" (string-downcase (symbol-name var))) :valid-exit-codes `((0 . nil) (1 . :unset)) :output t)
      (or status (string-right-trim '(#\Return #\Newline) output)))))

(defun (setf gitvar) (val var &optional directory)
  (declare (type symbol var) (type string val))
  (maybe-within-directory directory
    (git (list "config" "--replace-all" (string-downcase (symbol-name var)) val))))

(defun gitbranches (&optional directory)
  (maybe-within-directory directory
    (let ((output (execution-output-string 'git "branch")))
      (remove '* (mapcar (compose #'intern #'string-upcase) 
                         (mapcan (curry #'split-sequence #\Space)
                                 (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output))))))))

(defun gitremotes (&optional directory)
  (maybe-within-directory directory
    (let ((output (execution-output-string 'git "remote")))
      (mapcar (compose #'intern #'string-upcase) (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output))))))

(defun gitremote-present-p (name &optional directory)
  (member name (gitremotes directory)))

(defun add-gitremote (name url &optional directory)
  (maybe-within-directory directory
    (git "remote" "add" (downstring name) url)))

(defun ensure-gitremote (name url &optional directory)
  (unless (gitremote-present-p name directory)
    (add-gitremote name url directory)))

(defun repository-present-p (&optional directory)
  "See if MODULE repository and source code is available at LOCALITY."
  (and (if directory (directory-exists-p directory) t)
       (maybe-within-directory directory
         (and (directory-exists-p (subdirectory* nil ".git"))
              (not (null (gitbranches)))))))

(defun repository-bare-p (&optional directory)
  (maybe-within-directory directory
    (null (directory-exists-p (subdirectory* nil ".git")))))

(defun (setf repository-bare-p) (val directory)
  (within-directory directory
    (if val
        (error "not implemented")
        (progn
          (let ((git-files (directory (make-pathname :directory '(:relative) :name :wild))))
            (sb-posix:mkdir ".git" #o755)
            (dolist (filename git-files)
              (move-to-directory filename (make-pathname :directory '(:relative ".git") :name (pathname-name filename) :type (pathname-type filename)))))
          (setf (gitvar 'core.bar) "false")
          (git "reset" "--hard")
          nil))))

(defun repository-world-readable-p (&optional directory)
  "See, whether or not MODULE within LOCALITY is allowed to be exported
   for the purposes of git-daemon."
  (file-exists-p (subfile* directory ".git" "git-daemon-export-ok")))

(defun (setf repository-world-readable-p) (val &optional directory)
  (let ((path (subfile* directory ".git" "git-daemon-export-ok")))
    (if val
        (open path :direction :probe :if-does-not-exist :create)
        (and (delete-file path) nil))))

(defun ensure-master-branch-from-remote (&key directory (remote-name (first (gitremotes directory))))
  (maybe-within-directory directory
    (unless (find 'master (gitbranches))
      (git "checkout" "-b" "master" (fuse-downcased-string-path-list (list remote-name "master"))))))

(defmacro within-ref (ref &body body)
  `(progn
     (git "checkout" (flatten-path-list ,ref))
     (unwind-protect (progn ,@body)
       (git "checkout" "master"))))
