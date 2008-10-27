;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLING; Base: 10 -*-
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

(in-package :cling)

(defmacro within-repository ((repo &rest pathname-elements) &body body)
  `(with-changed-directory (namestring (make-pathname :directory (append (pathname-directory (path ,repo)) (list ,@pathname-elements))))
     ,@body))

(defun repository-bare-p (repo)
  (within-repository (repo)
    (null (probe-file ".git"))))

(defun (setf repository-bare-p) (val repo)
  (within-repository (repo)
    (if val
        (error "not implemented")
        (progn
          (let ((git-files (directory (make-pathname :directory '(:relative) :name :wild))))
            (sb-posix:mkdir ".git" #o755)
            (dolist (filename git-files)
              (move-to-directory filename (make-pathname :directory '(:relative ".git") :name (pathname-name filename) :type (pathname-type filename)))))
          (git "config" "--replace-all" "core.bare" "false")
          (git "checkout" "master")
          (git "reset" "--hard")
          nil))))

(defun world-readable-p (repo)
  (within-repository (repo ".git")
    (not (null (probe-file "git-daemon-export-ok")))))

(defun (setf world-readable-p) (val repo)
  (within-repository (repo ".git")
    (if val
        (with-open-file (s "git-daemon-export-ok" :if-does-not-exist :create) t)
        (and (delete-file "git-daemon-export-ok") nil))))

(defun repo-var (repo var)
  (declare (type git-repository repo) (type symbol var))
  (within-repository (repo)
    (multiple-value-bind (status output) (run-external-program 'git (list "config" (string-downcase (symbol-name var))) :valid-exit-codes `((0 . nil) (1 . :unset)) :capture-output t)
      (or status (string-right-trim '(#\Return #\Newline) output)))))

(defun (setf repo-var) (val repo var)
  (declare (type git-repository repo) (type symbol var) (type string val))
  (within-repository (repo)
    (run-external-program 'git (list "config" "--replace-all" (string-downcase (symbol-name var)) val))))