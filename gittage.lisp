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


(defmacro within-module-repository ((dir module locality) &body body)
  `(within-directory (,dir (module-path ,module ,locality))
     ,@body))

(defvar *purgeworth-binaries* 
  '("dfsl"        ;; OpenMCL
    "ppcf" "x86f" ;; CMUCL
    "fasl"        ;; SBCL
    "fas" "o"     ;; ECL
    "lib" "obj"   ;; ECL/win32
    )) 

(defun purge-module-binaries (module &optional (locality (master 'git)))
  "Purge MODULE's files with type among one of *PURGEWORTH-BINARIES* in
   LOCALITY."
  (dolist (type *purgeworth-binaries*)
    (mapc #'delete-file (directory (subfile (module-path module locality) '(:wild-inferiors :wild) :type type)))))

(define-reported-condition repository-not-clean-during-fetch (repository-error external-program-failure) ()
  (:report (locality module)
           "~@<repository for ~S in ~S has uncommitted changes during fetch~:@>" module locality))

(defun repo-var (module var &optional (locality (master 'git)))
  (declare (type git-locality locality) (type symbol var))
  (within-module-repository (dir module locality)
    (multiple-value-bind (status output)
        (run-external-program 'git (list "config" (string-downcase (symbol-name var))) :valid-exit-codes `((0 . nil) (1 . :unset)) :output t)
      (or status (string-right-trim '(#\Return #\Newline) output)))))

(defun (setf repo-var) (val module var &optional (locality (master 'git)))
  (declare (type git-locality locality) (type symbol var) (type string val))
  (within-module-repository (dir module locality)
    (run-external-program 'git (list "config" "--replace-all" (string-downcase (symbol-name var)) val))))

(defun module-gitbranches (module &optional (locality (master 'git)))
  (within-module-repository (dir module locality)
    (let ((output (external-program-output-as-string 'git "branch")))
      (remove '* (mapcar (compose #'intern #'string-upcase) 
                         (mapcan (curry #'split-sequence #\Space)
                                 (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output))))))))

(defun module-gitremotes (module &optional (locality (master 'git)))
  (within-module-repository (dir module locality)
    (let ((output (external-program-output-as-string 'git "remote")))
      (mapcar (compose #'intern #'string-upcase) (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output))))))

(defun module-add-gitremote (module gitremote &aux (locality (master 'git)))
  (within-directory (dir (module-path module locality))
    (git "remote" "add" (down-case-name gitremote) (url gitremote module))))

(defun ensure-module-gitremote (module gitremote &aux (locality (master 'git)))
  (unless (member (name gitremote) (module-gitremotes module locality))
    (module-add-gitremote module gitremote)))

(defun ensure-module-master-branch-from-remote (module locality &optional (remote-name (first (module-gitremotes module locality))))
  (unless (find 'master (module-gitbranches module locality))
    (within-directory (dir (module-path module locality))
      (git "checkout" "-b" "master" (concatenate 'string (downstring remote-name) "/master")))))

(defun module-bare-p (module &optional (locality (master 'git)))
  "See, whether or not MODULE within LOCALITY has its source checked out."
  (null (directory-exists-p (subdirectory* (module-path module locality) ".git"))))

(defun (setf module-bare-p) (val module &optional (locality (master 'git)))
  (within-module-repository (dir module locality)
    (if val
        (error "not implemented")
        (progn
          (let ((git-files (directory (make-pathname :directory '(:relative) :name :wild))))
            (sb-posix:mkdir ".git" #o755)
            (dolist (filename git-files)
              (move-to-directory filename (make-pathname :directory '(:relative ".git") :name (pathname-name filename) :type (pathname-type filename)))))
          (setf (repo-var module 'core.bar locality) "false")
          (git "reset" "--hard")
          nil))))

(defun determine-module-presence (module &optional (locality (master 'git)))
  "See if MODULE repository and source code is available at LOCALITY."
  (let ((repo (module-path module locality)))
    (and (directory-exists-p repo)
         (directory-exists-p (subdirectory* repo ".git"))
         (within-directory (repo repo)
           (not (null (module-gitbranches module locality)))))))

(defun module-present-p (module &optional (locality (master 'git)) check-when-present-p)
  "See if MODULE's presence cache is positive for LOCALITY, failing that check the
   repository, and update the cache if it is found.

   CHECK-WHEN-PRESENT-P determines if presence check is performed when MODULE's cache
   is positive."
  (with-slots (scan-positive-localities) module
    (if-let ((cache-hit (find locality scan-positive-localities))
             (no-cache-revalidation (null check-when-present-p)))
      t
      (lret ((actually-present-p (determine-module-presence module locality)))
        (if actually-present-p
            (push locality scan-positive-localities)
            (removef scan-positive-localities locality))))))

(defun module-world-readable-p (module &optional (locality (master 'git)))
  "See, whether or not MODULE within LOCALITY is allowed to be exported
   for the purposes of git-daemon."
  (file-exists-p (subfile* (module-path module locality) ".git" "git-daemon-export-ok")))

(defun (setf module-world-readable-p) (val module &optional (locality (master 'git)))
  (let ((path (subfile* (module-path module locality) ".git" "git-daemon-export-ok")))
    (if val
        (open path :direction :probe :if-does-not-exist :create)
        (and (delete-file path) nil))))
