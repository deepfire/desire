;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2008 by
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


(defun metafile-path (name metastore-directory)
  "Return the pathname of the metafile called NAME within METASTORE-DIRECTORY."
  (subfile* metastore-directory (symbol-name name)))

(defun metafile-present-p (name metastore-directory)
  "See if metafile called by NAME exists within METASTORE-DIRECTORY."
  (file-exists-p (metafile-path name metastore-directory)))

(defun metafile-empty-p (name metastore-directory &aux (path (metafile-path name metastore-directory)))
  "See if metafile called by NAME within METASTORE-DIRECTORY is empty."
  (or (not (file-exists-p path))
      (with-open-file (stream path)
        (zerop (file-length stream)))))

(defun create-metafile (name metastore-directory &key (if-exists :append))
  "Ensure that metafile called by NAME exists within METASTORE-DIRECTORY.
  
   The IF-EXISTS keyword is passed to OPEN."
  (open (merge-pathnames (string name) metastore-directory) :direction :probe :if-does-not-exist :create :if-exists if-exists)
  (with-explanation ("creating metafile ~A in ~S" name metastore-directory)
    (git metastore-directory "add" (string name))))

(defmacro with-open-metafile ((stream name &optional (metastore-directory '*repository*) &rest open-options) &body body)
  `(with-open-file (,stream (metafile-path ,name ,metastore-directory) :element-type 'character 
                                 ,@(remove-from-plist open-options :element-type))
     ,@body))

(defun invoke-with-output-to-new-metafile (metastore-directory name commit-p commit-message fn)
  (lret (return-value)
    (let ((content (with-output-to-string (stream)
                     (setf return-value (funcall fn stream)))))
      (with-output-to-file (stream (metafile-path name metastore-directory) :element-type 'character :if-does-not-exist :create)
        (write-string content stream)))
    (when (and commit-p (repository-changes-p metastore-directory))
      (commit-metafile name metastore-directory commit-message))))

(defmacro with-output-to-new-metafile ((stream name metastore-directory &key commit-p commit-message) &body body)
  `(invoke-with-output-to-new-metafile ,metastore-directory ,name ,commit-p ,commit-message (lambda (,stream) ,@body)))

(defun commit-metafile (name metastore-directory &optional (commit-message (format nil "Updated ~A" name)))
  "Commit contents of the metafile called by NAME in METASTORE-DIRECTORY.
Return status indicates whether there were changes and a new commit was done."
  (with-explanation ("committing changes (if any) to metafile ~A in ~S" (symbol-name name) metastore-directory)
    (git metastore-directory "commit" "-m" (format nil "~A" commit-message) (symbol-name name))))

(defun metastore-present-p (directory &optional required-metafiles)
  "Determine if a metastore is present in DIRECTORY, optionally
additionally requiring that REQUIRED-METAFILES are present."
  (and (directory-exists-p (merge-pathnames ".git/" directory))
       (every (compose #'file-exists-p
                       (rcurry #'metafile-path directory))
              required-metafiles)))

(defun init-metastore (directory &optional required-metafiles (publicp t))
  "Initialise metastore in DIRECTORY, with optional, empty
REQUIRED-METAFILES."
  (with-explanation ("initialising git metastore database in ~S" directory)
    (init-repo directory))
  (when publicp
    (open (merge-pathnames ".git/git-daemon-export-ok" directory) :direction :probe :if-does-not-exist :create))
  (dolist (mf required-metafiles)
    (create-metafile mf directory)
    (commit-metafile mf directory (format nil "Created metafile ~A" mf)))
  t)

(defun ensure-metastore (directory &key required-metafiles (public t))
  "Ensure that a metastore exists at DIRECTORY.

   Returns T if the metastore was created, or updated; NIL otherwise."
  (unless (metastore-present-p directory required-metafiles)
    (init-metastore directory required-metafiles public)))
