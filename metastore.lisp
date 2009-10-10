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
  (within-directory (metastore-directory)
    (open (string name) :direction :probe :if-does-not-exist :create :if-exists if-exists)
    (with-explanation ("creating metafile ~A in ~S" name *default-pathname-defaults*)
      (git "add" (string name)))))

(defmacro with-open-metafile ((stream name &optional (metastore-directory '*default-pathname-defaults*) &rest open-options) &body body)
  `(with-open-file (,stream (metafile-path ,name ,metastore-directory) :element-type 'character 
                                 ,@(remove-from-plist open-options :element-type))
     ,@body))

(defmacro with-output-to-new-metafile ((stream name metastore-directory &rest open-options &key commit-p commit-message &allow-other-keys) &body body)
  (once-only (name metastore-directory)
    `(prog1
         (with-output-to-file (,stream (metafile-path ,name ,metastore-directory) :element-type 'character :if-does-not-exist :error
                                       ,@(remove-from-plist open-options :element-type :if-does-not-exist :if-exists :commit-p :commit-message))
           ,@body)
       ,@(when commit-p
           `((when ,commit-p
               (commit-metafile ,name ,metastore-directory ,@(maybecall commit-message #'list commit-message))))))))

(defmacro appending-to-metafile ((stream name metastore-directory &rest open-options) &body body)
  `(with-open-file (,stream (metafile-path ,name ,metastore-directory) :element-type 'character :if-does-not-exist :error :if-exists :append
                            ,@(remove-from-plist open-options :element-type :if-does-not-exist :if-exists))
     ,@body))

(defun commit-metafile (name metastore-directory &optional (commit-message (format nil "Updated ~A" name)))
  "Commit contents of the metafile called by NAME in METASTORE-DIRECTORY.
Return status indicates whether there were changes and a new commit was done."
  (within-directory (metastore-directory)
    (with-explanation ("committing changes (if any) to metafile ~A in ~S" (symbol-name name) *default-pathname-defaults*)
      (git "commit" "-m" (format nil "~A" commit-message) (symbol-name name)))))

(defun metastore-present-p (directory &optional required-metafiles)
  "Determine if a metastore is present in DIRECTORY, optionally
additionally requiring that REQUIRED-METAFILES are present."
  (and (directory-exists-p (subdirectory* directory ".git"))
       (every (compose #'file-exists-p
                       (rcurry #'metafile-path directory))
              required-metafiles)))

(defun init-metastore (directory &optional required-metafiles)
  "Initialise metastore in DIRECTORY, with optional, empty
REQUIRED-METAFILES."
  (within-directory (directory :if-does-not-exist :create :if-exists :error)
    (with-explanation ("initialising git metastore database in ~S" *default-pathname-defaults*)
      (git "init"))
    (open  ".git/git-daemon-export-ok" :direction :probe :if-does-not-exist :create)
    (dolist (mf required-metafiles)
      (create-metafile mf directory)
      (commit-metafile mf directory (format nil "Created metafile ~A" mf)))
    t))

(defun ensure-metastore (directory &key required-metafiles)
  "Ensure that a metastore exists at DIRECTORY.

   Returns T if the metastore was created, or updated; NIL otherwise."
  (unless (metastore-present-p directory required-metafiles)
    (init-metastore directory required-metafiles)))
