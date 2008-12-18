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
  (open (metafile-path name metastore-directory) :direction :probe :if-does-not-exist :create :if-exists if-exists))

(defmacro with-open-metafile ((stream name metastore-directory &rest open-options) &body body)
  `(with-open-file (,stream (metafile-path ,name ,metastore-directory) :element-type 'character 
                                 ,@(remove-from-plist open-options :element-type))
     ,@body))

(defmacro with-output-to-new-metafile ((stream name metastore-directory &rest open-options) &body body)
  `(with-output-to-file (,stream (metafile-path ,name ,metastore-directory) :element-type 'character :if-does-not-exist :error
                                 ,@(remove-from-plist open-options :element-type :if-does-not-exist :if-exists))
     ,@body))

(defmacro appending-to-metafile ((stream name metastore-directory &rest open-options) &body body)
  `(with-open-file (,stream (metafile-path ,name ,metastore-directory) :element-type 'character :if-does-not-exist :error :if-exists :append
                            ,@(remove-from-plist open-options :element-type :if-does-not-exist :if-exists))
     ,@body))

(defmacro within-meta ((meta-path &rest options) &body body)
  `(within-directory (,(gensym) ,meta-path ,@options)
     ,@body))

(defun commit-metafile (name metastore-directory &optional (commit-message (format nil "Updated ~A" name)))
  "Commit contents of the metafile called by NAME in METASTORE-DIRECTORY."
  (within-meta (metastore-directory)
    (git "add" (symbol-name name))
    (when (with-valid-exit-codes ((1 nil)) (git "status"))
      (git "commit" "-m" (format nil "~A" commit-message)))))

(defun ensure-metastore (directory &key required-metafiles)
  "Ensure that a metastore exists at DIRECTORY.

   Returns T if the metastore was created, or updated; NIL otherwise."
  (if-let ((meta-missing-p (not (and (directory-exists-p (subdirectory* directory ".git"))
                                     (every (compose #'file-exists-p
                                                     (rcurry #'metafile-path directory))
                                            required-metafiles)))))
    (within-meta (directory :if-does-not-exist :create)
      (git "init")
      (open (subfile* directory ".git" "git-daemon-export-ok") :direction :probe :if-does-not-exist :create)
      (dolist (mf required-metafiles)
        (create-metafile mf directory)
        (commit-metafile mf directory (format nil "Created ~A" mf)))
      t)
    nil))
