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

(defun create-metafile (name metastore-directory &key (if-exists :append))
  "Ensure that metafile called by NAME exists within METASTORE-DIRECTORY.
  
   The IF-EXISTS keyword is passed to OPEN."
  (open (metafile-path name metastore-directory) :direction :probe :if-does-not-exist :create :if-exists if-exists))

(defun ensure-metastore (directory &key required-metafiles)
  "Ensure that a metastore exists at DIRECTORY.

   Returns T if the metastore was created, or updated; NIL otherwise."
  (if-let ((meta-missing-p (not (and (directory-exists-p (subdirectory* directory ".git"))
                                     (every (compose #'file-exists-p
                                                     (rcurry #'metafile-path directory))
                                            required-metafiles)))))
    (within-directory (meta-path directory :if-does-not-exist :create)
      (git "init")
      (open (subfile* directory ".git" "git-daemon-export-ok") :direction :probe :if-does-not-exist :create)
      (mapcar (rcurry #'create-metafile directory) required-metafiles)
      t)
    nil))

(defmacro with-open-metafile ((stream name metastore-directory &rest open-options) &body body)
  `(with-open-file (,stream (metafile-path ,name ,metastore-directory) ,@open-options)
     ,@body))

(defmacro with-output-to-new-metafile ((stream name metastore-directory &rest open-options) &body body)
  `(with-open-file (,stream (metafile-path ,name ,metastore-directory) :if-does-not-exist :error :if-exists :supersede ,@open-options)
     ,@body))

(defmacro appending-to-metafile ((stream name metastore-directory &rest open-options) &body body)
  `(with-open-file (,stream (metafile-path ,name ,metastore-directory) :if-does-not-exist :error :if-exists :append ,@open-options)
     ,@body))

(defmacro within-meta ((meta-path) &body body)
  `(within-directory (,meta-path (meta-path))
     ,@body))
