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

(defun create-metafile (name metastore-directory &key (if-exists :continue))
  "Ensure that metafile called by NAME exists within METASTORE-DIRECTORY.
  
   The IF-EXISTS keyword is passed to OPEN."
  (open (subfile* metastore-directory ".git" (symbol-name name)) :direction :probe :if-does-not-exist :create :if-exists if-exists))

(defun ensure-metastore (directory &key required-metafiles)
  "Ensure that a metastore exists at DIRECTORY."
  (when-let* ((meta-missing-p (not (and (directory-exists-p (subdirectory* directory ".git"))
                                        (every (compose #'file-exists-p
                                                        (rcurry #'subfile* (namestring directory))
                                                        #'symbol-name)
                                               required-metafiles)))))
    (within-directory (meta-path directory :if-does-not-exist :create)
      (git "init")
      (open (subfile* directory ".git" "git-daemon-export-ok") :direction :probe :if-does-not-exist :create)
      (mapcar #'create-metafile required-metafiles))))

(defmacro with-open-metafile ((stream name metastore-directory &rest open-options) &body body)
  `(with-open-file (,stream (subfile* ,metastore-directory (symbol-name ,name)) ,@open-options)
     ,@body))

(defmacro with-output-to-metafile ((stream name metastore-directory &rest open-options) &body body)
  `(with-open-file (,stream (subfile* ,metastore-directory (symbol-name ,name)) :if-does-not-exist :error ,@open-options)
     ,@body))

(defmacro within-meta ((meta-path) &body body)
  `(within-directory (,meta-path (meta-path))
     ,@body))
