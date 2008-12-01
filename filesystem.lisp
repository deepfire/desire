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

(defun module-present-p (module &optional (locality (master 'git)))
  "See, whether MODULE is present in git LOCALITY."
  (directory-exists-p (subdirectory* (module-path module locality) ".git")))
