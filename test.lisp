;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE-TESTS; Base: 10; indent-tabs-mode: nil -*-
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

(defpackage desire-tests
  (:use :common-lisp :desire :pergamum)
  (:export
   #:*definitions-path*
   #:run-tests))

(in-package :desire-tests)

(defparameter *definitions-path* (merge-pathnames (make-pathname :name "definitions") #.*compile-file-pathname*))

(defun run-tests (&optional (test-root #p"/tmp/desire-tests"))
  (with-empty-hash-containers (desire::*distributors*
                               desire::*remotes*
                               desire::*localities*
                               desire::*localities-by-path*
                               desire::*modules*
                               desire::*leaves*
                               desire::*nonleaves*
                               desire::*systems*
                               desire::*apps*
                               desire::*masters*)
    (read-definitions *definitions-path*)
    (define-master-localities
        (subdirectory* test-root "git")
        (subdirectory* test-root "hg")
        (subdirectory* test-root "darcs")
        (subdirectory* test-root "cvs")
        (subdirectory* test-root "svn"))))