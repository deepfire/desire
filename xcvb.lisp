;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2009 by
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


(define-executable patch)

(defvar *xcvbifier-base-uri* "http://common-lisp.net/project/xcvb/releases/patches/")

(defun invoke-with-file-from-www (filename url fn)
  (unwind-protect (progn (wget url "-O" filename)
                         (funcall fn))
    (delete-file filename)))

(defmacro with-file-from-www ((filename url) &body body)
  `(invoke-with-file-from-www ,filename ,url (lambda () ,@body)))

(defun apply-diff (filename &optional directory &key (if-fails :error))
  (maybe-within-directory directory
    (cond ((with-valid-exit-codes ((1 nil)
                                   (2 nil))
             (with-explanation ("applying diff ~S in ~S" filename *default-pathname-defaults*)
               (patch "-p1" "-i" filename))))
          (t
           (git-repository-reset-hard)
           (ecase if-fails
             (:error (error "~@<Failed to apply ~S in ~S.~:@>" filename *default-pathname-defaults*))
             (:continue nil))))))

(defun xcvbify-module (module)
  (update-module module)
  (ensure-gitbranch )
  (within-directory ((module-pathname module))
    (with-file-from-www (".xcvbifier.diff" `(,*xcvbifier-base-uri* ,(down-case-name module) ".diff"))
      )))