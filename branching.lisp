;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
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


(define-reported-condition patch-failure (repository-error)
  ((output :reader condition-output :initarg :output))
  (:report (pathname output)
           "~@<Failed to apply patch in ~S. The output was:~%~A~%~:@>" pathname output))

(defun master-detached-p (&optional (repo-dir *repository*))
  "See if the master branch is driven by the user, that is out of desire's control."
  (not (ref= '("master") '("tracker") repo-dir)))

(defun ensure-tracker-branch (&optional (repo-dir *repository*) ref &aux
                              (ref (or ref (get-head t repo-dir))))
  (unless (git-branch-present-p :tracker repo-dir)
    (git-set-branch :tracker repo-dir ref)))

(defun switch-to-op (&optional (repository *repository*) &aux
                     (op '("desire" "op")))
  (set-head op repository))