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


(defvar *follow-upstream* t
  "Whether tracking upstream should update HEAD.
Defaults to T.")

(defvar *silently-reset-dirty-repositories* nil
  "Whenever a dirty repository comes up in a situation which requires
a clean one to proceed, quietly reset, or otherwise cleanup the repository,
without raising any signals.
Defaults to NIL.")

(defun master-detached-p (&optional repo-dir)
  "See if the master branch is driven by the user, that is out of desire's control."
  (not (ref= '("master") '("tracker") repo-dir)))

(defun ensure-tracker-branch (&optional repo-dir (ref (get-head repo-dir)))
  (unless (git-branch-present-p :tracker repo-dir)
    (git-set-branch :tracker repo-dir ref)))
