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
  "Whether HEAD should be tracking upstream.
Defaults to T.")

(defvar *dirty-repository-behaviour* :reset
  "Whenever a dirty repository comes up in a situation which requires
a clean one to proceed, do accordingly to the value of this variable:
  :RESET  -  reset the dirty repository, losing unsaved changes,
  :STASH  -  reset the dirty repository, stashing unsaved changes,
  :ERROR  -  raise an error.
Defaults to :RESET")

(defvar *drive-git-masters* t
  "Whether desire should be in control of the 'master' branches in modules
natively git, essentially equating it to the 'tracker' branch.
Defaults to T.")

(defvar *drive-git-masters-matching-trackers* t
  "Whether desire should be in control of the 'master' branches in modules
natively git, when they match 'tracker' branches before module update, 
essentially equating it to the 'tracker' branch.
Defaults to T.")

(define-reported-condition patch-failure (repository-error)
  ((output :initarg :output))
  (:report (pathname output)
           "~@<Failed to apply patch in ~S. The output was:~%~A~%~:@>" pathname output))

(defun master-detached-p (&optional repo-dir)
  "See if the master branch is driven by the user, that is out of desire's control."
  (not (ref= '("master") '("tracker") repo-dir)))

(defun ensure-tracker-branch (&optional repo-dir (ref (get-head repo-dir)))
  (unless (git-branch-present-p :tracker repo-dir)
    (git-set-branch :tracker repo-dir ref)))
