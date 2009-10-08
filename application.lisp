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

(defun define-application (name system package function &rest default-parameters)
  (make-instance 'application :name name :system (system system) :package package :function function :default-parameters default-parameters))

(defun run (application-designator &rest parameters)
  (let* ((a (coerce-to-application application-designator))
         (s (app-system a)))
    (unless (system-loadable-p s)
      (lust (name s)))
    (require (name s))
    (apply (symbol-function (find-symbol (string (app-function a)) (app-package a))) (or parameters (app-default-parameters a)))))

(defmethod purge-fasls ((a application))
  (mapc #'purge-fasls (system-module (app-system a))))