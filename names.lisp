;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
;;;
;;;  (c) copyright 2011 by
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

(defun canonicalise-name (name &optional preserve-case (package (load-time-value (find-package :desire))))
  "Given an object's NAME, whether in form of a string, keyword or a symbol
in any other package, return the canonical name, as a symbol in the
'DESIRE' package."
  (intern (xform (not preserve-case) #'string-upcase
                 (etypecase name
                   (symbol (symbol-name name))
                   (string name)))
          package))

(defun choose-local-name (root-pathname)
  "Choose a name for *SELF*.  Disambiguate, if we're doing a local
bootstrap."
  (let ((host-based-name (canonicalise-name (machine-instance))))
    (cond ((and *bootstrap-wishmaster*
                (eq (name *bootstrap-wishmaster*) host-based-name))
           (strconc* (string host-based-name) "-"
                     (normalise-name (namestring root-pathname))))
          (t
           host-based-name))))