;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2009 by
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


(defun make-cl-launch-system (locality-directory)
  (ensure-directories-exist (subdirectory* *desire-root* "bin"))
  (within-directory ((subdirectory* locality-directory "cl-launch"))
    (make "install_binary" `("INSTALL_BIN=" ,(subfile* *desire-root* "bin")))
    (make "install_source" `("INSTALL_SOURCE=" ,locality-directory))))

(defmethod satisfy-module :after ((module-name (eql 'cl-launch)) &optional locality system-type complete skip-present module-vocabulary system-vocabulary)
  (declare (ignore system-type complete skip-present module-vocabulary system-vocabulary))
  (make-cl-launch-system (locality-pathname locality))
  (ensure-module-systems-loadable (module 'cl-launch) locality))