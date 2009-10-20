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


(defun make-xcvb-using-asdf (locality-directory)
  (within-directory ((subdirectory* locality-directory "xcvb"))
    (cp "doc/configure.mk.example" "configure.mk")
    (make "xcvb-using-asdf" `("PREFIX=" ,*desire-root*)
                            `("INSTALL_IMAGE=" ,(subdirectory* *desire-root* "images"))
                            `("INSTALL_LISP=" ,locality-directory)
                            `("LISP_SOURCE=" ,locality-directory)
                            `("LISP_SYSTEMS=" ,(subdirectory* locality-directory ".asdf-registry")))))

(defmethod module-post-install ((o module) (name (eql 'xcvb)) (l locality))
  (make-xcvb-using-asdf (locality-pathname l)))