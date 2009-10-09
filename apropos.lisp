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


(defun set-designator-to-hash-table (set-designator)
  (if (typep set-designator 'hash-table)
      set-designator
      (ecase set-designator
        (:distributor *distributors*)
        (:remote *remotes*)
        (:module *modules*)
        (:system *systems*)
        (:application *apps*))))

(defun name-lessp (x y)
  (string-lessp (string (name x)) (string (name y))))

(defun apropos-desr-list (string-designator &optional set-designator)
  "Like APROPOS-DESR, except that it returns a list of desirable objects found."
  (declare (type (or symbol string) string-designator)
           (type (or null hash-table (member :distributor :remote :module :system :application)) set-designator))
  (if set-designator
      (let ((table (set-designator-to-hash-table set-designator))
            (string (string string-designator))
            (result nil))
        (maphash-values (lambda (o)
                          (when (search string (symbol-name (name o)) :test #'char-equal)
                            (push o result)))
                        table)
        (sort result #'name-lessp))
      (mapcan (lambda (table)
                (apropos-desr string-designator table))
              (list *distributors* *remotes* *modules* *systems* *apps*))))

(defgeneric briefly-describe (desirable &optional stream)
  (:method ((o distributor) &optional (stream *standard-output*))
    (format stream "~A, distributor" (name o)))
  (:method ((o remote) &optional (stream *standard-output*))
    (format stream "~A, remote, url: ~A" (name o) (url o :<module-name>)))
  (:method ((o module) &optional (stream *standard-output*))
    (format stream "~A, module, url: ~A" (name o) (url (module-remote o) o)))
  (:method ((o system) &optional (stream *standard-output*))
    (format stream "~A, system, loadable: ~:[no~;yes~]" (name o) (system-loadable-p o)))
  (:method ((o application) &optional (stream *standard-output*))
    (format stream "~A, application, entry point: ~A, default parameters: ~A" (name o) (app-function o) (app-default-parameters o))))

(defun apropos-desr (string-designator &optional set-designator)
  (dolist (o (apropos-desr-list string-designator set-designator))
    (briefly-describe o)
    (terpri))
  (values))

(defun list-modules ()
  (mapc #'briefly-describe (sort (hash-table-values *modules*) #'name-lessp))
  (values))