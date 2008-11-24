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

(defparameter *sbcl-systems-location* '(".sbcl" "systems"))

(defun system-definition-path (o)
  (declare (type system o))
  (make-pathname :directory (append (pathname-directory (path (system-repository o))) (system-relativity o)) :name (downstring (name o)) :type "asd"))

#-win32
(defun system-sbcl-symlink-path (o)
  (declare (type system o))
  (make-pathname :directory (append (pathname-directory (user-homedir-pathname)) *sbcl-systems-location*) :name (downstring (name o)) :type "asd"))

(defgeneric loadable-p (o)
  (:method ((o module))
    (if-let ((system (module-core-system o)))
            (loadable-p system)
            t))
  (:method ((o system))
    (etypecase (system-repository o)
      (user-repository (not (null (probe-file (system-definition-path o)))))
      #-win32
      (site-repository (and (probe-file (system-sbcl-symlink-path o))
                            (probe-file (sb-posix:readlink (system-sbcl-symlink-path o))))))))

(defgeneric ensure-loadable (o)
  (:method ((o module))
    (every #'ensure-loadable (module-systems o)))
  (:method ((o system))
    (etypecase (system-repository o)
      (user-repository t)
      #-win32
      (site-repository
       (unless (loadable-p o)
         (let ((symlink (system-sbcl-symlink-path o)))
           (when (probe-file symlink)
             (sb-posix:unlink symlink)) ;; FIXME: delete-file refuses to remove dead symlinks
           (sb-posix:symlink (system-definition-path o) symlink)))))))

(defun load-system (os)
  (declare (type (or symbol module system) os))
  (asdf:oos 'asdf:load-op (name (typecase os
                                  (symbol (system os))
                                  (module (module-core-system os))
                                  (system os)))))

(defgeneric purge-fasls (o)
  (:method ((o repository))
    (mapc #'delete-file (directory (make-pathname :directory (append (pathname-directory (path o)) (list :wild-inferiors)) :name :wild :type "fasl"))))
  (:method ((o system))
    (purge-fasls (system-repository o)))
  (:method ((o module))
    (purge-fasls (module-master-repository o)))
  (:method (o)
    (etypecase o
      (symbol (mapc (compose #'purge-fasls #'module-core-system) (module-full-dependencies (module o)))))))

(defun desire-find-system (name)
  (when (boundp '*perspective*)
    (when-let ((system (system name :if-does-not-exist :continue)))
      (probe-file (system-definition-path system)))))

(push 'desire-find-system asdf:*system-definition-search-functions*)
