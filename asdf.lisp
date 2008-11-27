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

(defun system-definition-path (system &optional (locality (master 'git)))
  (let* ((system (coerce-to-system system))
         (name (down-case-name system)))
    (values
     (subfile (locality-path locality) (append (list (down-case-name (system-module system))) (system-relativity system) (list name)) :type "asd")
     (subfile (git-locality-asdf-registry-path locality) (list name) :type "asd"))))

(defun symlink-dead-p (symlink)
  (if-let ((destination (file-exists-p symlink)))
    (pathname-match-p symlink destination)
    t))

(define-condition module-systems-unloadable-error (desire-error)
  ((module :accessor module-system-unloadable-error-module :initarg :module)
   (systems :accessor module-system-unloadable-error-systems :initarg :system))
  (:report (lambda (stream cond)
             (format stream "~@<Following ~S's systems couldn't be made loadable:~{ ~S~}~:@>"
                     (module-system-unloadable-error-module cond)
                     (module-system-unloadable-error-systems cond)))))

(defun system-loadable-p (system &optional (locality (master 'git)) &aux (name (coerce-to-name system)))
  "See whether SYSTEM is loadable by the means of ASDF."
  (and (not (symlink-dead-p (nth-value 1 (system-definition-path system locality))))
       (asdf:find-system name nil)))

(defun ensure-system-loadable (system &optional (locality (master 'git)))
  (unless (system-loadable-p system)
    (multiple-value-bind (system-asd system-asd-symlink) (system-definition-path system locality)
      (when (probe-file system-asd-symlink)
        (delete-file system-asd-symlink))
      (ensure-directories-exist system-asd-symlink)
      (sb-posix:symlink system-asd system-asd-symlink))))

(defun module-systems-loadable-p (module  &optional (locality (master 'git)))
  (every (rcurry #'system-loadable-p locality) (module-systems (coerce-to-module module))))

(defun ensure-module-systems-loadable (module &optional (locality (master 'git)) &aux (module (coerce-to-module module)))
  (mapc (rcurry #'ensure-system-loadable locality) (module-systems module))
  (unless (module-systems-loadable-p module)
    (error 'module-system-unloadable-error module (remove-if #'system-loadable-p (module-systems module)))))
