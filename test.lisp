;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE-TESTS; Base: 10; indent-tabs-mode: nil -*-
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

(defpackage desire-tests
  (:use :common-lisp :iterate :alexandria :desire :pergamum :executor)
  (:export
   #:*definitions-path*
   #:quicktest))

(in-package :desire-tests)

(defvar *test-host* "betelheise")
(defvar *test-user-account* "empty")
(defvar *bootstrap-script-location* "http://www.feelingofgreen.ru/shared/git/desire/climb.sh")

(defvar *clean-command* "rm -rf climb.sh desr")
(defvar *clean-module-command* "rm -rf desr/git/~(~A~)")
(defvar *download-bootstrapper-command* (format nil "wget ~A -O climb.sh" *bootstrap-script-location*))
(defvar *bootstrap-command*         "sh climb.sh ~A                           ~~/desr")
(defvar *alexandria-update-command* "sh climb.sh ~A -x '(update :alexandria)' ~~/desr")
(defvar *climacs-run-command*       "sh climb.sh ~A -a climacs                ~~/desr")
(defvar *xcvb-bootstrap-command*    "sh climb.sh ~A -s xcvb                   ~~/desr")

(defun quicktest (&key explain verbose clean list disable-debugger (mode :u) (branch "devo") update-climb)
  (find-executable 'ssh)
  (watch-remote-commands *test-host* *test-user-account*
                         (append (when clean
                                   `(,*clean-command*))
                                 (when (or clean update-climb)
                                   `(,*download-bootstrapper-command*))
                                 (when (eq mode :u)
                                   `(,(format nil *clean-module-command* :alexandria)))
                                 (when list
                                   '("ls -la ~/desr/git"))
                                 `(,(format nil (ecase mode
                                                  (:bootstrap *bootstrap-command*)
                                                  (:u *alexandria-update-command*)
                                                  (:climacs *climacs-run-command*)
                                                  (:xcvb *xcvb-bootstrap-command*))
                                            (concatenate 'string
                                                         (when disable-debugger "-g ")
                                                         (when branch (format nil "-b ~(~A~) " branch))
                                                         (when explain "-e ")
                                                         (when verbose "-v "))))))
  (values))
