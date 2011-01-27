;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
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


(defparameter *beginning-of-output-marker* :beginning-of-output-marker)
(defparameter *end-of-output-marker* :end-of-output-marker)
(defparameter *beginning-of-result-marker* :beginning-of-result-marker)
(defparameter *end-of-result-marker* :end-of-result-marker)

(defgeneric run-module-test (type module-name &optional verbose-internals record-output)
  (:method :around (type module-name &optional verbose-internals record-output)
    (declare (ignore verbose-internals))
    (pergamum::invoke-with-maybe-prepended-output
     record-output
     (lambda ()
       (multiple-value-bind (condition backtrace return-value)
           (block block
             (flet ((record-condition (c)
                      (return-from block
                        (values c (with-output-to-string (string-stream)
                                    (backtrace most-positive-fixnum string-stream))))))
               (handler-bind ((serious-condition #'record-condition)
                              #+sbcl
                              (sb-c:compiler-error #'record-condition)
                              #+sbcl
                              (sb-c:fatal-compiler-error #'record-condition))
                 (values nil nil
                         (unwind-protect (call-next-method)
                           (finish-output *standard-output*)
                           (finish-output *error-output*))))))
         (list* :return-value return-value (when condition
                                             (list :condition condition :backtrace backtrace))))))
           ;; (with-recorded-status (:record-backtrace t :record-output record-output)
           ;;   (unwind-protect (call-next-method)
           ;;     (finish-output *standard-output*)
           ;;     (finish-output *error-output*)))
           )
  (:method ((o (eql :master-reachability-phase)) mn &optional verbose-internals record-output)
    (declare (ignore verbose-internals record-output))
    (touch-module mn))
  (:method ((o (eql :master-update-phase)) mn &optional verbose-internals record-output)
    (declare (ignore verbose-internals record-output))
    (let ((*fetch-errors-serious* t))
      (update mn)
      t))
  (:method ((o (eql :master-recurse-phase)) mn &optional verbose-internals record-output)
    (declare (ignore record-output))
    (desire (list mn) :skip-present t :skip-missing t :seal nil :verbose verbose-internals)
    t)
  (:method ((o (eql :remote-lisp-fetch-phase)) mn &optional verbose-internals record-output)
    (declare (ignore verbose-internals record-output))
    (let ((*fetch-errors-serious* t)
          (remote (module-best-remote (module mn))))
      (unless (typep remote 'gate) ; Should it check for exact wishmaster identity?
        (module-error (module mn) "~@<Attempted to fetch module ~A from a non-gate remote ~A in remote lisp mode.~:@>"
                      mn (name remote)))
      (update mn)
      t))
  (:method ((o (eql :remote-lisp-load-phase)) mn &optional verbose-internals record-output)
    (declare (ignore record-output))
    (do-modules (m)
      (when (directory-exists-p (module-pathname m))
        (stash-module m nil)))
    (drop-system-caches *default-system-type*)
    (desire (list mn) :skip-present t :seal nil :verbose verbose-internals)
    (let ((m (module mn)))
      (multiple-value-bind (module-deps system-subdeps) (module-dependencies m)
        (let ((system-deps (list* (module-central-system m) system-subdeps))
              (old *readtable*)
              (*readtable* (copy-readtable)))
          (format t "~@<;;; ~@; Mo~@<dule dependencies: ~A~:@>~:@>~%" (mapcar #'name module-deps))
          (format t "~@<;;; ~@; Sy~@<stem dependencies: ~A~:@>~:@>~%" (mapcar #'name system-deps))
          (format t "~@<;;; ~@; The dependencies are ~:[IN~;~]complete~:@>~%"
                  (every #'system-definition-complete-p system-deps))
          (multiple-value-prog1
              (loadsys (module-central-system mn) :verbose verbose-internals)
            (unless (readtable= old *readtable*)
              (module-error m "~@<WARNING: loading of module ~A affects *READTABLE*.~:@>" mn)))
          #+(or) (error 'undeclared-system-dependency :system system :guilty-set system-deps))))
    t)
  (:method ((o (eql :remote-lisp-test-phase)) mn &optional verbose-internals record-output)
    (declare (ignore verbose-internals record-output))
    (not-implemented :remote-lisp-test-phase)))

(defgeneric run-test-phase-with-markers (phase-name module-names verbose)
  (:method :around (phase-name module-names verbose)
    (syncformat t "(:phase ~S :module-count ~D)~%" phase-name (length module-names))
    (call-next-method)
    (syncformat t "(:phase-end ~S)~%" phase-name))
  (:method (phase-name module-names verbose)
    (iter (for mn in module-names)
          (syncformat t "(:name ~S :mode ~(~S~)~%~
                         ~S~%"
                      mn phase-name
                      *beginning-of-result-marker*)
          (destructuring-bind (&key return-value condition backtrace) (run-module-test phase-name mn verbose)
            (syncformat t "~%~:[~*~;~%>>> A condition of type ~A was encountered during execution.~%~]~
                           ~S~%~
                           :status ~S :condition ~S :backtrace ~S)~%"
                        condition (type-of condition)
                        *end-of-result-marker*
                        return-value (format nil "~A" condition) backtrace)))))

(defun run-test-phases-with-markers (phase-names module-names &key verbose &aux
                                     (module-names (mapcar #'canonicalise-name module-names)))
  (dolist (phase-name (mapcar (compose #'make-keyword #'string-upcase #'string) phase-names))
    (run-test-phase-with-markers phase-name module-names verbose)))

(defun invoke-with-output-markers (stream fn)
  (syncformat stream "~%~S~%" *beginning-of-output-marker*)
  (unwind-protect (funcall fn)
    (syncformat stream "~%~S~%" *end-of-output-marker*)))

(defmacro with-output-markers (() &body body)
  `(invoke-with-output-markers t (lambda () ,@body)))
