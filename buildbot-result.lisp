;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE-BUILDBOT; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :desire-buildbot)


(defclass webbable ()
  ((description :reader web-description :initarg :description)
   (class :reader web-class :initarg :class)
   (marker :reader web-marker :initarg :marker)))

(defmacro define-webbable-class (name superclasses slots &rest class-options)
  (if-let ((web-initargs (assoc :web-initargs class-options)))
    (let ((web-initargs (rest web-initargs)))
      `(defclass ,name (,@superclasses)
         ((description :allocation :class :initform ,(getf web-initargs :description))
          (class :allocation :class :initform ,(getf web-initargs :class))
          (marker :allocation :class :initform ,(getf web-initargs :marker))
          ,@slots)
         ,@(remove :web-initargs class-options :key #'car)))
    (error "~@<No web-initargs class option was specified.~:@>")))

(define-action-root-class result (action webbable)
  ((module :accessor result-module :initarg :module)
   (path :accessor result-path :initarg :path)
   (commit-id :accessor result-commit-id :initarg :commit-id)
   (id :accessor result-id :initarg :id)
   (output :accessor result-output :initarg :output)
   (output-bytes :accessor result-output-bytes :initform 0)
   (output-consumers :accessor result-output-consumers :type list :initform nil))
  (:subclass-to-action-class-map
   (success result-success)
   (failure result-failure)
   (unhandled-failure result-unhandled-failure)))

(define-webbable-class result-not-yet (result incomplete-action) ()
  (:web-initargs
   :class "nevr" :marker "n" :description "the test was never run"))
(define-webbable-class result-success (result success) ()
  (:web-initargs
   :class "succ" :marker "o" :description "the test completed successfully"))
(define-webbable-class result-failure (result failure) ()
  (:web-initargs
   :class "fail" :marker "f" :description "the test failed" ))
(define-webbable-class result-unhandled-failure (result unhandled-failure) ()
  (:web-initargs
   :class "excp" :marker "x" :description "an exception occured while running test"))

(progn
  (sb-mop:finalize-inheritance (find-class 'result-not-yet))
  (sb-mop:finalize-inheritance (find-class 'result-success))
  (sb-mop:finalize-inheritance (find-class 'result-failure))
  (sb-mop:finalize-inheritance (find-class 'result-unhandled-failure)))

(defparameter *popup-display-nlines-threshold* 15)

(defgeneric emit-with-result-emission (stream result fn)
  (:method ((stream stream) (o result) (fn function))
    (with-html-output (stream)
      (:div :class "result cell"
            (funcall fn)
            (let ((condition (action-condition o)))
              (htm
               (:div :class "nhint"
                     (let* ((*print-escape* nil)
                            (*print-length* nil)
                            (*print-circle* t)
                            (*print-level* nil)
                            (condition-printed-form (when condition (escape-string (princ-to-string condition)))))
                       (fmt "~A~:[~;<br><br>encountered condition:<br><pre>~A</pre>~]" (string-downcase (symbol-name (name (result-module o))))
                            (and condition (> *popup-display-nlines-threshold* (count #\Newline condition-printed-form))) condition-printed-form)))
               (:div :class (web-class o)
                     (str (web-marker o))
                     :br
                     (fmt "<a href='/desire-waterfall?mode=output&result-id=~D'>out</a>" (result-id o))
                     (when condition
                       (htm 
                        :br
                        (fmt "<a href='/desire-waterfall?mode=cond&result-id=~D'>co</a>" (result-id o)))))))))))

(defmacro with-result-emission ((stream result) &body body)
  `(emit-with-result-emission ,stream ,result (lambda () ,@body)))

(defun print-legend (stream)
  (with-html-output (stream)
    :br
    :br
    (:span
     (:div :class "legend" "Legend:" :br
           (iter (for class-name in '(result-not-yet result-success result-failure result-unhandled-failure))
                 (for p = (class-prototype class-name))
                 (htm (:span :class "legend-entry" :style "clear: both; background: green;"
                             (:span :style "clear:both; float:left;"
                                    (:div :class (conc "result cell " (web-class p))
                                          (str (web-marker p)))
                                    " - " (str (web-description p))))))))))