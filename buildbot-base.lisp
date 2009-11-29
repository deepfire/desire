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

(defpackage desire-buildbot
  (:nicknames :desree)
  (:use :common-lisp :iterate :alexandria :pergamum :executor :portable-spawn :desire :hunchentoot :cl-who :elsewhere.0)
  (:export
   ;; buildslave entry point
   #:buildslave
   ;; buildmaster
   #:buildmaster-error #:simple-buildmaster-error #:simple-build-test-error
   #:ping-slave #:one #:one*
   ;; buildmaster web interface
   #:start-cl-waterfall))

(in-package :desire-buildbot)


;;;
;;; Buildbot conditions
;;;
(define-condition buildmaster-error (desire-error) ())
(define-simple-error buildmaster-error)

(define-reported-condition buildslave-error (buildmaster-error)
  ((output :reader error-output :initarg :output))
  (:report (output)
           "~@<Error encountered while initialising buildslave:~%~A~%~:@>" output))

(define-condition build-test-error (desire-error) ())
(define-simple-error build-test-error)

;;;
;;; Period
;;;
(defclass period ()
  ((start-time :accessor period-start-time :type (integer 0) :initarg :start-time)
   (end-time :accessor period-end-time :type (integer 0) :initarg :end-time)))

(defgeneric period-started-p (period)
  (:method ((o period))
    (slot-boundp o 'start-time)))

(defgeneric period-ended-p (period)
  (:method ((o period))
    (slot-boundp o 'end-time)))

(defgeneric start-period (period)
  (:method ((o period))
    (setf (period-start-time o) (get-universal-time))))

(defgeneric end-period (period)
  (:method ((o period))
    (setf (period-end-time o) (get-universal-time))))

(defgeneric period-length (period)
  (:method ((o period))
    (if (slot-boundp o 'end-time)
        (- (period-end-time o) (period-start-time o))
        (buildmaster-error "~@<Period not finalised.~:@>"))))

;;;
;;; Action
;;;
(define-protocol-class action (period)
  ((condition :reader action-condition :initarg :condition)
   (backtrace :reader action-backtrace :initarg :backtrace)
   (class-map :reader action-class-map :allocation :class :initarg :class-map))
  (:default-initargs
   :condition nil
   :backtrace nil))

(defmacro define-action-root-class (name superclasses (&rest slots) &rest class-options)
  (if-let ((subclass-map (assoc :subclass-to-action-class-map class-options)))
    `(defclass ,name ,superclasses
       (,@slots
        (class-map :allocation :class
                   :initform (alist-hash-table ',(mapcar (curry #'apply #'cons) (rest subclass-map)) :test 'eq)))
       ,@(remove :subclass-to-action-class-map class-options :key #'car))
    (error "~@<No subclass-to-action-class-map class option was specified.~:@>")))

(defgeneric specific-action-subclass (action type)
  (:method ((o action) (type symbol))
    (or (gethash type (action-class-map o))
        (error "~@<Root action class of class ~A doesn't specify a specific action subclass of type ~A.~:@>"
               (class-name (class-of o)) type))))

(defclass incomplete-action (action) ())
(defclass complete-action (action) ())
(defclass success (complete-action) ())
(defclass failure (complete-action)
  ((condition :reader failure-condition :initarg :condition)))
(defclass unhandled-failure (failure) ())

(defun processingp (action)
  (declare (type action action))
  (not (typep action 'complete-action)))

(defun successp (complete-action)
  (declare (type complete-action complete-action))
  (typep complete-action 'success))

(defun terminatedp (complete-action)
  (declare (type complete-action complete-action))
  (typep complete-action 'unhandled-failure))

(defmethod update-instance-for-different-class :after ((old action) (new complete-action) &key &allow-other-keys)
  (end-period new))

(defgeneric complete-action (action class initargs)
  (:method ((o action) (class symbol) initargs)
    (apply #'change-class o class initargs)))

(defgeneric terminate-action (action &rest initargs)
  (:method ((o action) &rest initargs)
    (declare (ignore initargs)))
  (:method :around ((o action) &rest initargs)
    (call-next-method)
    (complete-action o (specific-action-subclass o 'unhandled-failure) initargs)))

(defgeneric fail-action (action &rest initargs)
  (:method ((o incomplete-action) &rest initargs)
    (complete-action o (specific-action-subclass o 'failure) initargs)))

(defgeneric succeed-action (action &rest initargs)
  (:method ((o incomplete-action) &rest initargs)
    (complete-action o (specific-action-subclass o 'success) initargs)))

(defun invoke-with-tracked-termination (action fn)
  (let (normally-executed-p
        termination-done-p)
    (unwind-protect
         (handler-case (multiple-value-prog1 (funcall fn)
                         (setf normally-executed-p t))
           (serious-condition (c)
             (terminate-action action :condition c)
             (setf termination-done-p t)
             (error c)))
      (unless (or normally-executed-p termination-done-p)
        (terminate-action action)))))

(defmacro with-tracked-termination ((action) &body body)
  `(invoke-with-tracked-termination ,action (lambda () ,@body)))
