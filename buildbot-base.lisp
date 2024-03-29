;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE-BUILDBOT; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
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
  (:use :common-lisp :iterate :alexandria :hunchentoot :cl-who :split-sequence
        :elsewhere.0 :pergamum :executor :portable-spawn :desire)
  (:export
   ;; buildmaster
   #:buildbot-condition
   #:buildbot-error
   #:buildmaster-condition
   #:buildmaster-error
   #:remote-lisp-condition
   #:remote-lisp-error
   #:simple-buildmaster-error
   #:remote-lisp-communication-error
   #:simple-remote-lisp-communication-error
   #:remote-lisp-initialisation-error
   #:condition-ctx
   #:ping-remote
   #:one
   #:one*
   #:metaone
   ;; buildmaster web interface
   #:start-cl-waterfall
   #:render-cl-waterfall-static))

(in-package :desire-buildbot)


;;;;
;;;; More or less generic-ish basis
;;;;
(defun relativise-path (new-base path &optional (ascent-marker ".."))
  (let* ((old-base (butlast path))
         (name (lastcar path))
         (mismatch-posn (mismatch new-base old-base))
         (newbaselen (length new-base)))
    (multiple-value-bind (unwind rewind) (cond
                                           ((null mismatch-posn))
                                           ((= mismatch-posn newbaselen)
                                            (values nil (subseq old-base newbaselen)))
                                           (t
                                            (values (make-list (- newbaselen mismatch-posn)
                                                               :initial-element ascent-marker)
                                                    (subseq old-base mismatch-posn))))
      (append unwind rewind (list name)))))

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

(defgeneric period-unstart (period)
  (:method ((o period))
    (slot-makunbound o 'start-time)))

(defgeneric period-length (period)
  (:method ((o period))
    (if (slot-boundp o 'end-time)
        (- (period-end-time o) (period-start-time o))
        (error "~@<Period not finalised.~:@>"))))

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
                   :initform (alist-hash-table ',(mapcar (curry #'apply #'cons) (rest subclass-map))
                                               :test 'eq)))
       ,@(remove :subclass-to-action-class-map class-options :key #'car))
    (error "~@<No subclass-to-action-class-map class option was specified.~:@>")))

(defgeneric specific-action-subclass (action type)
  (:method ((o action) (type symbol))
    (or (gethash type (action-class-map o))
        (error "~@<Root action class of class ~A doesn't specify a ~
                   specific action subclass of type ~A.~:@>"
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

(defmethod update-instance-for-different-class :after ((old action) (new complete-action) &key
                                                       &allow-other-keys)
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

(defvar *verbose-termination* nil)
(define-binder with-verbose-termination *verbose-termination* :fixed-value t)
(define-binder with-maybe-verbose-termination *verbose-termination* :fixed-value t :maybep t)

(defun invoke-with-tracked-termination (action handlep fn)
  (let (normally-executed-p
        termination-done-p)
    (unwind-protect
         (handler-bind ((serious-condition
                         (lambda (c)
                           (when *verbose-termination*
                             (format t "~&~@<;; ~@;Te~@<rminating ~A due to ~A:~%~A~:@>~:@>~%" action (type-of c) c))
                           (terminate-action action
                                             :condition c
                                             :backtrace (with-output-to-string (s)
                                                          (backtrace most-positive-fixnum s)))
                           (setf termination-done-p t)
                           (when handlep
                             (return-from invoke-with-tracked-termination nil)))))
           (multiple-value-prog1 (funcall fn)
             (setf normally-executed-p t)))
      (unless (or normally-executed-p termination-done-p)
        (terminate-action action
                          :backtrace (with-output-to-string (s)
                                       (backtrace most-positive-fixnum s)))))))

(defmacro with-tracked-termination ((action &optional handlep) &body body)
  `(invoke-with-tracked-termination ,action ,handlep (lambda () ,@body)))

(defgeneric invoke-with-active-period (period handlep fn)
  (:method ((o period) handlep (fn function))
    (start-period o)
    (multiple-value-prog1 (with-tracked-termination (o handlep)
                            (funcall fn))
      (end-period o))))

(defmacro with-active-period ((period &optional handlep period-form) &body body)
  (if period-form
      `(let ((,period ,period-form))
         (invoke-with-active-period ,period ,handlep (lambda () ,@body)))
      `(invoke-with-active-period ,period ,handlep (lambda () ,@body))))

;;;
;;; Webbable
;;;
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


;;;;
;;;; Buildbot
;;;;
(define-condition buildbot-condition (desire-condition) ())
(define-condition buildbot-error (buildbot-condition desire-error) ())
(define-condition buildmaster-condition (buildbot-condition) ())
(define-condition buildmaster-error (buildmaster-condition buildbot-error) ())
(define-condition remote-lisp-condition (buildbot-condition)
  ((ctx :reader condition-ctx :initarg :ctx)))
(define-condition remote-lisp-error (remote-lisp-condition buildbot-error) ())
(define-simple-error buildmaster-error)

(define-condition remote-lisp-communication-error (remote-lisp-error) ())
(define-simple-error remote-lisp-communication-error :object-initarg :ctx)

(define-reported-condition remote-lisp-initialisation-error (remote-lisp-communication-error)
  ((output :reader error-output :initarg :output))
  (:report (output)
           "~@<Error encountered while initialising remote-lisp:~%~A~%~:@>" output))

(define-condition build-test-error (desire-error) ())
(define-simple-error build-test-error)

;;;
;;; Result
;;;
(define-action-root-class result (action webbable)
  ((module :accessor result-module :initarg :module)
   (phase :accessor result-phase :initarg :phase)
   (id :accessor result-id :initarg :id)
   (commit :accessor result-commit)
   ;;
   (hint-cache :accessor result-hint-cache :initform nil)
   (hint-cache-final-p :accessor result-hint-cache-final-p :initform nil)
   (path :accessor result-path :initarg :path)
   ;;
   (output :accessor result-output :initarg :output)
   (output-bytes :accessor result-output-bytes :initform 0)
   (output-consumers :accessor result-output-consumers :type list :initform nil))
  (:subclass-to-action-class-map
   (success result-success)
   (failure result-failure)
   (unhandled-failure result-unhandled-failure)))

;;;
;;; Result output vector management
;;;
(defconstant header-leeway 32)
(defconstant page-size (virtual-memory-page-size))
(defconstant initial-output-length (- page-size header-leeway))
(defconstant output-size-scale-factor 4)

(defun prepare-result-vector (phases n-phase-results modules locality)
  (lret* ((n-phases (length phases))
          (results (make-array (* n-phases n-phase-results) :fill-pointer t)))
    (iter (with pathnames = (mapcar (rcurry #'module-pathname locality) modules))
          (for phase in phases)
          (for phase-no below n-phases)
          (for base from 0 by n-phase-results)
          (iter (for i below n-phase-results)
                (for m in modules)
                (for pathname in pathnames)
                (setf (aref results (+ base i))
                      (make-instance 'result-not-yet :phase phase :module m :id (+ base i)
                                     :path pathname
                                     :output (make-array initial-output-length
                                                         :element-type 'character :adjustable t)))))
    (setf (fill-pointer results) 0)))

(defgeneric append-result-output (result string &optional finalp)
  (:method ((o result) string &optional finalp)
    (let ((used-bytes (result-output-bytes o))
          (rvec (result-output o)))
      (if finalp
          (setf (result-output o)
                (if (zerop used-bytes)
                    string
                    (concatenate 'string (subseq rvec 0 used-bytes) string))
                (result-output-bytes o) (length string))
          (let* ((length (length string))
                 (required-length (+ used-bytes length 1)) ; we do newlines manually
                 (rvec-length (array-dimension rvec 0)))
            (when (< rvec-length required-length)
              (let ((fitting-length (ash 1 (+ (integer-length required-length)
                                              ;; Quadruple, while we're going for speed and
                                              ;; double when we've got to resort to compactness.
                                              (if (> (integer-length required-length) 20) 0 1)))))
                (setf rvec (adjust-array rvec fitting-length)
                      (result-output o) rvec)))
            (setf (subseq rvec used-bytes (1- required-length)) string
                  (aref rvec (1- required-length)) #\Newline
                  (result-output-bytes o) required-length))))))

;;;
;;; Test phase
;;;
(defclass test-phase (action) 
  ((run :reader phase-run :initarg :run)
   (current :accessor current-phase-result :initarg :current)
   (action-description :reader phase-action-description :initarg :action-description)
   (nr :reader phase-nr :initarg :nr)
   (base :reader phase-base :initarg :base)
   (n-results :reader phase-n-results :initarg :n-results))
  (:default-initargs
   :current nil))

(defclass interrupted-test-phase (unhandled-failure test-phase) ())

(define-action-root-class local-test-phase (test-phase)
  ()
  (:subclass-to-action-class-map
   (unhandled-failure interrupted-local-test-phase)))
(define-action-root-class remote-test-phase (test-phase)
  ((ctx :accessor remote-phase-ctx))
  (:subclass-to-action-class-map
   (unhandled-failure interrupted-remote-test-phase)))

(defclass interrupted-local-test-phase (interrupted-test-phase local-test-phase) ())
(defclass interrupted-remote-test-phase (interrupted-test-phase remote-test-phase) ())

(defclass master-reachability-phase (local-test-phase) ()
  (:default-initargs :action-description "test upstream repository reachability"))
(defclass master-update-phase (local-test-phase) ()
  (:default-initargs :action-description "fetch upstream modules and convert them"))
(defclass master-recurse-phase (local-test-phase) ()
  (:default-initargs :action-description "unwind module dependencies"))
(defclass remote-lisp-fetch-phase (remote-test-phase) ()
  (:default-initargs :action-description "fetch modules from wishmaster"))
(defclass remote-lisp-load-phase (remote-test-phase) ()
  (:default-initargs :action-description "load modules"))
(defclass remote-lisp-test-phase (remote-test-phase) ()
  (:default-initargs :action-description "test modules"))

(defgeneric describe-phase (test-phase)
  (:method ((o test-phase))
    (format nil "~A on ~:[the wishmaster~;a remote-lisp~]; ~:[not started~; started ~:*~A~]~
                                                          ~:[~;, finished ~:*~A~]"
            (phase-action-description o) (typep o 'remote-test-phase)
            (when (period-started-p o)
              (multiple-value-call #'print-decoded-time
                (decode-universal-time (period-start-time o))))
            (when (period-ended-p o)
              (multiple-value-call #'print-decoded-time
                (decode-universal-time (period-end-time o)))))))

(defgeneric phase-type-dependency (phase)
  (:method ((o (eql 'master-reachability-phase))) nil)
  (:method ((o (eql 'master-update-phase))) 'master-reachability-phase)
  (:method ((o (eql 'master-recurse-phase))) nil)
  (:method ((o (eql 'remote-lisp-fetch-phase))) 'master-recurse-phase)
  (:method ((o (eql 'remote-lisp-load-phase))) 'remote-lisp-fetch-phase)
  (:method ((o (eql 'remote-lisp-test-phase))) 'remote-lisp-load-phase)
  (:method ((o master-reachability-phase)) nil)
  (:method ((o master-update-phase)) 'master-reachability-phase)
  (:method ((o master-recurse-phase)) nil)
  (:method ((o remote-lisp-fetch-phase)) 'master-update-phase)
  (:method ((o remote-lisp-load-phase)) 'remote-lisp-fetch-phase)
  (:method ((o remote-lisp-test-phase)) 'remote-lisp-load-phase))

(defun result-dependency (result)
  (let ((phase (result-phase result)))
    (when-let* ((dependency-type (phase-type-dependency phase))
                (dependency (find-if (of-type dependency-type) (master-run-phases (phase-run phase)))))
      (result (result-id result) (type-of dependency) (phase-run phase)))))

;;;
;;; Buildmaster run
;;;
(define-action-root-class buildmaster-run (action)
  ((lock :reader master-run-lock :initarg :lock)
   (condvar :reader master-run-condvar :initarg :condvar)
   (modules :reader master-run-modules :initarg :modules)
   (phases :reader master-run-phases :initarg :phases)
   (current :accessor current-master-run-phase :initarg :current)
   (n-phases :reader master-run-n-phases :initarg :n-phases)
   (n-phase-results :reader master-run-n-phase-results :initarg :n-phase-results)
   (results :reader master-run-results :initarg :results))
  (:subclass-to-action-class-map
   (unhandled-failure interrupted-buildmaster-run))
  (:default-initargs
   :lock (bordeaux-threads:make-lock)
   :condvar (bordeaux-threads:make-condition-variable)
   :current nil))

(defclass interrupted-buildmaster-run (unhandled-failure buildmaster-run) ())


;;;
;;; Accessing results
;;;
(defvar *buildmaster-runs* nil)

(defun master-run-n-complete-results (master-run)
  "Not meant to be precise: can underreport without lock held."
  (if (period-ended-p master-run)
      (* (master-run-n-phases master-run)
         (master-run-n-phase-results master-run))
      (1- (fill-pointer (master-run-results master-run)))))

(defmacro with-master-run-lock ((master) &body body)
  `(bordeaux-threads:with-lock-held ((master-run-lock ,master))
     ,@body))

(defmethod initialize-instance :after ((o buildmaster-run) &key modules phases locality)
  (let ((n-phase-results (length modules))
        (n-phases (length phases)))
    (setf (slot-value o 'phases) (iter (for i from 0)
                                       (for base from 0 by n-phase-results)
                                       (for phase-type in phases)
                                       (collect (make-instance phase-type :run o :nr i :base base :n-results n-phase-results)))
          (slot-value o 'n-phases) n-phases
          (slot-value o 'results) (prepare-result-vector (slot-value o 'phases) n-phase-results
                                                         modules locality)
          (slot-value o 'n-phase-results) n-phase-results)))

(defun master-result (master-run i)
  (declare (type buildmaster-run master-run))
  (aref (master-run-results master-run) i))

(defun result-nr (result)
  (- (result-id result) (phase-base (result-phase result))))

(defun result (result phase &optional (master-run (first *buildmaster-runs*)))
  (aref (master-run-results master-run)
        (let ((base (etypecase phase
                      (integer (* phase (master-run-n-phase-results master-run)))
                      (symbol (phase-base (find (string phase) (master-run-phases master-run)
                                                :key #'type-of :test #'string=))))))
          (etypecase result
            ((or symbol string)
             (position result (master-run-results master-run)
                       :start base :key (compose #'name #'result-module) :test #'string=))
            (integer
             (+ base (mod result (master-run-n-phase-results master-run))))))))

(defun first-phase-result-p (master-run result)
  (< (result-id result) (master-run-n-phase-results master-run)))

(defun previous-phase-result (master-run result)
  (master-result master-run (- (result-id result) (master-run-n-phase-results master-run))))

(defun previous-phase-result-success-p (master-run result)
  (typep (previous-phase-result master-run result) 'success))


;;;;
;;;; Emission-related globals
;;;;
(defvar *style*)
(defparameter *uri-base* '("desire-waterfall"))
(defparameter *global-condition* nil)
(defparameter *verbose-static-emission* nil)
(defvar *emitting-static* nil)
(defvar *current-base* nil)
(defvar *completed-static-emissions* (make-hash-table :test #'equal))
(defvar *static-output-base-pathname* nil)
(defvar *master-run-nr*)

(define-root-container *completed-static-emissions* completed-static-emission :key-type list :type boolean :if-does-not-exist :continue)

;;;
;;; Automagic staticity machinery: in static mode %URI calls RENDER-CL-WATERFALL,
;;; which implies: watch out for graph loops!
;;;
(defun invoke-maybe-static/content-type (base-pathname path-fn content-type fn)
  (if *emitting-static*
      (let* ((subpath (funcall path-fn))
             (subdirectory (butlast subpath))
             (filename (lastcar subpath))
             (filetype (ecase content-type
                         (:text/plain "txt")
                         ((nil) "html")))
             (output-pathname (subfile base-pathname subpath :type filetype)))
        (when *verbose-static-emission*
          (format t "~@<; ~@;Static: base ~S, sub ~S, name ~A.~A   Output: ~S.~:@>~%"
                  base-pathname subdirectory filename filetype
                  output-pathname))
        (ensure-directories-exist output-pathname)
        (with-output-to-file (s output-pathname)
          (funcall fn s)))
      (progn
        (when content-type
          (setf (content-type*) (ecase content-type
                                  (:text/plain "text/plain"))))
        (funcall fn (flexi-streams:make-flexi-stream (send-headers))))))

(defmacro with-maybe-static/content-type ((stream content-type base-pathname path-fn) &body body)
  `(invoke-maybe-static/content-type ,base-pathname ,path-fn ,content-type
                                     (lambda (,stream)
                                       ,@body)))

(defun %uri (path extension text base)
  (flet ((interpret-path-element (x)
           (etypecase x
             (integer (write-to-string x))
             (string x)
             (symbol (down-case-string x)))))
    (let ((processed-path (mapcar #'interpret-path-element path)))
      (lret ((uri (format nil "<a href='~A~:[~;.~:*~A~]'>~A</a>"
                          (flatten-path-list (if *emitting-static*
                                                 (relativise-path *current-base* processed-path)
                                                 (append base processed-path))
                                             (not *emitting-static*))
                          extension text)))
        (when (and *emitting-static*
                   (not (completed-static-emission path)))
          (when *verbose-static-emission*
            (format t "~@<; ~@;Recurring with ~S~:@>~%" path))
          (render-cl-waterfall path))))))

(defun plain-uri (path text &optional (extensionp *emitting-static*) (base *uri-base*))
  (%uri path (when extensionp "txt") text base))

(defun uri (path text &optional (extensionp *emitting-static*) (base *uri-base*))
  (%uri path (when extensionp "html") text base))
