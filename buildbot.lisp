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
  (:use :common-lisp :iterate :alexandria :pergamum :executor :portable-spawn :desire :hunchentoot)
  (:export
   ;; buildslave entry point
   #:buildslave
   ;; buildmaster
   #:run-build-tests))

(in-package :desire-buildbot)

(define-condition build-test-error (desire-error) ())
(define-simple-error build-test-error)

(defvar *default-buildslave-host* "betelheise")
(defvar *default-buildslave-username* "empty")
(defvar *default-buildslave-master* :git.feelingofgreen.ru)

(defparameter *bootstrap-script-location* "http://www.feelingofgreen.ru/shared/git/desire/climb.sh")
(defparameter *purge-command* "rm -rf desr")
(defparameter *purge-metastore-command* "rm -rf desr/git/.meta")
(defparameter *update-bootstrapper-command* (format nil "wget ~A -O climb.sh" *bootstrap-script-location*))

;;;
;;; Common for buildmaster and buildslave
;;;
(defun invoke-with-status-recording (fn)
  (with-output-to-string (output)
    (multiple-value-bind (condition successp)
        (with-collected-conditions (error)
          (let ((*standard-output* output)
                (*error-output* output))
            (funcall fn)))
      (finish-output output)
      (return-from invoke-with-status-recording
        (list* :status successp :output (string-right-trim '(#\Newline) (get-output-stream-string output))
               (when condition `(:condition ,(format nil "\"~A\"" condition))))))))

(defmacro with-recorded-status (() &body body)
  `(invoke-with-status-recording (lambda () ,@body)))

(defun module-test-fetchability (m)
  (let ((*fetch-errors-serious* t))
    (with-recorded-status ()
      (update m)
      t)))

(defun module-test-loadability (m)
  (with-recorded-status ()
    (not (null (asdf:oos 'asdf:load-op (name m))))))

(defvar *remote-output-marker* :beginning-of-test-results-marker)

(defun buildslave ()
  (let ((converted-modules (mapcar #'module (desire::distributor-converted-modules (distributor *default-buildslave-master*)))))
    (syncformat t "~&~S~%" *remote-output-marker*)
    (iter (for m in converted-modules)
          (syncformat t "(:name ~S :mode :fetch~%" (name m))
          (syncformat t " ~{ ~S~})~%" (module-test-fetchability m)))
    (iter (for m in converted-modules)
          (syncformat t "(:name ~S :mode :load~%" (name m))
          (syncformat t " ~{ ~S~})~%" (module-test-loadability m)))
    #+(or)
    (iter (for m in converted-modules)
          (syncformat t "(:name ~S :mode :test" (name m))
          (when (module-has-tests-p m)
            (syncformat t "~{ ~S~})~%" (module-run-tests m))))))

;;;
;;; Buildmaster only below
;;;
(defun cook-buildslave-command (purge purge-metastore branch metastore-branch disable-debugger verbose)
  (remove nil (list
               (when purge
                 *purge-command*)
               (when purge-metastore
                 *purge-metastore-command*)
               *update-bootstrapper-command*
               (format nil "bash climb.sh ~:[~;-v ~]~
                                          ~:[~;-b ~:*~(~A~) ~] ~:[~;-t ~:*~(~A~) ~]~
                                          ~:[~;-g ~]~
                                          -x '(progn (desr:ensure-module-systems-loadable :desire) ~
                                                     (require :desire-buildbot) ~
                                                     (funcall (find-symbol \"TEST-CONVERTED-MODULES\" :desire-buildbot)) ~
                                                     (sb-ext:quit))' ~
                                          ~~/desr"
                       verbose
                       branch metastore-branch
                       disable-debugger))))

(defun run-build-tests (&key (hostname *default-buildslave-host*) (username *default-buildslave-username*)
                        purge purge-metastore branch metastore-branch disable-debugger verbose)
  "A light one-shot, manual-drive version of the buildmaster."
  (find-executable 'ssh)
  (let ((gate (gate *self*)))
    (iter (for m in (append (location-module-names gate) (gate-converted-module-names gate)))
          (destructuring-bind (&key name mode status output condition) (list* :name m :mode :convert (module-test-fetchability m))
            (format t "module ~A, ~A ~A~:[~; encountered condition: ~:*~A, output was:~%~A~]~%" name mode status condition output)))
    (multiple-value-bind (status output condition)
        (watch-remote-commands hostname username (cook-buildslave-command purge purge-metastore branch metastore-branch disable-debugger verbose))
      (declare (ignore condition))
      (if-let ((marker-posn (search (prin1-to-string *remote-output-marker*) output)))
        (with-input-from-string (s output :start (+ marker-posn 2 (length (symbol-name *remote-output-marker*))))
          (iter (for form = (read s nil nil))
                (while form)
                (destructuring-bind (&key name mode status output condition) form
                  (declare (ignore output))
                  (format t "module ~A, ~A ~A~:[~; encountered condition: ~:*~A~]~%" name mode status condition))))
        (build-test-error "~<@Marker ~S wasn't found in remote output.~:@>" *remote-output-marker*))
      status)))

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
        (error "~@<Period not finalised.~:@>"))))

(defconstant header-leeway 32)
(defconstant page-size (desr::virtual-memory-page-size))
(defconstant initial-output-length (- page-size header-leeway))
(defconstant output-size-scale-factor 4)

(defclass result (period)
  ((module :accessor result-module :initarg :module)
   (path :accessor result-path :initarg :path)
   (commit-id :accessor result-commit-id :initarg :commit-id)
   (output :accessor result-output :initarg :output)
   (output-consumer :accessor result-output-consumer :initarg :output-consumer)
   (success-p :accessor result-success-p :initarg :success-p)
   (condition :accessor result-condition :initarg :condition))
  (:default-initargs
   :output (make-array initial-output-length :element-type 'character :adjustable t)
   :output-consumer nil
   :condition nil))

(defun extend-result-output (result)
  (setf (result-output result) (adjust-array (result-output result) (* output-size-scale-factor (length (result-output result))))))

(defclass master-mixin () ())
(defclass slave-mixin () ())

(defclass master-update (master-mixin) ())
(defclass slave-fetch (slave-mixin) ())
(defclass slave-load (slave-mixin) ())
(defclass slave-test (slave-mixin) ())

(defclass master-update-result (master-update result) ())
(defclass slave-fetch-result (slave-fetch result) ())
(defclass slave-load-result (slave-load result) ())
(defclass slave-test-result (slave-test result) ())

(defclass test-phase (period) 
  ()
  (:default-initargs
   :start-time (get-universal-time)))

(defclass buildmaster-run (period)
  ((lock :reader master-run-lock :initarg :lock)
   (condvar :accessor master-run-condvar :initarg :condvar)
   (modules :reader master-run-modules :initarg :modules)
   (n-phases :reader master-run-n-phases :initarg :n-phases)
   (n-phase-results :accessor master-run-n-phase-results :initarg :n-phase-results)
   (results :accessor master-run-results :initarg :results))
  (:default-initargs
   :lock (bordeaux-threads:make-lock)
   :condvar (bordeaux-threads:make-condition-variable)
   :start-time (get-universal-time)))

(defvar *buildmaster-runs* nil)

(defmacro with-master-run-lock ((master) &body body)
  `(bordeaux-threads:with-lock-held ((master-run-lock ,master))
     ,@body))

(defun prepare-result-vector (n-phases n-phase-results modules locality)
  (lret ((results (make-array (* n-phases n-phase-results) :fill-pointer t)))
    (iter (for base below n-phases)
          (iter (for i below n-phase-results)
                (for m in modules)
                (setf (aref results (+ base i))
                      (make-instance 'result :module m
                                     :path (if (zerop base)
                                               (module-pathname m locality)
                                               (result-path (aref results (+ base (- n-phase-results) i))))))))
    (setf (fill-pointer results) 0)))

(defgeneric open-result (master-run)
  (:method ((o buildmaster-run))
    (let* ((result-vector (phase-results o))
           (index (fill-pointer result-vector)))
      (with-master-run-lock (o)
        (incf (fill-pointer result-vector))
        (lret ((r (aref result-vector index)))
          (start-period r)
          (setf (result-commit-id r) (desr::ref-value '("master") (result-path r)))
          (bordeaux-threads:condition-notify (master-run-condvar o)))))))

(defmethod initialize-instance :after ((o buildmaster-run) &key modules n-phases locality)
  (let ((n-phase-results (length modules)))
    (setf (master-run-results o) (prepare-result-vector n-phases n-phase-results modules locality)
          (master-run-n-phase-results o) n-phase-results)))

(defgeneric grab-result (master i stream)
  (:method ((o buildmaster-run) (i integer) (stream stream))
    (with-master-run-lock (o)
      (let* ((result-vector (master-run-results o))
             (r (aref result-vector i)))
        (if (< i (1- (fill-pointer result-vector)))
            (values r t)
            (progn (finish-output stream)
                   (setf (result-output-consumer r) stream)
                   ;; let master take over the stream and wake us once it's done
                   (bordeaux-threads:condition-wait (master-condvar o) (master-lock o))
                   (values r nil)))))))

(defgeneric finalise-result (result successp condition total-octets &optional octets-unwritten)
  (:method ((o result) successp condition total-octets &optional (octets-unwritten total-octets))
    (end-period o)
    (setf (result-success-p o) successp
          (result-condition o) condition)
    (when (result-output-consumer o)
      (write-sequence (result-output o) (result-output-consumer o)
                      :start (- total-octets octets-unwritten)
                      :end total-octets)
      (finish-output (result-output-consumer o)))))

(defgeneric invoke-with-complete-result (result fn)
  (:method ((o result) fn)
    (if (period-started-p o)
        (with-result-lock (o)
          (bordeaux-threads:condition-wait (result-condvar o) (result-lock o))
          (funcall fn))
        (error "~@<Period ~A hasn't started yet.~:@>" o))))

(defmacro with-complete-result ((result) &body body)
  `(invoke-with-complete-result ,result (lambda () ,@body)))

(defclass master-update-phase (master-update test-phase) ())
(defclass slave-fetch-phase (slave-fetch test-phase) ())
(defclass slave-load-phase (slave-load test-phase) ())
(defclass slave-test-phase (slave-test test-phase) ())

(defun buildmaster-one (&key (hostname *default-buildslave-host*) (username *default-buildslave-username*)
                        purge purge-metastore branch metastore-branch disable-debugger verbose)
  (find-executable 'ssh)
  (let* ((gate (gate *self*))
         (modules (append (location-module-names gate) (gate-converted-module-names gate)))
         (commands (cook-buildslave-command purge purge-metastore branch metastore-branch disable-debugger verbose)))
    (let ((phase (make-phase 'test-phase modules gate)))
      (push phase )
      (iter (for r in-vector (phase-results phase))
            (for m = (result-module r))
            (start-result r)
            (destructuring-bind (&key name mode status output condition) (list* :name (name m) :mode :convert (module-test-fetchability m))
              (format t "module ~A, ~A ~A~:[~; encountered condition: ~:*~A, output was:~%~A~]~%" name mode status condition output)

              (finalise-result r status condition (length output)))))
    (multiple-value-bind (status output condition)
        (let (successp
              condition)
          (let ((output
                 (with-output-to-string (capture)
                   (with-pipe-stream (pipe :element-type 'character :buffering :none)
                     (let ((*executable-standard-output-direction* pipe)
                           (commands (compile-shell-command commands)))
                       (handler-case
                           (let ((process (with-asynchronous-execution
                                            (with-input-from-string (stream commands)
                                              (with-executable-input-stream stream
                                                (ssh `(,username "@" ,hostname) "bash" "-s"))))))
                             (declare (ignore process))
                             (close (two-way-stream-output-stream pipe))
                             (loop :for line = (read-line pipe nil nil)
                                :while line
                                :do
                                (format t "> ~A~%" line)
                                (finish-output)
                                (write-line line capture)))
                         (serious-condition (c)
                           (setf condition c))))))))
            (apply #'values
                   successp
                   output
                   (when condition (list condition)))))
      (if-let ((marker-posn (search (prin1-to-string *remote-output-marker*) output)))
        (with-input-from-string (s output :start (+ marker-posn 2 (length (symbol-name *remote-output-marker*))))
          (iter (for form = (read s nil nil))
                (while form)
                (destructuring-bind (&key name mode status output condition) form
                  (declare (ignore output))
                  (format t "module ~A, ~A ~A~:[~; encountered condition: ~:*~A~]~%" name mode status condition))))
        (build-test-error "~<@Marker ~S wasn't found in remote output.~:@>" *remote-output-marker*))
      status)))

(defun cl-waterfall ()
  (if-let ((run (first *buildmaster-runs*)))
    (let* ((binary-pipe (send-headers))
           (pipe (flexi-streams:make-flexi-stream binary-pipe)))
      )
    (no-runs-as-of-yet-page-generator)))

(defun start-cl-waterfall ()
  (push (create-regex-dispatcher "/desire-buildbot" 'cl-waterfall) *dispatch-table*))

(defun no-runs-as-of-yet-page-generator ()
  "<pre> There are no buildmaster runs, as of yet.</pre>")