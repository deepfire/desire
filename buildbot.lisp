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
  (:use :common-lisp :iterate :alexandria :pergamum :executor :portable-spawn :desire :hunchentoot)
  (:export
   ;; buildslave entry point
   #:buildslave
   ;; buildmaster
   #:buildmaster-error #:simple-buildmaster-error #:simple-build-test-error
   #:buildmaster-one
   ;; buildmaster web interface
   #:start-cl-waterfall))

(in-package :desire-buildbot)

(define-condition buildmaster-error (desire-error) ())
(define-simple-error buildmaster-error)

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
        (buildmaster-error "~<@Marker ~S wasn't found in remote output.~:@>" *remote-output-marker*))
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
        (buildmaster-error "~@<Period not finalised.~:@>"))))

(defclass action (period)
  ((successp :accessor action-success-p :initarg :successp)
   (terminatedp :accessor action-terminated-p :initarg :terminatedp)
   (condition :accessor action-condition :initarg :condition))
  (:default-initargs
   :successp nil
   :terminatedp nil
   :condition nil))

(defgeneric terminate-action (action &optional condition)
  (:method ((o action) &optional condition)
    (declare (ignore condition)))
  (:method :around ((o action) &optional condition)
    (call-next-method)
    (setf (action-success-p o) nil
          (action-terminated-p o) t
          (action-condition o) condition)))

(defun invoke-with-tracked-termination (action fn)
  (handler-case (funcall fn)
    (serious-condition (c)
      (terminate-action action c)
      (error c))))

(defmacro with-tracked-termination ((action) &body body)
  `(invoke-with-tracked-termination ,action (lambda () ,@body)))

(defconstant header-leeway 32)
(defconstant page-size (desr::virtual-memory-page-size))
(defconstant initial-output-length (- page-size header-leeway))
(defconstant output-size-scale-factor 4)

(defclass result (action)
  ((module :accessor result-module :initarg :module)
   (path :accessor result-path :initarg :path)
   (commit-id :accessor result-commit-id :initarg :commit-id)
   (output :accessor result-output :initarg :output)
   (output-consumer :accessor result-output-consumer :initarg :output-consumer))
  (:default-initargs
   :output (make-array initial-output-length :element-type 'character :adjustable t)
   :output-consumer nil))

(defun extend-result-output (result)
  (setf (result-output result) (adjust-array (result-output result) (* output-size-scale-factor (length (result-output result))))))

(defclass test-phase (action) 
  ((action-description :reader phase-action-description :initarg :action-description)))

(defclass remote-test-phase (test-phase)
  ((hostname :reader remote-phase-hostname :initarg :hostname)
   (username :reader remote-phase-username :initarg :username)))

(defclass master-update-phase (test-phase)
  ()
  (:default-initargs
   :action-description "fetch upstream modules and convert them"))
(defclass slave-fetch-phase (remote-test-phase)
  ()
  (:default-initargs
   :action-description "fetch modules from wishmaster"))
(defclass slave-load-phase (remote-test-phase)
  ()
  (:default-initargs
   :action-description "load modules"))
(defclass slave-test-phase (remote-test-phase)
  ()
  (:default-initargs
   :action-description "test modules"))

(defgeneric describe-phase (test-phase)
  (:method ((o test-phase))
    (format nil "~A on ~:[the wishmaster~;a buildslave~]."
            (string-capitalize (phase-action-description o)) (typep o 'remote-test-phase))))

(defclass buildmaster-run (action)
  ((lock :reader master-run-lock :initarg :lock)
   (condvar :accessor master-run-condvar :initarg :condvar)
   (modules :reader master-run-modules :initarg :modules)
   (phases :reader master-run-phases :initarg :phases)
   (n-phases :reader master-run-n-phases :initarg :n-phases)
   (n-phase-results :accessor master-run-n-phase-results :initarg :n-phase-results)
   (results :accessor master-run-results :initarg :results))
  (:default-initargs
   :lock (bordeaux-threads:make-lock)
   :condvar (bordeaux-threads:make-condition-variable)
   :start-time (get-universal-time)))

(defvar *buildmaster-runs* nil)

(defun master-run-n-complete-results (master)
  "Not meant to be precise: can underreport without lock held."
  (1- (fill-pointer (master-run-results master))))

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

(defmethod initialize-instance :after ((o buildmaster-run) &key modules phases locality)
  (unless phases
    (buildmaster-error "~@<Cannot create a buildmaster run with no phases specified.~:@>"))
  (let ((n-phase-results (length modules))
        (n-phases (length phases)))
    (setf (master-run-phases o) (mapcar #'make-instance phases)
          (master-run-results o) (prepare-result-vector n-phases n-phase-results modules locality)
          (master-run-n-phase-results o) n-phase-results)))

;; must be called under master run lock
(defgeneric finalise-result (result successp condition output octets-unwritten)
  (:method ((o result) successp condition output octets-unwritten)
    (end-period o)
    (setf (action-success-p o) successp
          (result-output o) output
          (action-condition o) condition)
    (when (result-output-consumer o)
      (write-sequence (result-output o) (result-output-consumer o)
                      :start (- (length output) octets-unwritten))
      (finish-output (result-output-consumer o)))))

(defgeneric advance-result (master-run &optional successp condition output octets-unwritten)
  (:method ((o buildmaster-run) &optional successp condition output (octets-unwritten (length output)))
    (let* ((result-vector (phase-results o))
           (i (fill-pointer result-vector)))
      (with-master-run-lock (o)
        (when-let ((completed-r (when (plusp i)
                                  (aref result-vector (1- i)))))
          (finalise-result completed-r successp condition output octets-unwritten))
        (incf (fill-pointer result-vector))
        (bordeaux-threads:condition-notify (master-run-condvar o))
        (if (< i (* (master-run-n-phase-results  o)
                    (master-run-n-phases o)))
            (lret ((new-r (aref result-vector i)))
              (start-period new-r)
              (setf (result-commit-id new-r) (desr::ref-value '("master") (result-path new-r))))
            (end-period o))))))

;; (setf (result-output-consumer r) stream)
                   
(defgeneric grab-result (master i)
  (:method ((o buildmaster-run) (i integer))
    (with-master-run-lock (o)
      (lret ((r (aref (master-run-results o) i)))
        (unless (< i (master-run-n-complete-results o))
          ;; let master wake us once the result is done
          (bordeaux-threads:condition-wait (master-condvar o) (master-lock o)))
        r))))

(defclass master-update-phase (master-update test-phase) ())
(defclass slave-fetch-phase (slave-fetch test-phase) ())
(defclass slave-load-phase (slave-load test-phase) ())
(defclass slave-test-phase (slave-test test-phase) ())

(defparameter *buildmaster-run-phases* '(master-update-phase
                                         #+(or)
                                         slave-fetch-phase
                                         #+(or)
                                         slave-load-phase))

(defun buildmaster-one (&key (hostname *default-buildslave-host*) (username *default-buildslave-username*)
                        purge purge-metastore branch metastore-branch disable-debugger verbose)
  (find-executable 'ssh)
  (let* ((gate (gate *self*))
         (module-names (append (location-module-names gate) (gate-converted-module-names gate)))
         (modules (mapcar #'module module-names))
         (commands (cook-buildslave-command purge purge-metastore branch metastore-branch disable-debugger verbose))
         (m-r (make-instance 'buildmaster-run :locality gate :phases *buildmaster-run-phases* :modules modules)))
    ;; first goes the upstream fetch phase
    (with-tracked-termination (m-r)
      (let ((update-phase (first (master-run-phases m-r))))
        (start-period update-phase)
        (with-tracked-termination (update-phase)
          (let ((initial-result (advance-result m-r)))
            (push m-r *buildmaster-runs*)
            (iter (with r = initial-result)
                  (repeat (master-run-n-phase-results m-r))
                  (for m = (result-module r))
                  (with-tracked-termination (r)
                    (destructuring-bind (&key status output condition) (module-test-fetchability m)
                      (setf r (advance-result m-r status condition output)))))))
        (end-period update-phase)))
    ;; (multiple-value-bind (status output condition)
    ;;     (let (successp
    ;;           condition)
    ;;       (let ((output
    ;;              (with-output-to-string (capture)
    ;;                (with-pipe-stream (pipe :element-type 'character :buffering :none)
    ;;                  (let ((*executable-standard-output-direction* pipe)
    ;;                        (commands (compile-shell-command commands)))
    ;;                    (handler-case
    ;;                        (let ((process (with-asynchronous-execution
    ;;                                         (with-input-from-string (stream commands)
    ;;                                           (with-executable-input-stream stream
    ;;                                             (ssh `(,username "@" ,hostname) "bash" "-s"))))))
    ;;                          (declare (ignore process))
    ;;                          (close (two-way-stream-output-stream pipe))
    ;;                          (loop :for line = (read-line pipe nil nil)
    ;;                             :while line
    ;;                             :do
    ;;                             (format t "> ~A~%" line)
    ;;                             (finish-output)
    ;;                             (write-line line capture)))
    ;;                      (serious-condition (c)
    ;;                        (setf condition c))))))))
    ;;         (apply #'values
    ;;                successp
    ;;                output
    ;;                (when condition (list condition)))))
    ;;   (if-let ((marker-posn (search (prin1-to-string *remote-output-marker*) output)))
    ;;     (with-input-from-string (s output :start (+ marker-posn 2 (length (symbol-name *remote-output-marker*))))
    ;;       (iter (for form = (read s nil nil))
    ;;             (while form)
    ;;             (destructuring-bind (&key name mode status output condition) form
    ;;               (declare (ignore output))
    ;;               (format t "module ~A, ~A ~A~:[~; encountered condition: ~:*~A~]~%" name mode status condition))))
    ;;     (build-test-error "~<@Marker ~S wasn't found in remote output.~:@>" *remote-output-marker*))
    ;;   status)
    ))

(defgeneric emit-master-run-description (stream master-run)
  (:method (stream (o buildmaster-run))
    (format stream 
            "<div>Build master run of ~D modules, started ~A.</div><br/>~%~
               <div>Status: ~A.</div><br/>"
            (master-run-n-phase-results o)
            (multiple-value-call #'desr::print-decoded-time
              (period-start-time o))
            (cond ((action-terminated-p o)
                   (format nil "terminated~:[~;, due to following condition: ~:*~A~]"
                           (action-condition o)))
                  ((period-ended-p o)
                   (format nil "completed successfully at ~A"
                           (multiple-value-call #'desr::print-decoded-time
                             (period-start-time o))))
                  (t
                   (format nil "still going, with ~D tests complete"
                           (master-run-n-complete-results o)))))))

(defgeneric emit-phase-head (stream phase)
  (:method (stream (o test-phase))
    (format stream "<div class=\"phase\"><div class=\"phase-header\">~A</div>~%" (describe-phase o))))

(defgeneric emit-complete-result (stream master-run result)
  (:method (stream (m buildmaster-run) (r result))
    (format stream "<div class=\"result>")
    (format stream "</div>")
    (finish-output stream)))

(defgeneric emit-result-pre-output (stream result)
  (:method (stream (r result))
    (format stream "<div class=\"result\">")
    (finish-output stream)))

(defgeneric emit-result-post-output (stream result)
  (:method (stream (r result))
    (format stream "<div class=\"~A\">o</div></div>"
            (cond ((action-terminated-p r) "term")
                  ((action-success-p r) "succ")
                  (t "fail")))
    (finish-output stream)))

(defgeneric emit-phase-tail (stream phase)
  (:method (stream (o test-phase))
    (format stream "</div>~%")))

(defgeneric emit-master-run-results (stream master-run)
  (:method (stream (o buildmaster-run))
    (let ((n-phases-total (master-run-n-phases o))
          (n-complete-results (master-run-n-complete-results o))
          (n-phase-results (master-run-n-phase-results o)))
      (multiple-value-bind (n-complete-phases n-incomplete-phase-complete-results) (floor n-complete-results n-phase-results)
        ;; quickly write complete phases
        (iter (for phase-no to n-complete-phases)
              (for phase in (buildmaster-run-phases o))
              (for base from 0 by n-phase-results)
              (emit-phase-head stream phase)
              (dotimes (i n-phase-results)
                (emit-result stream o (aref (master-run-results o) i)))
              (emit-phase-tail stream phase))
        (let* ((first-incomplete-phase-nr n-complete-phases)
               (incomplete-phases (nthcdr first-incomplete-phase-nr (master-run-phases o))))
          (when-let ((incomplete-phase (and (< n-complete-phases n-phases-total)
                                            (first incomplete-phases))))
            ;; quickly write complete part of the incomplete phase
            (emit-phase-head stream incomplete-phase)
            (iter (for i from (* n-phase-results first-incomplete-phase-nr))
                  (repeat n-incomplete-phase-complete-results)
                  (emit-result stream o (aref (master-run-results o) i)))
            ;; follow the buildmaster for the incomplete part (guaranteed to be at least 1 module, due to FLOOR above)
            (iter (for i from n-complete-results)
                  (repeat (- n-phase-results n-incomplete-phase-complete-results))
                  (emit-result-pre-output stream r)
                  (for r = (grab-result o i))
                  (emit-result-post-output stream r))
            (emit-phase-tail stream incomplete-phase)
            ;; follow the buildmaster for the rest of phases
            (iter (for phase-no from (1+ first-incomplete-phase-nr) below n-phases-total)
                  (for phase in (rest incomplete-phases))
                  (for base from (* n-phase-results (1+ first-incomplete-phase-nr)) by n-phase-results)
                  (emit-phase-head stream phase)
                  (iter (for i from base)
                        (repeat n-phase-results)
                        (emit-result-pre-output stream r)
                        (for r = (grab-result o i))
                        (emit-result-post-output stream r))
                  (emit-phase-tail stream phase))))))))

(defun cl-waterfall ()
  (if-let ((run (first *buildmaster-runs*)))
    (let* ((binary-pipe (send-headers))
           (pipe (flexi-streams:make-flexi-stream binary-pipe)))
      (write-string (emit-head) pipe)
      (emit-master-run-description pipe run)
      (finish-output pipe)
      (emit-master-run-results pipe run)
      (write-string (emit-tail) pipe)
      (finish-output pipe))
    (no-runs-as-of-yet-page-generator)))

(defun start-cl-waterfall ()
  (push (create-regex-dispatcher "/desire-waterfall" 'cl-waterfall) *dispatch-table*))

;;;
;;; Boring.
;;;
(defun no-runs-as-of-yet-page-generator ()
  (concatenate 'string
               (head)
               "<pre>There are no buildmaster runs, as of yet.</pre>"
               (tail)))

(defun emit-head ()
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
<html>
<head>
<title>desire buildbot waterfall</title>
</head>

<body>
")

(defun emit-tail ()"
</body>
</html>")
