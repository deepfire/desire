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
    (end-period o)
    (setf (action-success-p o) nil
          (action-terminated-p o) t
          (action-condition o) condition)))

(defun invoke-with-tracked-termination (action fn)
  (unwind-protect-case ()
      (handler-case (funcall fn)
        (serious-condition (c)
          (terminate-action action c)
          (error c)))
    (:abort
     (unless (action-condition action)
       (terminate-action action nil)))))

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
  ((action-description :reader phase-action-description :initarg :action-description)
   (nr :reader phase-nr :initarg :nr)))

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
    (format nil "~A on ~:[the wishmaster~;a buildslave~]; ~:[not started~; started ~:*~A~]~:[~;, finished ~:*~A~]"
            (phase-action-description o) (typep o 'remote-test-phase)
            (when (period-started-p o)
              (multiple-value-call #'desr::print-decoded-time
                (decode-universal-time (period-start-time o))))
            (when (period-ended-p o)
              (multiple-value-call #'desr::print-decoded-time
                (decode-universal-time (period-end-time o)))))))

(defclass buildmaster-run (action)
  ((lock :reader master-run-lock :initarg :lock)
   (condvar :reader master-run-condvar :initarg :condvar)
   (modules :reader master-run-modules :initarg :modules)
   (phases :reader master-run-phases :initarg :phases)
   (n-phases :reader master-run-n-phases :initarg :n-phases)
   (n-phase-results :reader master-run-n-phase-results :initarg :n-phase-results)
   (results :reader master-run-results :initarg :results))
  (:default-initargs
   :lock (bordeaux-threads:make-lock)
   :condvar (bordeaux-threads:make-condition-variable)
   :start-time (get-universal-time)))

(defmethod terminate-action :after ((o buildmaster-run) &optional condition)
  (declare (ignore condition))
  (let ((result-vector (master-run-results o)))
    (setf (fill-pointer result-vector) (length result-vector))))

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

(defun prepare-result-vector (n-phases n-phase-results modules locality)
  (lret ((results (make-array (* n-phases n-phase-results) :fill-pointer t)))
    (iter (for phase-no below n-phases)
          (for base from 0 by n-phase-results)
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
    (setf (slot-value o 'phases) (iter (for i from 0)
                                       (for phase-type in phases)
                                       (collect (make-instance phase-type :nr i)))
          (slot-value o 'n-phases) n-phases
          (slot-value o 'results) (prepare-result-vector n-phases n-phase-results modules locality)
          (slot-value o 'n-phase-results) n-phase-results)))

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
    (let* ((result-vector (master-run-results o))
           (i (fill-pointer result-vector)))
      (with-master-run-lock (o)
        (when-let ((completed-r (when (plusp i)
                                  (aref result-vector (1- i)))))
          (finalise-result completed-r successp condition output octets-unwritten))
        (bordeaux-threads:condition-notify (master-run-condvar o))
        (cond ((< i (* (master-run-n-phase-results o)
                       (master-run-n-phases o)))
               (incf (fill-pointer result-vector))
               (lret ((new-r (aref result-vector i)))
                 (start-period new-r)
                 (setf (result-commit-id new-r) (desr::ref-value '("master") (result-path new-r)))))
              (t
               (end-period o)))))))

;; (setf (result-output-consumer r) stream)
                   
(defgeneric grab-result (master i)
  (:method ((o buildmaster-run) (i integer))
    (with-master-run-lock (o)
      (lret ((r (aref (master-run-results o) i)))
        (unless (< i (master-run-n-complete-results o))
          ;; let master wake us once the result is done
          (bordeaux-threads:condition-wait (master-run-condvar o) (master-run-lock o)))
        r))))

(defparameter *buildmaster-run-phases* '(master-update-phase
                                         #+(or)
                                         slave-fetch-phase
                                         #+(or)
                                         slave-load-phase))

(defun buildmaster-one (&key (hostname *default-buildslave-host*) (username *default-buildslave-username*)
                        purge purge-metastore branch metastore-branch disable-debugger verbose)
  (find-executable 'ssh)
  (let* ((gate (gate *self*))
         (module-names (sort (copy-list (append (location-module-names gate) (gate-converted-module-names gate)))
                             #'string<))
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

(defgeneric emit-master-run (stream master-run fresh-p)
  (:method (stream (o buildmaster-run) fresh-p)
    (format stream 
            "<br/><br/><div class=\"run\"><div>~A Run of ~D modules and ~D phases ~(~A~).</div>~
             <div class=\"run-status\">Status: ~A.</div>~%~%"
            (multiple-value-call #'desr::print-decoded-time
              (decode-universal-time (period-start-time o)))
            (master-run-n-phase-results o)
            (master-run-n-phases o)
            (mapcar #'type-of (master-run-phases o))
            (cond ((action-terminated-p o)
                   (format nil "terminated~:[, with no known reason~;, due to following condition: ~:*~A~]"
                           (action-condition o)))
                  ((period-ended-p o)
                   (format nil "completed successfully at ~A"
                           (multiple-value-call #'desr::print-decoded-time
                             (decode-universal-time (period-end-time o)))))
                  (fresh-p
                   (format nil "still going, with ~D tests complete"
                           (master-run-n-complete-results o)))
                  (t
                   (format nil "failed, with uncaught exception, with ~D tests complete"
                           (master-run-n-complete-results o)))))
    (when fresh-p
      (emit-master-run-header stream o)
      (finish-output stream)
      (emit-master-run-results stream o))
    (write-string "</div>" stream)))

(defgeneric emit-master-run-header (stream master-run)
  (:method (stream (o buildmaster-run))
    (let ((max-length (iter (for m in (master-run-modules o))
                            (maximize (length (symbol-name (name m)))))))
      (format stream "<div class=\"runheader\">")
      (dolist (m (mapcar (compose #'symbol-name #'name) (master-run-modules o)))
        (format stream "<div class=\"mn0\"><div class=\"mn1\">")
        (let ((name m))
          (dotimes (i (- max-length (length name)))
            (write-string "&nbsp;&nbsp;
" stream))
          (iter (for c in-vector name)
                (write-char (char-downcase c) stream)
                (write-string "&nbsp; " stream)))
        (format stream "</div></div>"))
      (format stream "</div>~%"))))

(defgeneric emit-phase-head (stream phase)
  (:method (stream (o test-phase))
    (format stream "<div class=\"phase\"><div class=\"phase-header\">Phase #~D: ~A.</div>~%" (phase-nr o) (describe-phase o))))

(defgeneric emit-result-head (stream result &optional flush-output)
  (:method (stream (r result) &optional (flush-output t))
    (format stream "<div class=\"result\"><div class=\"nhint\">~A</div>" (string-downcase (symbol-name (name (result-module r)))))
    (when flush-output
      (finish-output stream))))

(defun print-legend (stream)
  (format stream "<br/><br/><span><div class=\"legend\">Legend:<br/>")
  (iter (for (class sign desc) in '(("nevr" "n" "the test was never run")
                                    ("term" "x" "an exception occured while running test")
                                    ("succ" "o" "the test completed successfully")
                                    ("fail" "f" "the test failed")))
        (format stream "<span class=\"legend-entry\" style=\"clear: both; background: green;\"><span style=\"clear: both; float: left\"><div class=\"result ~A\">~A</div> - ~A</span></span>"
                class sign desc))
  (format stream "</div></span>"))

(defgeneric emit-result-completion (stream result &optional flush-output)
  (:method (stream (r result) &optional (flush-output t))
    (multiple-value-bind (class sign) (cond ((not (period-started-p r)) (values "nevr" "n"))
                                            ((action-terminated-p r) (values "term" "x"))
                                            ((action-success-p r) (values "succ" "o"))
                                            (t (values "fail" "f")))
      (format stream "<div class=\"~A\">~A</div></div>" class sign))
    (when flush-output
      (finish-output stream))))

(defgeneric emit-complete-result (stream result)
  (:method (stream (r result))
    (emit-result-head stream r nil)
    (emit-result-completion stream r nil)
    (finish-output stream)))

(defgeneric emit-phase-tail (stream phase)
  (:method (stream (o test-phase))
    (format stream "</div>~%")))

(defgeneric emit-master-run-results (stream master-run)
  (:method (stream (o buildmaster-run))
    (let ((n-phases-total (master-run-n-phases o))
          (n-complete-results (master-run-n-complete-results o))
          (n-phase-results (master-run-n-phase-results o)))
      (format stream "<div class=\"phases\">")
      (multiple-value-bind (n-complete-phases n-incomplete-phase-complete-results) (floor n-complete-results n-phase-results)
        ;; quickly write complete phases
        (iter (for phase-no below n-complete-phases)
              (for phase in (master-run-phases o))
              (for base from 0 by n-phase-results)
              (emit-phase-head stream phase)
              (dotimes (i n-phase-results)
                (emit-complete-result stream (aref (master-run-results o) (+ base i))))
              (emit-phase-tail stream phase))
        (let* ((first-incomplete-phase-nr n-complete-phases)
               (incomplete-phases (nthcdr first-incomplete-phase-nr (master-run-phases o))))
          (format t "phases-total ~D, complete-results ~D, phase-results, ~D, complete-phases ~D, incomplete-phases-complete-results ~D, first-incomplete-phase ~D, incomplete-phases ~D, product: ~D~%"
                  n-phases-total n-complete-results n-phase-results n-complete-phases n-incomplete-phase-complete-results first-incomplete-phase-nr incomplete-phases
                  (* n-phase-results first-incomplete-phase-nr))
          (when-let ((incomplete-phase (and (< n-complete-phases n-phases-total)
                                            (first incomplete-phases)))
                     (incomplete-phase-base (* n-phase-results first-incomplete-phase-nr)))
            ;; quickly write complete part of the incomplete phase
            (emit-phase-head stream incomplete-phase)
            (iter (for i from incomplete-phase-base)
                  (repeat n-incomplete-phase-complete-results)
                  (emit-complete-result stream (aref (master-run-results o) i)))
            ;; follow the buildmaster for the incomplete part (guaranteed to be at least 1 module, due to FLOOR above)
            (iter (for i from (+ incomplete-phase-base n-incomplete-phase-complete-results))
                  (repeat (- n-phase-results n-incomplete-phase-complete-results))
                  (emit-result-head stream (aref (master-run-results o) i)) ;; pre-output is safe
                  (for r = (grab-result o i))
                  (emit-result-completion stream r))
            (emit-phase-tail stream incomplete-phase)
            ;; follow the buildmaster for the rest of phases
            (iter (for phase-no from (1+ first-incomplete-phase-nr) below n-phases-total)
                  (for phase in (rest incomplete-phases))
                  (for base from (* n-phase-results (1+ first-incomplete-phase-nr)) by n-phase-results)
                  (emit-phase-head stream phase)
                  (iter (for i from base)
                        (repeat n-phase-results)
                        (emit-result-head stream (aref (master-run-results o) i))
                        (for r = (grab-result o i))
                        (emit-result-completion stream r))
                  (emit-phase-tail stream phase)))))
      (format stream "</div>"))))

(defun cl-waterfall ()
  (let* ((binary-stream (send-headers))
         (stream (flexi-streams:make-flexi-stream binary-stream)))
    (emit-head stream *self*)
    (destructuring-bind (&optional first-run &rest rest-runs) *buildmaster-runs*
      (cond (first-run
             (print-legend stream)
             (emit-master-run stream first-run t)
             (dolist (run rest-runs)
               (emit-master-run stream run nil)))
            (t
             (emit-no-master-runs stream))))
    (emit-tail stream *self*)
    (finish-output stream)))

(defun start-cl-waterfall ()
  (push (create-regex-dispatcher "/desire-waterfall" 'cl-waterfall) *dispatch-table*))

;;;
;;; Boring.
;;;
(defun emit-head (stream wishmaster)
  ;; writing-mode: tb-rl;
  (format stream "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" class=\"root\">
<head>
<title>desire buildbot waterfall on ~A</title>
<style type=\"text/css\"><!--
.body {
  overflow: scroll;
}
.run {
  min-width: 3000px;
  position: relative;
}
.runheader {
}
.runheader, .result {
  font-family: monospace;
  font-size: 120%;
}
.results {
  background: green;
  clear: both;
}
.phase {
  clear: both;
}
.mn0 {
  width: 2em;
  display: block;
  float: left;
  background: #fff8dc;
  padding: 1px;
}
.result {
  display: inline;
  float: left;
  width: 2em;
  padding: 1px;
  position: relative;
}
.nhint {
  display: none;
  background: #FFE4B5;
  position: absolute;
  top: 2em;
  left: 1em;
  z-index: 10;
}
.result:hover .nhint {
  display: block;
  border: 1px solid black;
}
.nevr {
  background: gray;
}
.succ {
  background: #90EE90;
}
.term {
  background: #FFA07A;
}
.fail {
  background: #FF4500;
}
.legend {
  background: #FFE4B5;
  border: 1px solid black;
  max-width: auto;
  width: auto;
  padding: 2px;
  height: 7em;
}
.legend-entry {
  display: inline;
  clear: both;
}
--></style>
</head>
<body class=\"body\">
<div class=\"header\">Hello, this is buildmaster on ~:*~A speaking.<br/>Local time is ~A.</div>
"
          (string-downcase (string (name wishmaster)))
          (multiple-value-call #'desr::print-decoded-time
            (get-decoded-time))))

(defun emit-tail (stream wishmaster)
  (declare (ignore wishmaster))
  (format stream "
</body>
</html>"))

(defun emit-no-master-runs (stream)
  (format stream "<br/><div class=\"no-runs\">There have been no buildmaster runs to speak of.</div>~%"))