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


(defgeneric emit-master-run-header (stream master-run)
  (:method (stream (o buildmaster-run))
    (let ((max-length (iter (for m in (master-run-modules o))
                            (maximize (length (symbol-name (name m))))))
          (names (mapcar (compose #'symbol-name #'name) (master-run-modules o))))
      (with-html-output (stream)
        (:div :class "runheader cfont"
              (:div :class "letterule"
                    (iter (with current-letter = #\#)
                          (for m in names)
                          (cond ((char= current-letter (schar m 0))
                                 (htm (:div :class "let cell" "&nbsp;&nbsp; ")))
                                (t
                                 (setf current-letter (schar m 0))
                                 (htm (:div :class "let cell" (str (format nil "~A.." current-letter))))))))
              (:div :class "names"
                    (dolist (m names)
                      (htm (:div :class "mn0 cell"
                                 (:div :class "mn1"
                                       (let ((name m))
                                         (dotimes (i (- max-length (length name)))
                                           (str "&nbsp;&nbsp;")
                                           (terpri stream))
                                         (iter (for c in-vector name)
                                               (write-char (char-downcase c) stream)
                                               (str "&nbsp; ")))))))))))))

(defgeneric invoke-with-phase-emission (stream phase fn)
  (:method ((stream stream) (o test-phase) (fn function))
    (with-html-output (stream)
      (:div :class "phase"
            (:div :class "phase-header" "Phase #" (str (princ-to-string (phase-nr o))) ": " (str (describe-phase o)) ".")
            (:div :class "result-row cfont"
                  (str (funcall fn)))))))

(defmacro with-phase-emission ((stream phase) &body body)
  `(invoke-with-phase-emission ,stream ,phase (lambda () ,@body)))

(defgeneric emit-master-run-results (stream master-run)
  (:method (stream (o buildmaster-run))
    (with-html-output (stream)
      (:div :class "phases"
            (let ((n-phases-total (master-run-n-phases o))
                  (n-complete-results (master-run-n-complete-results o))
                  (n-phase-results (master-run-n-phase-results o)))
              (multiple-value-bind (n-complete-phases n-incomplete-phase-complete-results) (floor n-complete-results n-phase-results)
                ;; quickly write complete phases
                (iter (for phase-no below n-complete-phases)
                      (for phase in (master-run-phases o))
                      (for base from 0 by n-phase-results)
                      (with-phase-emission (stream phase)
                        (dotimes (i n-phase-results)
                          (with-result-emission (stream (master-result o (+ base i)))))))
                (let* ((first-incomplete-phase-nr n-complete-phases)
                       (incomplete-phases (nthcdr first-incomplete-phase-nr (master-run-phases o))))
                  (when-let ((incomplete-phase (and (< n-complete-phases n-phases-total)
                                                    (first incomplete-phases)))
                             (incomplete-phase-base (* n-phase-results first-incomplete-phase-nr)))
                    ;; quickly write complete part of the incomplete phase
                    (with-phase-emission (stream incomplete-phase)
                      (iter (for i from incomplete-phase-base)
                            (repeat n-incomplete-phase-complete-results)
                            (with-result-emission (stream (master-result o i))))
                      (finish-output stream)
                      ;; follow the buildmaster for the incomplete part (guaranteed to be at least 1 module, due to FLOOR above)
                      (iter (for i from (+ incomplete-phase-base n-incomplete-phase-complete-results))
                            (repeat (- n-phase-results n-incomplete-phase-complete-results))
                            (with-result-emission (stream (master-result o i)) ;; pre-output is safe
                              (grab-result o i))
                            (finish-output stream)))
                    ;; follow the buildmaster for the rest of phases
                    (iter (for phase-no from (1+ first-incomplete-phase-nr) below n-phases-total)
                          (for phase in (rest incomplete-phases))
                          (for base from (* n-phase-results (1+ first-incomplete-phase-nr)) by n-phase-results)
                          (with-phase-emission (stream phase)
                            (iter (for i from base)
                                  (repeat n-phase-results)
                                  (with-result-emission (stream (master-result o i))
                                    (grab-result o i))
                                  (finish-output stream))))))))))))

(defgeneric emit-master-run (stream master-run complete-p &optional header-p)
  (:method (stream (o buildmaster-run) complete-p &optional header-p)
    (with-html-output (stream)
      (:div :class "run"
            (:div (str (multiple-value-call #'print-decoded-time
                         (decode-universal-time (period-start-time o))))
                  (str (format nil ". Run of ~D modules and ~D phases ~(~A~)."
                               (master-run-n-phase-results o)
                               (master-run-n-phases o)
                               (mapcar #'type-of (master-run-phases o)))))
            (:div :class "run-status"
                  "Status: " (str (cond ((processingp o)
                                         (format nil "still going, with ~D tests complete"
                                                 (master-run-n-complete-results o)))
                                        ((terminatedp o)
                                         (format nil "terminated~:[, with no known reason~;, due to following condition: ~:*~A~]"
                                                 (action-condition o)))
                                        ((period-ended-p o)
                                         (format nil "completed successfully at ~A"
                                                 (multiple-value-call #'print-decoded-time
                                                   (decode-universal-time (period-end-time o)))))
                                        (t
                                         (format nil "failed, with uncaught exception, with ~D tests complete"
                                                 (master-run-n-complete-results o))))) ".")
            (when complete-p
              (when header-p
                (emit-master-run-header stream o))
              (finish-output stream)
              (emit-master-run-results stream o))))))

;; "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
(defun cl-waterfall ()
  (let* ((hostname (string-downcase (string (name *self*))))
         (parameters (iter (for (param . value) in (get-parameters*))
                           (appending (list (intern (string-upcase param) :keyword) value))))
         (mode (or (when-let ((mode (getf parameters :mode)))
                     (make-keyword (string-upcase mode)))
                   :overview)))
    (ecase mode
      (:output
       (let ((result-id (or (let ((result-id (getf parameters :result-id)))
                              (and result-id (ignore-errors (parse-integer result-id))))
                            (buildmaster-error "~@<In result output mode: no result id provided.~:@>"))))
         (setf (content-type*) "text/plain")
         (when-let ((m-r (first *buildmaster-runs*)))
           (let ((r (master-result m-r result-id)))
             (subseq (result-output r) 0 (result-output-bytes r))))))
      (:cond
       (let ((result-id (or (let ((result-id (getf parameters :result-id)))
                              (and result-id (ignore-errors (parse-integer result-id))))
                            (buildmaster-error "~@<In result condition mode: no result id provided.~:@>"))))
         (setf (content-type*) "text/plain")
         (when-let ((m-r (first *buildmaster-runs*)))
           (let ((r (master-result m-r result-id)))
             (concatenate 'string
                          (when-let ((condition (action-condition r)))
                            (format nil "~A~%" condition))
                          (when-let ((backtrace (action-backtrace r)))
                            (format nil "~%The backtrace was:~%~%~A~%" backtrace)))))))
      (:overview
       (let* ((binary-stream (send-headers))
              (stream (flexi-streams:make-flexi-stream binary-stream)))
         (with-html-output (stream nil :prologue t)
           (:html :class "root" :xmlns "http://www.w3.org/1999/xhtml" :|XML:LANG| "en" :lang "en"
                  (:head (:title "desire buildbot waterfall on " (str hostname))
                         (:style :type "text/css" (str *style*)))
                  (:body :class "body"
                         (:div :class "header"
                               "Hello, this is buildmaster on " (str hostname) " speaking." (:br)
                               "Local time is " (str (multiple-value-call #'print-decoded-time
                                                       (get-decoded-time))) ".")
                         (destructuring-bind (&optional first-run &rest rest-runs) *buildmaster-runs*
                           (cond (first-run
                                  (print-legend stream)
                                  (emit-master-run stream first-run t t)
                                  (dolist (run rest-runs)
                                    (emit-master-run stream run t)))
                                 (t
                                  (htm :br
                                       (:div :class "no-runs"
                                             "There have been no buildmaster runs to speak of."))))))))
         (finish-output stream))))))

(defun start-cl-waterfall (&optional (prefix "/desire-waterfall"))
  (push (create-regex-dispatcher prefix 'cl-waterfall) *dispatch-table*))

(defparameter *style*
  "<!--
.body {
  overflow: scroll;
  margin-bottom: 15em;
}
.run {
  min-width: 3000px;
  clear: both;
  margin-top: 2em;
  background: #f0f0ff;
}
.cell {
  padding: 1px;
  width: 2em;
}
.cfont {
  font-size: 120%;
  font-family: monospace;
}
.letterule * {
  background: #FFE4B5;
}
.letterule, .names {
  clear: both;
}
.let {
  float: left;
}
.results {
  background: green;
  clear: both;
}
.phase {
  clear: both;
}
.result-row {
  height: 4em;
}
.mn0 {
  display: block;
  float: left;
  background: #fff8dc;
}
.mn1 {
  padding: 0px;
}
.result {
  display: inline;
  float: left;
  position: relative;
}
.result > a > * {
  font-size: 50%;
}
.nhint {
  display: none;
  background: #FFE4B5;
  position: absolute;
  bottom: -3em;
  left: 1em;
  z-index: 10;
}
.nhint * {
  display: inline;
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
.excp {
  background: #FFA07A;
}
.fail {
  background: #FF4500;
}
.legend {
  background: #FFE4B5;
  border: 1px solid black;
  width: auto;
  padding: 2px;
  height: 7em;
}
.legend-entry {
  display: inline;
  clear: both;
}
-->")