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

(in-package :desire-buildbot)


;;;;
;;;; Actual emission
;;;;
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
  (:method ((stream stream) (o test-phase) (fn function) &aux
            (results (master-run-results (phase-run o))))
    (with-html-output (stream)
      (:div :class "phase"
            (:div :class "phase-header" "Phase #" (str (princ-to-string (phase-nr o))) ": " (str (describe-phase o)) ". "
                  "Total of "
                  (str (princ-to-string (count-if (of-type 'failure) results
                                                  :start (phase-base o)
                                                  :end (min (length results)
                                                            (+ (phase-base o) (phase-n-results o)))))) " failures.")
            (:div :class "result-row cfont"
                  (str (funcall fn)))))))

(defmacro with-phase-emission ((stream phase) &body body)
  `(invoke-with-phase-emission ,stream ,phase (lambda () ,@body)))

(defgeneric emit-master-run-results (stream master-run only-failures)
  (:method (stream (o buildmaster-run) only-failures)
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
                          (let ((r (master-result o (+ base i))))
                            (unless (and only-failures (not (typep r 'failure)))
                              (with-result-emission (stream r)))))))
                (let* ((first-incomplete-phase-nr n-complete-phases)
                       (incomplete-phases (nthcdr first-incomplete-phase-nr (master-run-phases o))))
                  (when-let ((incomplete-phase (and (< n-complete-phases n-phases-total)
                                                    (first incomplete-phases)))
                             (incomplete-phase-base (* n-phase-results first-incomplete-phase-nr)))
                    ;; quickly write complete part of the incomplete phase
                    (with-phase-emission (stream incomplete-phase)
                      (iter (for i from incomplete-phase-base)
                            (repeat n-incomplete-phase-complete-results)
                            (let ((r (master-result o i)))
                              (unless (and only-failures (not (typep r 'failure)))
                                (with-result-emission (stream r)))))
                      (finish-output stream)
                      ;; follow the buildmaster for the incomplete part (guaranteed to be at least 1 module, due to FLOOR above)
                      (iter (for i from (+ incomplete-phase-base n-incomplete-phase-complete-results))
                            (repeat (- n-phase-results n-incomplete-phase-complete-results))
                            (let ((r (master-result o i)))
                              (unless (and only-failures (not (typep r 'failure)))
                                (with-result-emission (stream r) ;; pre-output is safe
                                  (grab-result o i))))
                            (finish-output stream)))
                    ;; follow the buildmaster for the rest of phases
                    (iter (for phase-no from (1+ first-incomplete-phase-nr) below n-phases-total)
                          (for phase in (rest incomplete-phases))
                          (for base from (* n-phase-results (1+ first-incomplete-phase-nr)) by n-phase-results)
                          (with-phase-emission (stream phase)
                            (iter (for i from base)
                                  (repeat n-phase-results)
                                  (let ((r (master-result o i)))
                                    (unless (and only-failures (not (typep r 'failure)))
                                      (with-result-emission (stream r)
                                        (grab-result o i))))
                                  (finish-output stream))))))))))))

(defgeneric emit-master-run (stream master-run mode nr &optional header-p)
  (:method :around (stream (o buildmaster-run) mode nr &optional header-p)
    (declare (ignore header-p))
    (with-html-output (stream)
      (:div :class "run"
            (:div (str (multiple-value-call #'print-decoded-time
                         (decode-universal-time (period-start-time o))))
                  (fmt ". Run of ~D modules and ~D phases ~(~A~)."
                       (master-run-n-phase-results o)
                       (master-run-n-phases o)
                       (mapcar #'type-of (master-run-phases o))))
            (:div (fmt "Total ~D ~A."
                       (count-if (of-type 'failure) (master-run-results o))
                       (uri `(:failsummary ,nr) "failures")))
            (:div :class "run-status"
                  "Status: " (str (cond ((not (period-ended-p o))
                                         (format nil "still going, with ~D tests complete."
                                                 (master-run-n-complete-results o)))
                                        ((action-condition o)
                                         (format nil "terminated at ~A, due to a ~A."
                                                 (multiple-value-call #'print-decoded-time
                                                   (decode-universal-time (period-end-time o)))
                                                 (uri '(:runcond) "condition")))
                                        (t
                                         (format nil "completed successfully at ~A."
                                                 (multiple-value-call #'print-decoded-time
                                                   (decode-universal-time (period-end-time o))))))))
            (let ((*master-run-nr* nr))
              (call-next-method)))))
  (:method (stream (o buildmaster-run) (mode (eql :full)) nr &optional header-p)
    (when header-p
      (emit-master-run-header stream o))
    (finish-output stream)
    (emit-master-run-results stream o nil))
  (:method (stream (o buildmaster-run) (mode (eql :errors)) nr &optional header-p)
    (declare (ignore header-p))
    (emit-master-run-results stream o t)))

;; "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
(defun render-cl-waterfall (&optional static-args)
  (when *emitting-static*
    (unless *static-output-base-pathname*
      (buildmaster-error "~@<*STATIC-OUTPUT-BASE-PATHNAME* must be set when *EMITTING-STATIC* is non-NIL.~:@>"))
    (setf (completed-static-emission static-args) t))
  (let* ((hostname (string-downcase (string (name *self*))))
         (request-path (if *emitting-static*
                           static-args
                           (iter (for elt in (cddr (split-sequence #\/ (script-name*))))
                                 (collect (or (ignore-errors (parse-integer elt))
                                              (make-keyword (string-upcase elt)))))))
         (*current-base* (when *emitting-static*
                           (butlast request-path))))
    (destructuring-bind (&optional (mode :summary) (run-nr 0) (result-id 0)) request-path
      (flet ((mode-run-result ()
               (list (down-case-string mode) (write-to-string run-nr) (write-to-string result-id)))
             (mode-run ()
               (list (down-case-string mode) (write-to-string run-nr)))
             (mode ()
               (list (down-case-string mode))))
        (multiple-value-bind (content-type path-fn) (ecase mode
                                                      ((:out :cond)     (values :text/plain #'mode-run-result))
                                                      (:runcond            (values :text/plain #'mode-run))
                                                      (:failsummary        (values nil #'mode-run))
                                                      ((:summary :current) (values nil #'mode)))
          (unless (and (keywordp mode)
                       (integerp run-nr) (not (minusp run-nr))
                       (integerp result-id) (not (minusp result-id)))
            (return-from render-cl-waterfall nil))
          (with-maybe-static/content-type (stream content-type *static-output-base-pathname* path-fn)
            (destructuring-bind (&key return-value condition backtrace &allow-other-keys)
                (with-recorded-status (:record-backtrace t)
                  (let* ((m-r (nth run-nr *buildmaster-runs*))
                         (r (when m-r
                              (master-result m-r result-id))))
                    (ecase mode
                      (:out
                       (when r
                         (write-string (subseq (result-output r) 0 (result-output-bytes r)) stream)))
                      (:cond
                        (when r
                          (when-let ((condition (action-condition r)))
                            (princ condition stream))
                          (when-let ((backtrace (action-backtrace r)))
                            (format stream "~%The backtrace was:~%~%~A~%" backtrace))))
                      (:runcond
                       (when-let ((condition (and m-r
                                                  (not (processingp m-r))
                                                  (terminatedp m-r)
                                                  (action-condition m-r))))
                         (princ condition stream)
                         (when-let ((backtrace (action-backtrace m-r)))
                           (format stream "~%the backtrace was:~%~%~A~%" backtrace))))
                      ((:failsummary :current :summary)
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
                                         (case mode
                                           (:failsummary
                                            (when m-r
                                              (fmt "<br/>Failure summary:<br/>")
                                              (emit-master-run stream m-r :errors 0 t)))
                                           (:current
                                            (when first-run
                                              (print-legend stream)
                                              (emit-master-run stream first-run :full 0 t)))
                                           (:summary
                                            (when *global-condition*
                                              (fmt "<br/>There was a global condition.<br/>"))
                                            (cond (first-run
                                                   (print-legend stream)
                                                   (if (period-ended-p first-run)
                                                       (emit-master-run stream first-run :full 0 t)
                                                       (fmt "<br/>There is a buildmaster run ~A.<br/>"
                                                            (uri '(:current) "in-progress")))
                                                   (iter (for run in rest-runs)
                                                         (for nr from 1)
                                                         (emit-master-run stream run :full nr)))
                                                  (t
                                                   (htm :br
                                                        (:div :class "no-runs"
                                                              "There have been no buildmaster runs to speak of."))))))))))))))
              (when condition
                (setf *global-condition* (list condition backtrace)))
              return-value)))))))

(defun render-cl-waterfall-static (base)
  (let ((*emitting-static* t)
        (*static-output-base-pathname* base))
    (clrhash *completed-static-emissions*)
    (render-cl-waterfall)))

(defun start-cl-waterfall (&optional (prefix (string-right-trim '(#\/) (flatten-path-list *uri-base* :absolute t))))
  (push (create-regex-dispatcher prefix 'render-cl-waterfall) *dispatch-table*))

(setf *style*
  "<!--
.body {
  overflow: scroll;
  margin-bottom: 15em;
}
.run {
  min-width: 10000px;
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
  bottom: 3em;
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
