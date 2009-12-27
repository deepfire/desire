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

(defgeneric emit-result-hint (phase result)
  (:method :around ((o test-phase) (r result))
    (let* ((*print-escape* nil)
           (*print-length* nil)
           (*print-circle* t)
           (*print-level* nil))
      (if (and (result-hint-cache r)
               (or (result-hint-cache-final-p r)
                   (typep r 'incomplete-action)))
          (result-hint-cache r)
          (setf (result-hint-cache-final-p r) (typep r 'complete-action)
                (result-hint-cache r)
                (let ((condition-printed-form (when-let ((condition (action-condition r)))
                                                (escape-string (princ-to-string condition)))))
                  (with-output-to-string (s)
                    (write-string "<pre>Module:    " s) (write-string (symbol-name (name (result-module r))) s) (write-string "</pre><br>" s)
                    (write-string (call-next-method) s)
                    (when (and condition-printed-form
                               (> *popup-display-nlines-threshold* (count #\Newline condition-printed-form)))
                      (write-string "<br><br>encountered condition:<br><pre>" s)
                      (write-string condition-printed-form s)
                      (write-string "</pre>" s))))))))
  (:method ((o test-phase) (r result))
    "")
  (:method ((o master-update-phase) (r result))
    (if-let ((commit (slot-value* r 'commit nil)))
      (concatenate 'string
                   "<pre>Commit-ID: " (desr::cook-refval (desr::commit-id commit)) "</pre><br>"
                   "<pre>Author:    " (desr::commit-author commit) "</pre><br>"
                   "<pre>Date:      " (desr::commit-date commit) "</pre><br>")
      "")))

(defun invalidate-result-hint-cache (&optional (buildmaster-run (first *buildmaster-runs*)))
  (iter (for r in-vector (master-run-results buildmaster-run))
        (setf (result-hint-cache r) nil)))

(defgeneric emit-with-result-emission (stream result fn)
  (:method ((stream stream) (o result) (fn function))
    (with-html-output (stream)
      (:div :class "result cell"
            (funcall fn)
            (let ((condition (action-condition o)))
              (htm
               (:div :class "nhint" (str (emit-result-hint (result-phase o) o)))
               (:div :class (web-class o)
                     (str (web-marker o))
                     :br
                     (unless (typep o 'result-not-yet)
                       (str (plain-uri `(:out ,*master-run-nr* ,(result-id o)) "out")))
                     (when condition
                       (htm
                        :br
                        (str (plain-uri `(:cond ,*master-run-nr* ,(result-id o)) "co")))))))))))

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