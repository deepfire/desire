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


(defvar *default-buildslave-host* "betelheise")
(defvar *default-buildslave-username* "empty")
(defvar *default-buildslave-master* :git.feelingofgreen.ru)

(defparameter *bootstrap-script-location* "http://www.feelingofgreen.ru/shared/git/desire/climb.sh")
(defparameter *purge-command* "rm -rf desr")
(defparameter *purge-metastore-command* "rm -rf desr/git/.meta")
(defparameter *update-bootstrapper-command* (format nil "wget ~A -O climb.sh" *bootstrap-script-location*))

(defun cook-buildslave-command (module-names phase-names purge purge-metastore branch metastore-branch disable-debugger verbose)
  (remove nil (list
               (when purge
                 *purge-command*)
               (when purge-metastore
                 *purge-metastore-command*)
               *update-bootstrapper-command*
               (format nil "bash climb.sh ~:[~;-v ~]~
                                          ~:[~;-b ~:*~(~A~) ~] ~:[~;-t ~:*~(~A~) ~]~
                                          ~:[~;-g ~]~
                                          -x \"(progn (desr:ensure-module-systems-loadable :desire) ~
                                                      (require :desire) ~
                                                      (funcall (find-symbol \\\"BUILDSLAVE\\\" :desire) '~A '~A) ~
                                                      (funcall (find-symbol \\\"QUIT\\\" :sb-ext)))\" ~
                                          ~~/desr"
                       verbose
                       branch metastore-branch
                       disable-debugger
                       module-names phase-names))))

;;;
;;; Result output vector management
;;;
(defconstant header-leeway 32)
(defconstant page-size (virtual-memory-page-size))
(defconstant initial-output-length (- page-size header-leeway))
(defconstant output-size-scale-factor 4)

(defun prepare-result-vector (n-phases n-phase-results modules locality)
  (lret ((results (make-array (* n-phases n-phase-results) :fill-pointer t)))
    (iter (with pathnames = (mapcar (rcurry #'module-pathname locality) modules))
          (for phase-no below n-phases)
          (for base from 0 by n-phase-results)
          (iter (for i below n-phase-results)
                (for m in modules)
                (for p in pathnames)
                (setf (aref results (+ base i))
                      (make-instance 'result-not-yet :module m :path p :id (+ base i) :output (make-array initial-output-length :element-type 'character :adjustable t)))))
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
              (let ((fitting-length (ash 1 (+ (integer-length required-length)                     ; Quadruple, while we're going for speed and
                                              (if (> (integer-length required-length) 20) 0 1))))) ; double when we've got to resort to compactness.
                (setf rvec (adjust-array rvec fitting-length)
                      (result-output o) rvec)))
            (setf (subseq rvec used-bytes (1- required-length)) string
                  (aref rvec (1- required-length)) #\Newline
                  (result-output-bytes o) required-length))))))

;;;
;;; Test phase
;;;
(defclass test-phase (action) 
  ((action-description :reader phase-action-description :initarg :action-description)
   (nr :reader phase-nr :initarg :nr)))

(defclass interrupted-test-phase (unhandled-failure test-phase) ())

(define-action-root-class local-test-phase (test-phase)
  ()
  (:subclass-to-action-class-map
   (unhandled-failure interrupted-local-test-phase)))
(define-action-root-class remote-test-phase (test-phase)
  ((hostname :reader remote-phase-hostname :initarg :hostname)
   (username :reader remote-phase-username :initarg :username)
   (slave-stream :accessor remote-phase-slave-stream))
  (:subclass-to-action-class-map
   (unhandled-failure interrupted-remote-test-phase)))

(defclass interrupted-local-test-phase (interrupted-test-phase local-test-phase) ())
(defclass interrupted-remote-test-phase (interrupted-test-phase remote-test-phase) ())

(defclass master-reachability-phase (local-test-phase) ()
  (:default-initargs :action-description "test upstream repository reachability"))
(defclass master-update-phase (local-test-phase) ()
  (:default-initargs :action-description "fetch upstream modules and convert them"))
(defclass slave-fetch-phase (remote-test-phase) ()
  (:default-initargs :action-description "fetch modules from wishmaster"))
(defclass slave-load-phase (remote-test-phase) ()
  (:default-initargs :action-description "load modules"))
(defclass slave-test-phase (remote-test-phase) ()
  (:default-initargs :action-description "test modules"))

(defgeneric describe-phase (test-phase)
  (:method ((o test-phase))
    (format nil "~A on ~:[the wishmaster~;a buildslave~]; ~:[not started~; started ~:*~A~]~:[~;, finished ~:*~A~]"
            (phase-action-description o) (typep o 'remote-test-phase)
            (when (period-started-p o)
              (multiple-value-call #'print-decoded-time
                (decode-universal-time (period-start-time o))))
            (when (period-ended-p o)
              (multiple-value-call #'print-decoded-time
                (decode-universal-time (period-end-time o)))))))

;;;
;;; Buildmaster run
;;;
(define-action-root-class buildmaster-run (action)
  ((lock :reader master-run-lock :initarg :lock)
   (condvar :reader master-run-condvar :initarg :condvar)
   (modules :reader master-run-modules :initarg :modules)
   (phases :reader master-run-phases :initarg :phases)
   (n-phases :reader master-run-n-phases :initarg :n-phases)
   (n-phase-results :reader master-run-n-phase-results :initarg :n-phase-results)
   (results :reader master-run-results :initarg :results))
  (:subclass-to-action-class-map
   (unhandled-failure interrupted-buildmaster-run))
  (:default-initargs
   :lock (bordeaux-threads:make-lock)
   :condvar (bordeaux-threads:make-condition-variable)
   :start-time (get-universal-time)
   :phases '(master-update-phase
             slave-fetch-phase
             slave-load-phase)))

(defclass interrupted-buildmaster-run (unhandled-failure buildmaster-run) ())

(defmethod terminate-action :after ((o buildmaster-run) &key condition &allow-other-keys)
  (declare (ignore condition))
  (let ((result-vector (master-run-results o)))
    (setf (fill-pointer result-vector) (array-dimension result-vector 0))))

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
                                       (for phase-type in phases)
                                       (collect (make-instance phase-type :nr i)))
          (slot-value o 'n-phases) n-phases
          (slot-value o 'results) (prepare-result-vector n-phases n-phase-results modules locality)
          (slot-value o 'n-phase-results) n-phase-results)))

;; must be called under master run lock
(defgeneric finalise-result (result successp condition)
  (:method ((o result) successp condition)
    (if successp
        (succeed-action o)
        (fail-action o :condition condition))))

(defun master-result (master-run i)
  (declare (type buildmaster-run master-run))
  (aref (master-run-results master-run) i))

(defgeneric advance-result (master-run &optional successp condition)
  (:method ((o buildmaster-run) &optional successp condition)
    (let* ((result-vector (master-run-results o))
           (i (fill-pointer result-vector)))
      (with-master-run-lock (o)
        (when-let ((completed-r (when (plusp i)
                                  (aref result-vector (1- i)))))
          (finalise-result completed-r successp condition))
        (bordeaux-threads:condition-notify (master-run-condvar o))
        (cond ((< i (* (master-run-n-phase-results o)
                       (master-run-n-phases o)))
               (incf (fill-pointer result-vector))
               (lret ((new-r (aref result-vector i)))
                 (start-period new-r)
                 (setf (result-commit-id new-r) (desr::ref-value '("tracker") (result-path new-r)))))
              (t
               (end-period o)))))))

(defgeneric grab-result (master i)
  (:method ((o buildmaster-run) (i integer))
    (with-master-run-lock (o)
      (lret ((r (master-result o i)))
        (unless (< i (master-run-n-complete-results o))
          ;; let master wake us once the result is done
          (bordeaux-threads:condition-wait (master-run-condvar o) (master-run-lock o)))
        r))))

(defparameter *buildmaster-run-phases* '(master-reachability-phase
                                         master-update-phase
                                         slave-fetch-phase
                                         slave-load-phase))

(defgeneric invoke-with-active-phase (phase fn)
  (:method ((o test-phase) (fn function))
    (start-period o)
    (prog1 (with-tracked-termination (o)
             (funcall fn))
      (end-period o))))

(defmacro with-active-phase ((phase &optional phase-form) &body body)
  (if phase-form
      `(let ((,phase ,phase-form))
         (invoke-with-active-phase ,phase (lambda () ,@body)))
      `(invoke-with-active-phase ,phase (lambda () ,@body))))

;;;
;;; Remote phase shenanigans
;;;
(defun read-string-list (string &optional (start 0) end)
  (let ((end (or end (length string)))
        (*read-eval* nil))
    (with-input-from-string (s string :start start :end end)
      (iter (for elt = (read s nil 'eof))
            (while (not (eq elt 'eof)))
            (collect elt)))))

(defun report-line (i line)
  (format t "~3D> ~S~%" i line))

(defun line-marker-p (line marker)
  (and (plusp (length line))
       (char= #\: (schar line 0))
       (string= line (symbol-name marker) :start1 1)))

(defun read-mandatory-line (stream description &optional (slurp-empty-lines t))
  (iter (for line = (read-line stream nil nil))
        (unless line
          (buildmaster-error "~@<Early termination from slave: premature end while ~A.~:@>" description))
        (while (and (zerop (length line)) slurp-empty-lines))
        (finally (return line))))
             
(defgeneric execute-test-phase (buildmaster-run test-phase result-marker)
  (:method :around ((m-r buildmaster-run) (p local-test-phase) result-marker)
    (with-active-phase (p)
      (iter (with r = result-marker)
            (repeat (master-run-n-phase-results m-r))
            (for m = (result-module r))
            (with-tracked-termination (r)
              (destructuring-bind (&key return-value output condition) (call-next-method m-r p r)
                (append-result-output r output t)
                (setf r (advance-result m-r return-value condition))))
            (finally (return r)))))
  (:method ((m-r buildmaster-run) (p master-reachability-phase) current-result)
    (module-test-reachability (result-module current-result) :capture-output t))
  (:method ((m-r buildmaster-run) (p master-update-phase) current-result)
    (module-test-fetchability (result-module current-result) :capture-output t))
  (:method ((m-r buildmaster-run) (p remote-test-phase) result-marker)
    (with-active-phase (p)
      (let ((*read-eval* nil)
            (*package* (find-package :desire))
            (pipe (remote-phase-slave-stream p)))
        (iter (with r = result-marker)
              (repeat (master-run-n-phase-results m-r))
              (for m = (result-module r))
              (with-tracked-termination (r)
                (let ((initial-line (read-mandatory-line pipe (name m))))
                  (if (char= #\( (schar initial-line 0))
                      (destructuring-bind (&key name mode) (read-string-list initial-line 1)
                        (report-line 0 initial-line)
                        (unless (eq name (name m))
                          (buildmaster-error "~@<Wrong module info from slave, next in turn was ~S, module returned ~S.~:@>" (name m) name))
                        (unless (eq mode (etypecase p
                                           (slave-fetch-phase :fetch)
                                           (slave-load-phase :load)))
                          (buildmaster-error "~@<Wrong phase for module ~A from slave: current phase ~S, client sent ~S~:@>" (name m) :fetch mode))
                        (format t "==( processing module ~A, phase ~A~%" name mode)
                        (let ((marker-line (read-mandatory-line pipe (name m))))
                          (unless (line-marker-p marker-line *buildslave-remote-test-output-marker*)
                            (buildmaster-error "~@<Missing test output marker for module ~A.~:@>" (name m)))
                          (report-line 1 marker-line))
                        (iter (for line = (read-mandatory-line pipe (name m) nil))
                              (for i from 2)
                              (report-line i line)
                              (when (line-marker-p line *buildslave-remote-end-of-test-output-marker*)
                                (return))
                              (append-result-output r line))
                        (let* ((eof 'eof-marker)
                               (*read-eval* nil)
                               (e0 (read pipe nil eof))
                               (e1 (read pipe nil eof))
                               (e2 (read pipe nil eof))
                               (e3 (read pipe nil eof))
                               (final-args (list e0 e1 e2 e3)))
                          (when (member eof final-args)
                            (buildmaster-error "~@<Early termination from slave while reading module ~A.~:@>" (name m)))
                          (let ((final-line (read-mandatory-line pipe (name m))))
                            (unless (and (= 1 (length final-line))
                                         (char= #\) (schar final-line 0)))
                              (buildmaster-error "~@<Corrupt final line while reading module ~A.~:@>" (name m))))
                          (destructuring-bind (&key status condition) final-args
                            (setf r (advance-result m-r status condition)))))
                      (buildmaster-error "~@<Corrupt initial line while reading module ~A.~:@>" (name m)))))
              (finally (return r)))))))

;;;
;;; Slave connection
;;;
(defvar *implementation-debugger-signature*
  #+sbcl "debugger invoked on a")
(defvar *implementation-unhandled-condition-signature*
  #+sbcl "unhandled")
(defun invoke-with-slave-connection (hostname username slave-setup-commands verbose fn)
  (flet ((setup-slave-connection (pipe hostname username commands)
           (with-asynchronous-execution
             (with-input-from-string (stream (compile-shell-command commands))
               (with-executable-input-stream stream
                 (let ((*executable-standard-output-direction* pipe))
                   (ssh `(,username "@" ,hostname) "bash" "-s")))))
           (close (two-way-stream-output-stream pipe))
           (iter (for line = (read-line pipe nil nil))
                 (unless line
                   (buildmaster-error "~@<Early termination from slave: no output marker found.~:@>"))
                 (when verbose
                   (report-line i line))
                 (when (or (starts-with-subseq *implementation-debugger-signature* line)
                           (starts-with-subseq *implementation-unhandled-condition-signature* line))
                   (let ((error-message (apply #'concatenate 'string line #(#\Newline)
                                               (iter (for line = (read-line pipe nil nil))
                                                     (while line)
                                                     (collect line)
                                                     (collect #(#\Newline))))))
                     (error 'buildslave-error :output error-message)))
                 (for i from 0)
                 (when (line-marker-p line *buildslave-remote-output-marker*)
                   (report-line i line)
                   (format t "==( found remote output beginning marker on line ~D~%" i)
                   (return))))
         (finalise-slave-connection (pipe)
           (let ((final-line (read-mandatory-line pipe "looking for final line")))
             (report-line -1 final-line)
             (unless (line-marker-p final-line *buildslave-remote-end-of-output-marker*)
               (buildmaster-error "~@<Early termination from slave: remote end-of-output marker missing.~:@>")))))
    (with-pipe-stream (pipe :element-type 'character :buffering :none)
      (setup-slave-connection pipe hostname username slave-setup-commands)
      (funcall fn pipe)
      (finalise-slave-connection pipe))))

(defmacro with-slave-connection ((pipe hostname username setup-commands &optional verbose) &body body)
  `(invoke-with-slave-connection ,hostname ,username ,setup-commands ,verbose (lambda (,pipe) ,@body)))

;;;
;;; Buildmaster entry points
;;;
(defun ping-slave (&key (hostname *default-buildslave-host*) (username *default-buildslave-username*)
                   purge purge-metastore branch metastore-branch disable-debugger verbose
                   just-print-conditions)
  (handler-case
      (with-maybe-just-printing-conditions (t buildslave-error) just-print-conditions
        (with-slave-connection (slave-pipe hostname username
                                           (cook-buildslave-command nil nil purge
                                                                    purge-metastore branch
                                                                    metastore-branch
                                                                    disable-debugger verbose)
                                           verbose)
          (declare (ignore slave-pipe))
          t))
    (buildslave-error (c)
      (return-from ping-slave (values nil c)))))

(defun one* (&optional (reachability t) (upstream t) (slave-fetch t) (slave-load t) (slave-test nil))
  (buildmaster-one :phases (append (when reachability '(master-reachability-phase))
                                   (when upstream '(master-update-phase))
                                   (when slave-fetch '(slave-fetch-phase))
                                   (when slave-load '(slave-load-phase))
                                   (when slave-test '(slave-test-phase)))))

(defun one (&key (hostname *default-buildslave-host*) (username *default-buildslave-username*) (phases *buildmaster-run-phases*)
            purge purge-metastore branch metastore-branch disable-debugger verbose)
  (find-executable 'ssh)
  (let* ((gate (gate *self*))
         (module-names (sort (copy-list (append (location-module-names gate) (gate-converted-module-names gate)))
                             #'string<))
         (modules (mapcar #'module module-names))
         (m-r (make-instance 'buildmaster-run :locality gate :phases phases :modules modules)))
    (with-tracked-termination (m-r)
      (let ((rest-phases (master-run-phases m-r))
            (result-marker (advance-result m-r)))
        (push m-r *buildmaster-runs*)
        (iter (for (phase . phases) on rest-phases)
              (while (typep phase 'local-test-phase))
              (setf result-marker (execute-test-phase m-r phase result-marker)
                    rest-phases phases))
        (with-slave-connection (slave-pipe hostname username (cook-buildslave-command module-names (mapcar #'type-of rest-phases)
                                                                                      purge purge-metastore branch metastore-branch disable-debugger verbose)
                                verbose)
          (iter (for (phase . phases) on rest-phases)
                (setf (remote-phase-slave-stream phase) slave-pipe
                      result-marker (execute-test-phase m-r phase result-marker))))))))

;;;
;;; Presentation
;;;
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
             (when-let ((condition (action-condition r)))
               (format nil "~A" condition))))))
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
