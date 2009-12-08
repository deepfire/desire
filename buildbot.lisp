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

(defun cook-buildslave-command (expr &key purge purge-metastore (branch "master") metastore-branch (debug t) disable-debugger verbose)
  (remove nil (list
               (when purge
                 *purge-command*)
               (when purge-metastore
                 *purge-metastore-command*)
               *update-bootstrapper-command*
               (format nil "bash climb.sh ~:[~;-v ~]~
                                          ~:[~;-b ~:*~(~A~) ~] ~:[~;-t ~:*~(~A~) ~]~
                                          ~:[~;-d ~]~:[~;-g ~]~
                                          -x ~S ~
                                          ~~/desr"
                       verbose
                       branch metastore-branch
                       debug disable-debugger
                       (format nil "~S" expr)))))

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
(defclass master-recurse-phase (local-test-phase) ()
  (:default-initargs :action-description "unwind module dependencies"))
(defclass slave-fetch-phase (remote-test-phase) ()
  (:default-initargs :action-description "fetch modules from wishmaster"))
(defclass slave-recurse-phase (remote-test-phase) ()
  (:default-initargs :action-description "unwind module dependencies"))
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

(defun master-result (master-run i)
  (declare (type buildmaster-run master-run))
  (aref (master-run-results master-run) i))

(defun first-phase-result-p (master-run result)
  (< (result-id result) (master-run-n-phase-results master-run)))

(defun previous-phase-result (master-run result)
  (master-result master-run (- (result-id result) (master-run-n-phase-results master-run))))

(defun previous-phase-result-success-p (master-run result)
  (typep (previous-phase-result master-run result) 'success))

(defgeneric advance-result (master-run &optional ranp successp condition backtrace)
  (:method ((o buildmaster-run) &optional ranp successp condition backtrace)
    (let* ((result-vector (master-run-results o))
           (i (fill-pointer result-vector)))
      (with-master-run-lock (o)
        (when-let ((completed-r (when (plusp i)
                                  (aref result-vector (1- i)))))
          (when ranp
            (if successp
                (succeed-action completed-r)
                (fail-action completed-r :condition condition :backtrace backtrace))))
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
                                         master-recurse-phase
                                         slave-fetch-phase
                                         slave-recurse-phase
                                         slave-load-phase
                                         ;; slave-test-phase
                                         ))

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
          (buildmaster-error "~@<Early termination from slave: premature end while processing ~A.~:@>" description))
        (while (and (zerop (length line)) slurp-empty-lines))
        (finally (return line))))

(defgeneric execute-test-phase (buildmaster-run test-phase result-marker &key verbose)
  (:method :around ((m-r buildmaster-run) (p master-recurse-phase) current-result &key verbose)
    (declare (ignore verbose))
    (when (unsaved-definition-changes-p)
      (save-definitions :seal t :commit-message (format nil "Saved changes in DEFINITIONS before ~A." (type-of p))))
    (call-next-method)
    (when (unsaved-definition-changes-p)
      (save-definitions :seal t :commit-message (format nil "Saved changes in DEFINITIONS after ~A." (type-of p)))))
  (:method :around ((m-r buildmaster-run) (p local-test-phase) result-marker &key verbose)
    (declare (ignore verbose))
    (with-active-phase (p)
      (iter (with r = result-marker)
            (repeat (master-run-n-phase-results m-r))
            (for m = (result-module r))
            (with-tracked-termination (r)
              (destructuring-bind (&key return-value output condition backtrace) (call-next-method m-r p r)
                (append-result-output r output t)
                (setf r (advance-result m-r t return-value condition backtrace))))
            (finally (return r)))))
  (:method ((m-r buildmaster-run) (p master-reachability-phase) current-result &key verbose)
    (declare (ignore verbose))
    (run-module-test :master-reachability-phase (result-module current-result) nil t))
  (:method ((m-r buildmaster-run) (p master-update-phase) current-result &key verbose)
    (declare (ignore verbose))
    (run-module-test :master-update-phase (result-module current-result) nil t))
  (:method ((m-r buildmaster-run) (p master-recurse-phase) current-result &key verbose)
    (declare (ignore verbose))
    (run-module-test :master-recurse-phase (result-module current-result) nil t))
  (:method ((m-r buildmaster-run) (p remote-test-phase) result-marker &key verbose)
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
                        (when verbose
                          (report-line 0 initial-line))
                        (unless (eq name (name m))
                          (buildmaster-error "~@<Wrong module info from slave, next in turn was ~S, module returned ~S.~:@>" (name m) name))
                        (unless (eq mode (make-keyword (symbol-name (type-of p))))
                          (buildmaster-error "~@<Wrong phase for module ~A from slave: current phase ~S, client sent ~S~:@>" (name m) :fetch mode))
                        (format t "==( processing module ~A, phase ~A~%" name mode)
                        (let ((marker-line (read-mandatory-line pipe (name m))))
                          (unless (line-marker-p marker-line *buildslave-remote-test-output-marker*)
                            (buildmaster-error "~@<Missing test output marker for module ~A.~:@>" (name m)))
                          (when verbose
                            (report-line 1 marker-line)))
                        (iter (for line = (read-mandatory-line pipe (name m) nil))
                              (for i from 2)
                              (when verbose
                                (report-line i line))
                              (when (line-marker-p line *buildslave-remote-end-of-test-output-marker*)
                                (return))
                              (append-result-output r line))
                        (let* ((eof 'eof-marker)
                               (*read-eval* nil)
                               (final-args (iter (repeat 6)
                                                 (collect (read pipe nil eof)))))
                          (when (member eof final-args)
                            (buildmaster-error "~@<Early termination from slave while reading module ~A.~:@>" (name m)))
                          (let ((final-line (read-mandatory-line pipe (name m))))
                            (when verbose
                              (report-line -1 final-line))
                            (unless (and (= 1 (length final-line))
                                         (char= #\) (schar final-line 0)))
                              (buildmaster-error "~@<Corrupt final line while reading module ~A.~:@>" (name m))))
                          (destructuring-bind (&key status condition backtrace) final-args
                            (setf r (advance-result m-r t status condition backtrace)))))
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
                   (format t "==( found remote error signature on line ~D~%" i)
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
           (let ((final-line (read-mandatory-line pipe "the final line")))
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
(defun make-buildslave-evaluation-form (&rest body)
  `(progn
     (with-slave-output-markers ()
       ,@body)
     (sb-ext:quit)))

(defun invoke-with-slave-evaluation (local-fn slave-form hostname username &rest keys &key print-slave-connection-conditions verbose-slave-communication &allow-other-keys)
  (handler-case
      (with-maybe-just-printing-conditions (t buildslave-error) print-slave-connection-conditions
        (with-slave-connection (slave-pipe hostname username (apply #'cook-buildslave-command (make-buildslave-evaluation-form slave-form)
                                                                    (remove-from-plist keys :print-slave-connection-conditions :verbose-slave-communication))
                                           verbose-slave-communication)
          (funcall local-fn slave-pipe)))
    (buildslave-error (c)
      (return-from invoke-with-slave-evaluation (values nil c)))))

(defmacro with-slave-evaluation (slave-form (slave-pipe hostname username &rest keys &key &allow-other-keys) &body body)
  `(invoke-with-slave-evaluation (lambda (,slave-pipe) ,@body) ,slave-form ,hostname ,username ,@keys))

(defun ping-slave (&rest keys &key (hostname *default-buildslave-host*) (username *default-buildslave-username*) &allow-other-keys)
  (apply #'invoke-with-slave-evaluation (lambda (pipe)
                                          (declare (ignore pipe))
                                          t)
         nil hostname username (remove-from-plist keys :hostname :username)))

(defun one* (&optional (reachability t) (upstream t) (recurse t) (slave-fetch t) (slave-recurse t) (slave-load t) (slave-test nil) &key modules purge (debug t) disable-debugger (verbose t) verbose-slave-communication)
  (one :phases (append (when reachability '(master-reachability-phase))
                       (when upstream '(master-update-phase))
                       (when recurse '(master-recurse-phase))
                       (when slave-fetch '(slave-fetch-phase))
                       (when slave-recurse '(slave-recurse-phase))
                       (when slave-load '(slave-load-phase))
                       (when slave-test '(slave-test-phase)))
       :modules modules
       :purge purge
       :debug debug
       :disable-debugger disable-debugger
       :verbose verbose
       :verbose-slave-communication verbose-slave-communication))

(defun one (&key (hostname *default-buildslave-host*) (username *default-buildslave-username*) (phases *buildmaster-run-phases*) modules
            purge branch metastore-branch debug disable-debugger (verbose t) verbose-slave-communication)
  (find-executable 'ssh)
  (let* ((gate (gate *self*))
         (module-names (or modules
                           (sort (copy-list (append (location-module-names gate) (gate-converted-module-names gate)))
                                 #'string<)))
         (modules (mapcar #'module module-names))
         (m-r (make-instance 'buildmaster-run :locality gate :phases phases :modules modules)))
    (with-tracked-termination (m-r)
      (let ((rest-phases (master-run-phases m-r))
            (result-marker (advance-result m-r)))
        (push m-r *buildmaster-runs*)
        (iter (for (phase . phases) on rest-phases)
              (while (typep phase 'local-test-phase))
              (setf result-marker (execute-test-phase m-r phase result-marker :verbose verbose-slave-communication)
                    rest-phases phases))
        (with-slave-evaluation `(buildslave ',module-names ',(mapcar #'type-of rest-phases) ,verbose)
            (slave-pipe hostname username :purge purge :branch branch :metastore-branch metastore-branch :debug debug :disable-debugger disable-debugger
                        :verbose verbose :verbose-slave-communication verbose-slave-communication)
          (iter (for (phase . phases) on rest-phases)
                (setf (remote-phase-slave-stream phase) slave-pipe
                      result-marker (execute-test-phase m-r phase result-marker :verbose verbose-slave-communication))))))))
