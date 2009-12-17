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

;;;;
;;;; Generator/presentation communication
;;;;
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
                 (start-period new-r)))
              (t
               (end-period o)
               nil))))))

(defgeneric grab-result (master i)
  (:method ((o buildmaster-run) (i integer))
    (with-master-run-lock (o)
      (lret ((r (master-result o i)))
        (unless (< i (master-run-n-complete-results o))
          ;; let master wake us once the result is done
          (bordeaux-threads:condition-wait (master-run-condvar o) (master-run-lock o)))
        r))))

;;;;
;;;; Phase management
;;;;
(defparameter *buildmaster-run-phases* '(master-reachability-phase
                                         master-update-phase
                                         master-discovery-phase
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

(defun read-string-list (string &optional (start 0) end)
  (let ((end (or end (length string)))
        (*read-eval* nil))
    (with-input-from-string (s string :start start :end end)
      (iter (for elt = (read s nil 'eof))
            (while (not (eq elt 'eof)))
            (collect elt)))))

(defun report-line (i line)
  (format t "~4D> ~S~%" i line))

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
  (:documentation
   "Execute a buildmaster test phase.")
  (:method :around ((m-r buildmaster-run) (p master-recurse-phase) current-result &key &allow-other-keys)
    (when (unsaved-definition-changes-p)
      (save-definitions :seal t :commit-message (format nil "Saved changes in DEFINITIONS before ~A." (type-of p))))
    (multiple-value-prog1  (call-next-method)
      (when (unsaved-definition-changes-p)
        (save-definitions :seal t :commit-message (format nil "Saved changes in DEFINITIONS after ~A." (type-of p))))))
  (:method :around ((m-r buildmaster-run) (p local-test-phase) result-marker &key &allow-other-keys)
    (with-active-phase (p)
      (iter (with r = result-marker)
            (repeat (master-run-n-phase-results m-r))
            (for m = (result-module r))
            (with-tracked-termination (r)
              (destructuring-bind (&key return-value output condition backtrace) (call-next-method m-r p r)
                (append-result-output r output t)
                (setf r (advance-result m-r t return-value condition backtrace))))
            (finally (return r)))))
  (:method ((m-r buildmaster-run) (p master-reachability-phase) current-result &key &allow-other-keys)
    (run-module-test :master-reachability-phase (result-module current-result) nil t))
  (:method ((m-r buildmaster-run) (p master-update-phase) current-result &key &allow-other-keys)
    ;; XXX: need to ensure that the working directory is up to date (the default) -- that we drive masters, etc.
    (prog1 (run-module-test :master-update-phase (result-module current-result) nil t)
      (setf (result-commit current-result) (desr::git-commit-log '("tracker") (result-path current-result)))))
  (:method ((m-r buildmaster-run) (p master-discovery-phase) current-result &key &allow-other-keys)
    (run-module-test :master-discovery-phase (result-module current-result) nil t))
  (:method ((m-r buildmaster-run) (p master-recurse-phase) current-result &key &allow-other-keys)
    (run-module-test :master-recurse-phase (name (result-module current-result)) nil t))
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
                          ;; Advance the result.
                          (destructuring-bind (&key status condition backtrace) final-args
                            (setf r (advance-result m-r t status condition backtrace)))))
                      (buildmaster-error "~@<Corrupt initial line while reading module ~A.~:@>" (name m)))))
              (finally (return r)))))))

;;;;
;;;; Slave connection
;;;;
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
      (multiple-value-prog1 (funcall fn pipe)
        (finalise-slave-connection pipe)))))

(defmacro with-slave-connection ((pipe hostname username setup-commands &optional verbose) &body body)
  `(invoke-with-slave-connection ,hostname ,username ,setup-commands ,verbose (lambda (,pipe) ,@body)))

(defun make-buildslave-evaluation-form (&rest body)
  `(progn
     (with-slave-output-markers ()
       ,@body)
     (sb-ext:quit)))

(defun invoke-with-slave-evaluation (slave-form local-fn hostname username &rest keys &key print-slave-connection-conditions verbose-slave-communication &allow-other-keys)
  (handler-case
      (with-maybe-just-printing-conditions (t buildslave-error) print-slave-connection-conditions
        (let ((slave-form (make-buildslave-evaluation-form slave-form)))
          (when verbose-slave-communication
            (format t "~@<;;; ~@;Evaluating on buildslave: ~S~:@>~%" slave-form))
          (with-slave-connection (slave-pipe hostname username (apply #'cook-buildslave-command slave-form
                                                                      (remove-from-plist keys :print-slave-connection-conditions :verbose-slave-communication))
                                             verbose-slave-communication)
            (funcall local-fn slave-pipe))))
    (buildslave-error (c)
      (return-from invoke-with-slave-evaluation (values nil c)))))

(defmacro with-slave-evaluation (slave-form (slave-pipe hostname username &rest keys &key &allow-other-keys) &body body)
  `(invoke-with-slave-evaluation ,slave-form (lambda (,slave-pipe) ,@body) ,hostname ,username ,@keys))

;;;;
;;;; Buildmaster entry points
;;;;
(defun ping-slave (&rest keys &key call-buildslave (hostname *default-buildslave-host*) (username *default-buildslave-username*) &allow-other-keys)
  (apply #'invoke-with-slave-evaluation (cond
                                          (call-buildslave
                                           `(buildslave nil `(:slave-fetch-phase) t)))
         (lambda (pipe)
           (declare (ignore pipe))
           t)
         hostname username :verbose-slave-communication (getf keys :verbose-slave-communication call-buildslave)
         (remove-from-plist keys :call-buildslave :hostname :username :verbose-slave-communication)))

(defun one* (&optional (reachability t) (upstream t) (discover t) (recurse t) (slave-fetch t) (slave-recurse t) (slave-load t) (slave-test nil) &key modules purge (debug t) disable-debugger (verbose t) verbose-slave-communication)
  (one :phases (append (when reachability '(master-reachability-phase))
                       (when upstream '(master-update-phase))
                       (when discover '(master-discovery-phase))
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
        (when rest-phases
          (handler-case
              (with-slave-evaluation `(buildslave ',(mapcar #'make-keyword module-names) ',(mapcar (compose #'make-keyword #'type-of) rest-phases) ,verbose)
                  (slave-pipe hostname username :purge purge :branch branch :metastore-branch metastore-branch :debug debug :disable-debugger disable-debugger
                              :verbose verbose :verbose-slave-communication verbose-slave-communication)
                (iter (for (phase . phases) on rest-phases)
                      (setf (remote-phase-slave-stream phase) slave-pipe
                            result-marker (execute-test-phase m-r phase result-marker :verbose verbose-slave-communication))))
            (buildslave-error (c)
              (terminate-action result-marker :condition c)
              (error c))))))
    (end-period m-r)))
