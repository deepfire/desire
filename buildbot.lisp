;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE-BUILDBOT; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t; -*-
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


(defvar *default-master* :git.feelingofgreen.ru)
(defparameter *bootstrap-script-location* "http://www.feelingofgreen.ru/shared/git/desire/climb.sh")

(defhostaccess :buildslave        :hostname "betelheise" :username "empty" :password nil)
(defhostaccess :buildslave-empty  :hostname "betelheise" :username "emptier" :password nil)
(defhostaccess :buildmaster       :hostname "localhost" :username "buildmaster" :password nil)

;;;;
;;;; Generator/presentation communication and concurrence management.
;;;;
(defgeneric advance-result (master-run &optional ranp successp condition backtrace)
  (:method ((o buildmaster-run) &optional ranp successp condition backtrace)
    (let* ((result-vector (master-run-results o))
           (i (fill-pointer result-vector)))
      (with-master-run-lock (o)
        (when-let ((completed-r (when (plusp i)
                                  (aref result-vector (1- i)))))
          (if ranp
              (if successp
                  (succeed-action completed-r)
                  (fail-action completed-r :condition condition :backtrace backtrace))
              (period-unstart completed-r)))
        (bordeaux-threads:condition-notify (master-run-condvar o))
        (cond ((< i (* (master-run-n-phase-results o)
                       (master-run-n-phases o)))
               (incf (fill-pointer result-vector))
               (lret ((new-r (aref result-vector i)))
                 (start-period new-r)))
              (t
               (when ranp
                 (end-period o))
               nil))))))

(defmethod terminate-action :after ((o buildmaster-run) &key condition &allow-other-keys)
  (declare (ignore condition))
  (let ((result-vector (master-run-results o)))
    (setf (fill-pointer result-vector) (array-dimension result-vector 0))
    (bordeaux-threads:condition-notify (master-run-condvar o))))

(defgeneric grab-result (master i)
  (:method ((o buildmaster-run) (i integer))
    (with-master-run-lock (o)
      (lret ((r (master-result o i)))
        (unless (< i (master-run-n-complete-results o))
          ;; let master wake us once the result is done
          (bordeaux-threads:condition-wait (master-run-condvar o) (master-run-lock o)))
        r))))

;;;;
;;;; Communication
;;;;
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

(defstruct (remote-lisp-context (:conc-name ctx-) (:constructor %make-remote-lisp-context))
  credentials
  store-root
  purge-store
  purge-metastore
  desire-branch
  metastore-branch
  optimize-debug
  disable-debugger
  module
  system
  package
  form
  verbose
  verbose-comm
  ;;
  commands
  pipe
  process)

(defun make-remote-lisp-context (&key credentials
                                 (store-root "~/desr") purge-store purge-metastore
                                 (desire-branch :master) (metastore-branch :master)
                                 (optimize-debug t) (disable-debugger t)
                                 module system (package :desr) form
                                 (verbose t) verbose-comm)
  (unless credentials
    (error "~@<Credentials not specified while making a remote lisp context.~:@>"))
  (%make-remote-lisp-context
   :credentials (coerce-to-credentials credentials)
   :store-root store-root :purge-store purge-store :purge-metastore purge-metastore
   :desire-branch desire-branch :metastore-branch metastore-branch
   :optimize-debug optimize-debug :disable-debugger disable-debugger
   :module module :system system :package package :form form
   :verbose verbose :verbose-comm verbose-comm))

(defvar *shell-error-signature*
  "bash: line ")
(defvar *bootstrapper-error-signature*
  "ERROR: ")
(defvar *implementation-debugger-signature*
  #+sbcl "debugger invoked on a")
(defvar *implementation-unhandled-condition-signature*
  #+sbcl "unhandled")

(defun snoop-remote-errors (line nr stream error-type &rest error-initargs)
  (when-let ((type (cond ((or (starts-with-subseq *implementation-debugger-signature* line)
                              (starts-with-subseq *implementation-unhandled-condition-signature* line))
                          :lisp)
                         ((starts-with-subseq *bootstrapper-error-signature* line)
                          :bootstrapper)
                         ((starts-with-subseq *shell-error-signature* line)
                          :shell))))
    (format t "==( found remote ~(~A~) error signature on line ~D~%" type nr)
    (let ((error-message (apply #'concatenate 'string line #(#\Newline)
                                (iter (for line = (read-line stream nil nil))
                                      (while line)
                                      (collect line)
                                      (collect #(#\Newline))))))
      (apply #'error error-type :output error-message error-initargs))))

(defun read-mandatory-line (ctx description &optional (slurp-empty-lines t))
  (iter (for line = (read-line (ctx-pipe ctx) nil nil))
        (unless line
          (remote-lisp-communication-error ctx
           "~@<Early termination from remote lisp: premature end while processing ~A.~:@>"
           description))
        (snoop-remote-errors line 0 (ctx-pipe ctx) 'remote-lisp-communication-error :ctx ctx)
        (while (and (zerop (length line)) slurp-empty-lines))
        (finally (return line))))

(defgeneric execute-test-phase (buildmaster-run test-phase result-marker &key &allow-other-keys)
  (:documentation
   "Execute a buildmaster test phase.")
  (:method :around ((m-r buildmaster-run) (p master-recurse-phase) current-result &key &allow-other-keys)
    (when (unsaved-definition-changes-p)
      (save-definitions :seal t :commit-message (format nil "Saved changes in DEFINITIONS before ~A."
                                                        (type-of p))))
    (multiple-value-prog1  (call-next-method)
      (when (unsaved-definition-changes-p)
        (save-definitions :seal t :commit-message (format nil "Saved changes in DEFINITIONS after ~A."
                                                          (type-of p))))))
  (:method :around ((m-r buildmaster-run) (p local-test-phase) result-marker &key &allow-other-keys)
    (iter (with r = result-marker)
          (repeat (master-run-n-phase-results m-r))
          (for m = (result-module r))
          (for rdep = (result-dependency r))
          (if (or (not rdep)
                  (and (period-ended-p rdep)
                       (successp rdep)))
              (with-tracked-termination (r)
                (destructuring-bind (&key return-value output condition backtrace)
                    (call-next-method m-r p r)
                  (append-result-output r output t)
                  (setf r (advance-result m-r t return-value condition backtrace))))
              (setf r (advance-result m-r nil)))
          (finally (return r))))
  (:method ((m-r buildmaster-run) (p master-reachability-phase) current-result &key &allow-other-keys)
    (run-module-test :master-reachability-phase (result-module current-result) nil t))
  (:method ((m-r buildmaster-run) (p master-update-phase) current-result &key &allow-other-keys)
    ;; XXX: need to ensure that the working directory is up to date (the default) -- 
    ;; that we drive masters, etc.
    (prog1 (run-module-test :master-update-phase (result-module current-result) nil t)
      (setf (result-commit current-result) (desr::git-commit-log '("tracker")
                                                                 (result-path current-result)))))
  (:method ((m-r buildmaster-run) (p master-recurse-phase) current-result &key &allow-other-keys)
    (run-module-test :master-recurse-phase (name (result-module current-result)) nil t))
  (:method ((m-r buildmaster-run) (p remote-test-phase) result-marker
            &key verbose verbose-starting-module)
    (let ((*read-eval* nil)
          (*package* (find-package :desire))
          (ctx (remote-phase-ctx p)))
      (with-slots (pipe) ctx
        (let ((phase-header (read-mandatory-line ctx "phase header")))
          (when verbose
            (report-line 0 phase-header))
          (destructuring-bind (&key phase module-count) (let ((*read-eval* nil))
                                                          (read-from-string phase-header))
            (unless (string= phase (type-of p))
              (remote-lisp-communication-error ctx
               "~@<Remote reports wrong phase ~A, while expected ~A.~:@>" phase (type-of p)))
            (unless (= (phase-n-results p) module-count)
              (remote-lisp-communication-error ctx
               "~@<Remote reports wrong module count for phase ~A: got ~D, expected ~D.~:@>"
               phase module-count (phase-n-results p)))))
        (prog1
            (iter (with r = result-marker)
                  (repeat (master-run-n-phase-results m-r))
                  (for m = (result-module r))
                  (when (string= verbose-starting-module (name m))
                    (format t "==( enabling verbosity, starting from module ~A~%" (name m))
                    (setf verbose t))
                  (with-tracked-termination (r)
                    (let ((initial-line (read-mandatory-line ctx (name m))))
                      (when verbose
                        (report-line 0 initial-line))
                      (unless (char= #\( (schar initial-line 0))
                        (remote-lisp-communication-error ctx
                         "~@<Corrupt initial line while reading module ~A:~%~S~:@>"
                         (name m) initial-line))
                      (destructuring-bind (&key name mode) (read-string-list initial-line 1)
                        (unless (eq name (name m))
                          (remote-lisp-communication-error ctx
                           "~@<Wrong module info from remote, next in turn was ~S, ~
                               module returned ~S.~:@>"
                           (name m) name))
                        (unless (eq mode (make-keyword (symbol-name (type-of p))))
                          (remote-lisp-communication-error ctx
                           "~@<Wrong phase for module ~A from remote: ~
                               current phase ~S, client sent ~S~:@>"
                           (name m) :fetch mode))
                        (format t "==( processing module ~A, phase ~A~%" name mode)
                        (let ((marker-line (read-mandatory-line ctx (name m))))
                          (when verbose
                            (report-line 1 marker-line))
                          (unless (line-marker-p marker-line *beginning-of-result-marker*)
                            (remote-lisp-communication-error ctx
                             "~@<Missing test output marker for module ~A, got instead:~%~S~:@>"
                             (name m) marker-line)))
                        (iter (for line = (read-mandatory-line ctx (name m) nil))
                              (for i from 2)
                              (when verbose
                                (report-line i line))
                              (when (line-marker-p line *end-of-result-marker*)
                                (return))
                              (append-result-output r line))
                        (let* ((eof 'eof-marker)
                               (*read-eval* nil)
                               (final-args (iter (repeat 6)
                                                 (collect (read pipe nil eof)))))
                          (when (member eof final-args)
                            (remote-lisp-communication-error ctx
                             "~@<Early termination from remote while reading module ~A.~:@>"
                             (name m)))
                          (let ((final-line (read-mandatory-line ctx (name m))))
                            (when verbose
                              (report-line -1 final-line))
                            (unless (and (= 1 (length final-line))
                                         (char= #\) (schar final-line 0)))
                              (remote-lisp-communication-error ctx
                               "~@<Corrupt final line while reading module ~A:~%~S~:@>"
                               (name m) final-line)))
                          ;; Advance the result.
                          (destructuring-bind (&key status condition backtrace) final-args
                            (setf r (advance-result m-r t status condition backtrace)))))))
                  (finally (return r)))
          (let ((phase-tail (read-mandatory-line ctx "phase tail")))
            (when verbose
              (report-line -1 phase-tail))
            (destructuring-bind (&key phase-end) (let ((*read-eval* nil))
                                                   (read-from-string phase-tail))
              (unless (string= phase-end (type-of p))
                (remote-lisp-communication-error ctx
                 "~@<Remote reports end of wrong phase ~A, while expected ~A.~:@>" phase-end (type-of p))))))))))

;;;;
;;;; Remote lisp communication
;;;;
(defun make-marked-output-evaluation-form (&rest body)
  `(progn
     (with-output-markers ()
       ,@body)
     (sb-ext:quit)))

(defun compute-commands-for-remote-lisp-context (form ctx)
  (with-slots (store-root purge-store purge-metastore
               desire-branch metastore-branch
               module system package
               optimize-debug disable-debugger
               verbose) ctx
    (remove nil (list
                 (when purge-store
                   (format nil "rm -rf ~A"
                           store-root))
                 (when purge-metastore
                   (format nil "rm -rf ~A/git/.meta"
                           store-root))
                 (format nil "wget ~A -O climb.sh"
                         *bootstrap-script-location*)
                 (format nil "bash climb.sh ~:[~;-v ~]~
                                            ~:[~;-b ~:*~(~A~) ~] ~:[~;-t ~:*~(~A~) ~]~
                                            ~:[~;-d ~] ~:[~;-g ~]~
                                            ~:[~;-m ~:*~S ~]~
                                            ~:[~;-s ~:*~S ~]~
                                            ~:[~;-k ~:*~S ~]~
                                            -x ~S ~
                                            ~A"
                         verbose
                         desire-branch metastore-branch
                         optimize-debug disable-debugger
                         module system package
                         (format nil "~S" form)
                         store-root)))))

(defun initiate-remote-lisp-execution (ctx)
  (with-slots (credentials commands pipe process) ctx
    (setf pipe (make-pipe-stream :element-type 'character :buffering :none))
    (with-input-from-string (stream (compile-shell-command commands))
      (with-executable-input-stream stream
        (let ((*executable-standard-output-direction* pipe))
          (setf process (with-asynchronous-execution
                          (ssh `(,(cred-username credentials) "@" ,(cred-hostname credentials)) "bash" "-s"))))))
    (close (two-way-stream-output-stream pipe))))

(defconstant ssh-connection-error-exit-code 255)

(defun remote-lisp-connection-failed-p (ctx)
  (with-slots (process) ctx
    (and (not (process-alive-p process))
         (= ssh-connection-error-exit-code (process-exit-code process)))))

(defun wait-for-remote-lisp-output-marker (ctx)
  (with-slots (pipe process verbose-comm) ctx
    (iter (for line = (read-line pipe nil nil))
          (unless line
            (if (remote-lisp-connection-failed-p ctx)
                (remote-lisp-communication-error ctx "~@<Failed to connect remote lisp.~:@>")
                (remote-lisp-communication-error ctx "~@<Early termination from remote lisp: ~
                                                       beginning-of-output marker missing, ~
                                                       process exit status ~D.~:@>"
                                                 (process-exit-code (process-wait process)))))
          (for i from 0)
          (when verbose-comm
            (report-line i line))
          (snoop-remote-errors line i pipe 'remote-lisp-initialisation-error :ctx ctx)
          (until (line-marker-p line *beginning-of-output-marker*))
          (finally
           (format t "==( found remote output beginning marker on line ~D~%" i)))))

(defun wait-for-remote-lisp-end-of-output-marker (ctx)
  (with-slots (pipe verbose-comm) ctx
    (let ((final-line (read-mandatory-line ctx "the final line")))
      (when verbose-comm
        (report-line -1 final-line))
      (unless (line-marker-p final-line *end-of-output-marker*)
        (remote-lisp-communication-error ctx "~@<Early termination from remote lisp: ~
                                             end-of-output marker missing~:@>"))
      (format t "==( found end of remote output marker~%"))))

(defun invoke-with-remote-lisp-context (remote-form ctx fn &key manual-output &aux
                                        (remote-form
                                         (make-marked-output-evaluation-form remote-form)))
  (with-slots (credentials pipe form commands verbose-comm) ctx
    (when verbose-comm
      (format t "~@<;;; ~@;Evaluating in remote lisp on '~A' as '~A': ~S~:@>~%"
              (cred-hostname credentials) (cred-username credentials) remote-form))
    (setf form remote-form
          commands (compute-commands-for-remote-lisp-context remote-form ctx))
    (unwind-protect
         (progn (initiate-remote-lisp-execution ctx)
                (wait-for-remote-lisp-output-marker ctx)
                (multiple-value-prog1 (funcall fn ctx)
                  (unless manual-output
                    (wait-for-remote-lisp-end-of-output-marker ctx))))
      (when pipe
        (close (two-way-stream-input-stream pipe))
        (close (two-way-stream-output-stream pipe))))))

(defmacro with-remote-lisp-context ((ctx ctx-form &key manual-output) form &body body)
  (let ((ctx-var (or ctx (gensym))))
    `(invoke-with-remote-lisp-context ,form ,ctx-form
                                      (lambda (,ctx-var)
                                        ,@(unless ctx
                                           `((declare (ignore ,@(unless ctx `(,ctx-var))))))
                                        ,@body)
                                      :manual-output ,manual-output)))

(defmacro with-captured-buildbot-errors (() &body body)
  `(handler-case (progn ,@body)
     (buildbot-error (c)
       (typecase c
         (remote-lisp-error
          (values nil c (condition-ctx c)))
         (t
          (values nil c))))))

;;;;
;;;; Buildmaster entry points
;;;;
(defun ping-remote (&optional call-remote-phase-processor credentials
                    &rest context-keys &key &allow-other-keys &aux
                    (credentials (or credentials :buildslave)))
  "Perform a bare-minimum remote lisp bootstrap attempt."
  (with-captured-buildbot-errors ()
    (with-remote-lisp-context (nil (apply #'make-remote-lisp-context
                                          :credentials credentials
                                          :verbose-comm (getf context-keys :verbose-comm call-remote-phase-processor)
                                          (remove-from-plist context-keys :credentials :verbose-comm)))
        (cond (call-remote-phase-processor
               `(run-test-phases-with-markers `(:remote-lisp-fetch-phase) nil :verbose t)))
      t)))

(defun one* (&optional (reachability t) (upstream t) (recurse t) (remote-fetch t)
             (remote-load t) (remote-test nil) &rest keys
             &key credentials modules module-sets purge-store purge-metastore (optimize-debug t) (disable-debugger t) (verbose t)
             verbose-comm verbose-comm-starting-phase verbose-comm-starting-module)
  "A spread-phase shortcut for ONE."
  (declare (ignore credentials modules module-sets purge-store purge-metastore optimize-debug disable-debugger
                   verbose verbose-comm verbose-comm-starting-phase verbose-comm-starting-module))
  (apply #'one :phases (append (when reachability '(master-reachability-phase))
                               (when upstream '(master-update-phase))
                               (when recurse '(master-recurse-phase))
                               (when remote-fetch '(remote-lisp-fetch-phase))
                               (when remote-load '(remote-lisp-load-phase))
                               (when remote-test '(remote-lisp-test-phase)))
         keys))

(defparameter *default-buildmaster-run-phases* '(master-reachability-phase
                                                 master-update-phase
                                                 master-recurse-phase
                                                 remote-lisp-fetch-phase
                                                 remote-lisp-load-phase
                                                 ;; remote-lisp-test-phase
                                                 ))
(defparameter *default-buildmaster-module-sets* '(:release :converted))

;; At some point we'll need to add stored sets.
(defun compute-module-set (&optional (component-sets *default-buildmaster-module-sets*))
  (let ((gate (gate *self*)))
    (iter (for set in (or component-sets))
          (appending (ecase set
                       (:defined (do-modules (m) (collect (name m))))
                       (:desirable (do-modules (m) (when (typep (module-best-remote m :if-does-not-exist :continue :allow-self t) 'gate)
                                                     (collect (name m)))))
                       (:converted (gate-converted-module-names gate))
                       (:release (location-module-names gate)))))))

(defun one (&rest options &key credentials
            (phases *default-buildmaster-run-phases*)
            modules (module-sets *default-buildmaster-module-sets*)
            purge-store purge-metastore desire-branch metastore-branch optimize-debug disable-debugger
            (verbose t)
            verbose-comm
            verbose-comm-starting-phase
            verbose-comm-starting-module)
  (declare (ignore purge-store purge-metastore desire-branch metastore-branch optimize-debug disable-debugger))
  (find-executable 'ssh :if-does-not-exist :error)
  (let* ((gate (gate *self*))
         (credentials (or credentials :buildslave))
         (module-names (or (sort (copy-list modules) #'string<)
                           (multiple-value-bind (test-worthy central-system-less) (unzip (rcurry #'module-central-system :continue)
                                                                                         (compute-module-set module-sets))
                             (when central-system-less
                               (format t "~@<; ~@;~@<Fo~;llowing modules were excluded from ~
                                                             testing, because they don't have a ~
                                                             central system:~{ ~A~}~:@>~:@>~%"
                                       central-system-less))
                             (sort test-worthy #'string<))))
         (modules (mapcar #'coerce-to-module module-names))
         (m-r (make-instance 'buildmaster-run :locality gate :phases phases :modules modules)))
    (with-captured-buildbot-errors ()
      (with-maybe-verbose-termination (verbose-comm)
        (with-active-period (m-r)
          (let ((rest-phases (master-run-phases m-r))
                (result-marker (advance-result m-r)))
            (push m-r *buildmaster-runs*)
            (iter (for (phase . phases) on rest-phases)
                  (while (typep phase 'local-test-phase))
                  (setf result-marker (with-active-period (phase)
                                        (execute-test-phase m-r phase result-marker :verbose verbose-comm))
                        rest-phases phases))
            (when rest-phases
              (handler-case
                  (with-remote-lisp-context (ctx (apply #'make-remote-lisp-context
                                                        :credentials credentials
                                                        :verbose (or verbose-comm
                                                                     verbose-comm-starting-phase
                                                                     verbose-comm-starting-module)
                                                        (remove-from-plist options :credentials
                                                                           :phases :modules :module-sets
                                                                           :verbose
                                                                           :verbose-comm-starting-phase
                                                                           :verbose-comm-starting-module)))
                      `(run-test-phases-with-markers ',(mapcar (compose #'make-keyword #'type-of) rest-phases)
                                                     ',(mapcar #'make-keyword module-names)
                                                     :verbose ,verbose)
                    (dolist (p rest-phases)
                      (setf (remote-phase-ctx p) ctx))
                    (iter (for (phase . phases) on rest-phases)
                          (setf result-marker (with-active-period (phase)
                                                (execute-test-phase m-r phase result-marker
                                                                    :verbose verbose-comm
                                                                    :verbose-starting-module
                                                                    (and (if verbose-comm-starting-phase
                                                                             (string= verbose-comm-starting-phase
                                                                                      (type-of phase))
                                                                             t)
                                                                         verbose-comm-starting-module))))))
                (error (c)
                  (terminate-action result-marker :condition c)
                  (error c))))))))))

(defun metaone (&optional phases verbose no-purge module-sets credentials
                &rest one-keys &key &allow-other-keys &aux
                (credentials (or credentials :buildmaster))
                (module-sets (or module-sets '(:desirable)))
                (phases (or phases '(t t t t t nil))))
  (with-remote-lisp-context (ctx (make-remote-lisp-context :credentials credentials
                                                           :system :desire-buildbot
                                                           :package :desire-buildbot
                                                           :purge-store (not no-purge)
                                                           :optimize-debug t
                                                           :verbose verbose
                                                           :verbose-comm verbose)
                                 :manual-output t)
      `(multiple-value-bind (status condition) (one* ,@phases
                                                     :credentials :buildslave
                                                     :module-sets ',module-sets
                                                     ,@(remove-from-plist one-keys
                                                                          :phases
                                                                          :credentials
                                                                          :module-sets))
         (render-cl-waterfall-static #p"buildbot-output/")
         (write-line ";; Result rendering complete.")
         (when condition
           (error condition))
         (write-line ";; Buildmaster run succeded."))
    (with-slots (pipe) ctx
      (iter (for line = (read-line pipe nil nil))
            (for i from 0)
            (while line)
            (report-line i line)
            (snoop-remote-errors line i pipe 'remote-lisp-initialisation-error :ctx ctx))
      t)))