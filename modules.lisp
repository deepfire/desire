;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :desire)


(define-reported-condition module-systems-unloadable-error (module-error)
  ((systems :reader condition-systems :initarg :systems))
  (:report (module systems)
           "~@<Following ~S's systems couldn't be made loadable:~{ ~S~}~:@>" module systems))

(define-reported-condition system-name-conflict (system-error module-error)
  ()
  (:report (module system)
           "~@<During system discovery in module ~A: found a definition for system ~A from ~:[host-provided system set~;module ~A~].~:@>"
           (name module) (name system) (system-module system) (name (system-module system))))

;;;;
;;;; System discovery
;;;;
(defun invoke-with-module-system-definitions-and-blacklists (module repo-dir system-pathname-type fn)
  (let* ((totally-black (eq t (module-system-path-blacklist module)))
         (path (truename repo-dir))
         (subdirs (unless totally-black
                    (remove-if (lambda (p) (member (lastcar (pathname-directory p)) '(".git" "_darcs") :test #'string=)) 
                               (directory (subdirectory path '(:wild))))))
         (pass1 (unless totally-black
                  (append (directory (subfile path '(:wild) :type system-pathname-type))
                          (iter (for subdir in subdirs)
                                (appending (directory (subwild subdir '(:wild-inferiors) :name :wild :type system-pathname-type)))))))
         (blacklist-patterns (unless totally-black
                               (list* (subwild path '("test"))
                                      (subwild path '("tests"))
                                      (subwild path '("_darcs"))
                                      (when-let ((blacklist (module-system-path-blacklist module)))
                                        (list (subwild path blacklist)))))))
    (funcall fn pass1 blacklist-patterns)))

(defmacro do-module-system-definitions ((pathname module repo-dir system-pathname-type) &body body)
  (with-gensyms (pathnames blacklists)
    `(invoke-with-module-system-definitions-and-blacklists ,module ,repo-dir ,system-pathname-type
                                                           (lambda (,pathnames ,blacklists)
                                                             (flet ((blacklisted-p (x)
                                                                      (some (curry #'pathname-match-p x) ,blacklists)))
                                                               (iter (for ,pathname in ,pathnames)
                                                                     ,@body))))))

(defun find-module-system-definitions (module &optional (type *default-system-type*) (locality (gate *self*)))
  "Return a list of all MODULE's system definition pathnames corresponding to
system TYPE within LOCALITY."
  (do-module-system-definitions (path module (module-pathname module locality) (system-type-to-file-type type))
    (unless (blacklisted-p path)
      (collect path))))

(defun compute-system-definition-pathname (system repository &key (if-does-not-exist :error))
  "Return the pathname of a SYSTEM's definition within REPOSITORY."
  (let ((name (or (system-definition-pathname-name system) (system-canonical-definition-pathname-name system)))
        (type (system-pathname-type system)))
    (when (system-host-p system)
      (system-error system "~@<Asked to compute pathname of a host-provided system ~A.~:@>" name))
    (when (not (system-known-p system))
      (system-error system "~@<Asked to compute pathname of unknown system ~A.~:@>" name))
    (or (file-exists-p (subfile repository (list name) :type type)) ;; Try the fast path first.
        (do-module-system-definitions (path (system-module system) repository type)
          (finding path such-that (and (string= name (pathname-name path))
                                       (not (blacklisted-p path)))))
        (ecase if-does-not-exist
          (:error (error 'system-definition-missing-error :system system :path repository))
          (:continue nil)))))

(defun discover-and-register-module-systems (module &optional verbose (system-type *default-system-type*) (locality (gate *self*)) &aux
                                             (module (coerce-to-module module)))
  (let* ((sysfiles (find-module-system-definitions module system-type locality))
         (sysnames (mapcar (curry #'system-name-from-definition system-type) sysfiles)))
    (labels ((check-present-system-sanity (system)
               (unless (typep system system-type)
                 (system-error system "~@<During system discovery in module ~A: asked for a system ~S of type ~S, got one of type ~S~:@>"
                               system-type (name system) (type-of system)))
               (unless (eq module (system-module system))
                 (error 'system-name-conflict :system system :module module)))
             (register-new-system (name path)
               (syncformat t "~@<;; ~@;Registering a previously unknown system ~A at ~S~:@>~%" name path)
               (setf *unsaved-definition-changes-p* t)
               (make-instance system-type :name name :module module
                              :definition-pathname-name (when-let* ((pathname-name (pathname-name path))
                                                                    (hidden-p (not (equal pathname-name (downstring name)))))
                                                          pathname-name)))
             (ensure-system (name path)
               (lret ((system (or (lret ((present-system (system name :if-does-not-exist :continue)))
                                    (when present-system
                                      (if (system-known-p present-system)
                                          (check-present-system-sanity present-system)
                                          ;; Upgrading system to a known one!
                                          (remove-system present-system))))
                                  (register-new-system (canonicalise-name name) path))))
                 (setf (slot-value system 'definition-pathname) path
                       (slot-value system 'definition-write-date) (file-write-date path))
                 (ensure-system-loadable system path locality))))
      (let ((systems
             (iter (for path in sysfiles)
                   (for name in sysnames)
                   (let ((type (system-type-from-definition path)))
                     (unless (eq type system-type)
                       (recursor-error "~@<While operating in ~A mode, encountered an ~A at ~S.~:@>" system-type type path))
                     (when verbose
                       (format t "~@<;;;; ~@;Adding visible system ~A at ~S, and its hidden systems.~:@>~%" name path))
                     (handler-case
                         (let ((system (ensure-system name path)))
                           (appending (list* system
                                             ;; A hidden system is a system definition residing in a file named differently from main system's name.
                                             ;; Find them.
                                             ;; XXX: heuristics
                                             (when (typep system 'asdf-system)
                                               (let* ((raw-hidden-system-names (asdf-hidden-system-names path))
                                                      ;; this is only useful for the LOAD-based legacy method of hidden system name discovery
                                                      (raw-hidden-system-names-minus-known (set-difference raw-hidden-system-names sysnames :test #'equal)))
                                                 (iter (for hidden-system-name in raw-hidden-system-names-minus-known)
                                                       (when verbose
                                                         (format t "~@<;;;; ~@;Processing hidden system ~A at ~S.~:@>~%" hidden-system-name path))
                                                       (collect (ensure-system hidden-system-name path))))))))
                       (system-name-conflict (c)
                         (format *debug-io* "~@<;;;; ~@;~A~:@>~%" c)
                         (format *debug-io* "~@<;;;; ~@;Ignoring system definition ~S within module ~A.~:@>~%" path (name module))))))))
        (dolist (s (set-difference (module-systems module) systems))
          (do-remove-system s))
        (setf (module-systems module) systems)))))

(defun discover-and-register-systems (&optional verbose (system-type *default-system-type*) (locality (gate *self*)))
  "Scan repositories of modules present within LOCALITY for system definitions."
  (do-present-modules (m locality)
    (discover-and-register-module-systems m verbose system-type locality)))

;;;;
;;;; Module <-> system mapping heuristic
;;;;
;;; XXX: heuristics
;;; Why do we have this?
;;; Arguably naturally: we don't want to split projects at system granularity.
(defun module-central-system (module &optional (if-does-not-exist :error) &aux
                              (module (coerce-to-module module))
                              (systems (module-systems module)))
  "Answer the question -- what does it mean to load this particular module?"
  (or (cond ((endp (rest systems)) ; this takes care of the systemless case as well
             (first systems))
            ((find (name module) systems :key #'name))
            ((let ((non-test-systems (remove-if (curry #'search "TEST") systems :key (compose #'symbol-name #'name))))
               (cond ((endp (rest non-test-systems))
                      (first non-test-systems))
                     ((let ((fair-guesses (remove-if-not (curry #'search (symbol-name (name module))) non-test-systems :key (compose #'symbol-name #'name))))
                        (when (endp (rest fair-guesses))
                          (first fair-guesses))))))))
      (ecase if-does-not-exist
        (:continue)
        (:error (module-error module "~@<Unresolved ambiguity: failed to guess the system central in module ~A, among ~A.~:@>"
                              (name module) (mapcar #'name systems))))))

;;;;
;;;; Dependencies
;;;;
(defun module-dependencies (module &optional complete verbose &aux
                            (module (coerce-to-module module)))
  (let* ((all-systems (module-systems module))
         (main-system (module-central-system module))
         (other-systems (remove main-system all-systems))
         (required-systems (list* main-system (when complete other-systems))))
    (when verbose
      (format t "~@<;;;; ~@;Determining dependencies of module ~A.  Main system: ~A.  ~
                              Other systems: ~A.  Required systems: ~A.~:@>~%"
              (name module) (name main-system) (mapcar #'name other-systems) (mapcar #'name required-systems)))
    (let ((sysdeps (remove-if #'system-host-p (mapcan #'system-dependencies required-systems))))
      (when-let ((unknown-sysdeps (remove-if #'system-known-p sysdeps)))
        (module-error module "~@<Module ~A depends on unknown systems: ~A~:@>" (name module) (mapcar #'name unknown-sysdeps)))
      (when-let ((incomplete-sysdeps (remove-if #'system-definition-complete-p sysdeps)))
        (module-error module "~@<While calculating system dependencies of module ~A: incomplete sysdeps ~A with no unknown sysdeps.~:@>" (name module) (mapcar #'name incomplete-sysdeps)))
      (remove-duplicates (mapcar #'system-module sysdeps) :test #'eq))))

;;;;
;;;; System loadability
;;;;
(defun ensure-module-systems-loadable (module &optional (locality (gate *self*)) &aux
                                       (module (coerce-to-module module)))
  "Try making MODULE's systems loadable, defaulting to LOCALITY.
Raise an error of type MODULE-SYSTEMS-UNLOADABLE-ERROR upon failure."
  (dolist (s (module-systems module))
    (when *verbose-repository-maintenance*
      (format t "~@<;;; ~@;Ensuring loadability of ~A ~A~:@>~%" (type-of s) (name s)))
    (ensure-system-loadable s (system-definition-pathname s) locality)))

;;;;
;;;; Module repository maintenance hook
;;;;
;;; Branching model management.
;;; System discovery, loadability and dependencies.
(defgeneric notice-module-repository (module &optional compute-system-dependencies locality)
  (:documentation
   "Ensure that the repository corresponding to MODULE within LOCALITY
meets desire's operational requirements.")
  (:method ((o module) &optional (compute-system-dependencies t) (locality (gate *self*)))
    (let ((repo-dir (module-pathname o locality)))
      (when *verbose-repository-maintenance*
        (format t "~@<;; ~@;Processing ~A's repository at ~S~:@>~%" (name o) repo-dir))
      (ensure-tracker-branch repo-dir (ref-value "master" repo-dir))
      (discover-and-register-module-systems o *verbose-repository-maintenance* *default-system-type* locality)
      (dolist (s (module-systems o))
        (recompute-direct-system-dependencies-one s))
      (when compute-system-dependencies
        (when-let ((changed-systems (remove-if #'system-dependencies-up-to-date-p (module-systems o))))
          (recompute-full-system-dependencies-set changed-systems))))))
