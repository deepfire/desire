;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2009 by
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


(defparameter *implementation-provided-systems*
  #+sbcl '("ASDF-INSTALL" "SB-ACLREPL" "SB-BSD-SOCKETS" "SB-COVER" "SB-GROVEL" "SB-MD5" "SB-POSIX" "SB-ROTATE-BYTE" "SB-RT" "SB-SIMPLE-STREAMS")
  #-sbcl nil)

(define-reported-condition recursor-progress-halted (recursor-error)
  ((system-dictionary :reader condition-system-dictionary :initarg :system-dictionary))
  (:report (system-dictionary previous-system-dictionary)
           "~@<Progress halted while processing system dictionary ~S.~:@>" system-dictionary))
(define-reported-condition counterproductive-system-definition (recursor-error module-error system-error)
  ((raw-hidden-system-names :reader condition-raw-hidden-system-names :initarg :raw-hidden-system-names)
   (known-other-module-systems :reader condition-known-other-module-systems :initarg :known-other-module-systems)
   (other-modules :reader condition-other-modules :initarg :other-modules))
  (:report (module system raw-hidden-system-names known-other-module-systems other-modules)
           "~@<In module ~A: encountered a counterproductive definition for system ~A, ~
               which violates declarative semantics of system definitions by imperatively ~
               loading systems ~A known to be belonging to modules ~A.  Unprocessed hidden ~
               system discovery output: ~A.~:@>"
           (name module) (name system) (mapcar #'name known-other-module-systems) (mapcar #'name other-modules) raw-hidden-system-names))
(define-reported-condition system-name-conflict (recursor-error module-error system-error)
  ()
  (:report ()
           "~@<System name conflict.~:@>"))

;; system dictionary
(defun make-unwanted-missing (name) (cons name (cons nil nil)))
(defun make-wanted-missing (name) (cons name (cons :wanted nil)))
(defun make-unwanted-present (name) (cons name (cons nil :present)))
;; module dictionary
(defun make-notprocessing-undone (name) (cons name (cons nil nil)))
(defun make-processing-undone (name) (cons name (cons :processing nil)))

(defun module-dependencies (module &optional (locality (gate *self*)) (system-type *default-system-type*) complete system-dictionary verbose)
  (let ((syspath (make-hash-table :test #'equal)))
    (labels ((syspath (name) (gethash name syspath))
             (set-syspath (name value) (setf (gethash name syspath) value))
             (subj (c) (car c))
             (cell (c) (cdr c))
             ((setf cellar) (v c) (setf (cadr c) v))
             ((setf celldr) (v c) (setf (cddr c) v))
             (entry (vocab subj &optional (test #'eq))
               (assoc subj vocab :test test))
             (vocab-maybe-add-wanted-missing (vocab subj &optional (test #'eq))
               (if (entry vocab subj test)
                   vocab
                   (cons (make-wanted-missing subj) vocab)))
             (syscell-wanted-missingp (c) (and (car c) (not (cdr c))))
             (next-unsatisfied-system (system-dictionary)
               (subj (find-if #'syscell-wanted-missingp system-dictionary :key #'cell)))
             ((setf system-satisfiedp) (val system-dictionary sysname)
               (setf (celldr (entry system-dictionary sysname #'string=)) val))
             (register-new-system (name path type module)
               (syncformat t ";; Registering a previously unknown system ~A~%" name)
               (setf *unsaved-definition-changes-p* t)
               (make-instance type :name name :module module
                              :definition-pathname-name (when-let* ((pathname-name (pathname-name path))
                                                                    (hidden-p (not (equal pathname-name (downstring name)))))
                                                          pathname-name)))
             (add-system-dependencies (module system modules system-dictionary)
               "Given a MODULE's SYSTEM, detect its dependencies and, accordingly, extend the sets
              of known REQUIRED systems, MODULES and MISSING unknown systems, returning them
              as multiple values."
               (multiple-value-bind (new-sysdeps new-missing) (system-dependencies system)
                 (multiple-value-bind (local-newdeps othermodule-newdeps) (unzip (compose (feq module) #'system-module) new-sysdeps)
                   (let ((extended-system-dictionary system-dictionary))
                     (dolist (newdep (append (mapcar (compose #'string #'name) local-newdeps)
                                             new-missing))
                       (if-let ((entry (entry system-dictionary newdep #'string=)))
                         (setf (cellar entry) :wanted)
                         (setf extended-system-dictionary (cons (make-wanted-missing newdep) extended-system-dictionary))))
                     ;; NOTE: on module boundaries we lose precise system dependency names
                     (values (append (mapcar (compose #'name #'system-module) othermodule-newdeps) modules)
                             extended-system-dictionary)))))
             (satisfy-next-system (module system-type &optional modules system-dictionary)
               "Given a MODULE and a list of its REQUIRED systems, pick one and try to handle
              the fallout. Return the modified sets of known REQUIRED systems, MODULES and
              MISSING unknown systems, returning them as multiple values."
               (declare (special *syspath*))
               (if-let ((name (next-unsatisfied-system system-dictionary)))
      (let* ((all-sysfiles (compute-module-system-definitions module system-type locality))
                 (progn
                   (if-let ((system (system name :if-does-not-exist :continue)))
                     (let* ((path (or (syspath name)
                                      (recursor-error "~@<Internal invariant violation during dependency resolution: failed to find system ~S among syspathed ~S~:@>~%"
                                                      name (hash-table-keys *syspath*))))
                            (actual-type (system-type-from-definition path)))
                       (when verbose
                         (format t ";;;; processing known system ~A~%" name))
                       (unless (subtypep system-type actual-type)
                         (recursor-error "~@<While operating in ~A mode, encountered an ~A at ~S.~:@>" system-type actual-type path))
                       (unless (typep system system-type)
                         (recursor-error "~@<While operating in ~A mode, encountered an ~A.~:@>" system-type (type-of system)))
                       (setf (system-satisfiedp system-dictionary name) :present) ; made loadable, hiddens uncovered, deps about to be added
                       (add-system-dependencies module system modules system-dictionary))
                     (let ((entry (entry system-dictionary name #'string=)))
                       (when verbose
                         (format t ";;;; processing unknown system ~A~%" name))
                       (if (and entry (not (eq :wanted (car (cell entry)))))
                           (progn
                             (setf (cellar entry) :wanted)
                             (values modules (cons entry (remove entry system-dictionary))))
                           (recursor-error "~@<Encountered a non-local dependency on an unknown system ~A.~:@>" name)))))
                 (values modules system-dictionary)))
             (add-visible-system (module name path type dictionary known-local-visible &optional (actual-type (system-type-from-definition path)))
               (unless (eq type actual-type)
                 (recursor-error "~@<While operating in ~A mode, encountered an ~A at ~S.~:@>" type actual-type path))
               (when verbose
                 (format t "~@<;;;; ~@;Adding visible system ~A at ~S, and its hidden systems.~:@>~%" name path))
               (let ((system (or (lret ((system (system name :if-does-not-exist :continue)))
                                   (when system
                                     (unless (typep system type)
                                       (recursor-error "~@<During dependency resolution: asked for a system ~S of type ~S, got one of type ~S~:@>"
                                                       type name (type-of system)))))
                                 (register-new-system (intern (string-upcase name)) path type module))))
                 (set-syspath name path)
                 (ensure-system-loadable system path t locality)
                 (append dictionary
                         (when (typep system 'asdf-system)
                           ;; A hidden system is a system definition residing in a file named differently from main system's name.
                           ;; Find them.
                           (let* ((raw-hidden-system-names (asdf-hidden-system-names path))
                                  (raw-hidden-system-names-minus-known (set-difference raw-hidden-system-names known-local-visible :test #'equal)))
                             (iter (for hidden-system-name in raw-hidden-system-names-minus-known)
                                   (when verbose
                                     (format t "~@<;;;; ~@;Processing hidden system ~A at ~S.~:@>~%" hidden-system-name path))
                                   (let ((hidden-system (or (system hidden-system-name :if-does-not-exist :continue)
                                                            (register-new-system (intern hidden-system-name) path type module))))
                                     (unless (eq module (system-module hidden-system))
                                       (let ((known-other-module-systems (iter (for hsname in raw-hidden-system-names-minus-known)
                                                                               (for knownhs = (system hsname :if-does-not-exist :continue))
                                                                               (when (and knownhs (not (eq module (system-module knownhs))))
                                                                                 (collect knownhs)))))
                                         (error 'counterproductive-system-definition :module module :system system
                                                :raw-hidden-system-names raw-hidden-system-names :known-other-module-systems known-other-module-systems
                                                :other-modules (mapcar #'system-module known-other-module-systems))))
                                     (ensure-system-loadable hidden-system path nil locality))
                                   (set-syspath hidden-system-name path)
                                   (collect (if complete
                                                (make-wanted-missing hidden-system-name)
                                                (make-unwanted-missing hidden-system-name))))))))))
             (main-sysfile (find (string-downcase (module-central-system-name module)) all-sysfiles :key #'pathname-name :test #'string=))
             (other-sysfiles (remove main-sysfile all-sysfiles)))
        ;; This doesn't deal with other modules providing same systems. Will silently break.
        (let* ((required-sysfiles (xform main-sysfile (curry #'cons main-sysfile) (when complete other-sysfiles)))
               (also-sysfiles (unless complete other-sysfiles))
               (required-names (mapcar (curry #'system-name-from-definition system-type) required-sysfiles))
               (also-names (mapcar (curry #'system-name-from-definition system-type) also-sysfiles))
               (all-names (append required-names also-names))
               (extended-system-dictionary (append (mapcar #'make-wanted-missing required-names)
                                                   (mapcar #'make-unwanted-missing also-names)
                                                   system-dictionary)))
          (when verbose
            (format t "~@<;;;; ~@;Determining dependencies of module ~A.  Main system file: ~A.  ~
                       Other system files: ~A.  Required system files: ~A.  Pre-extension system ~
                       dictionary: ~A.~:@>~%"
                    (name module) main-sysfile other-sysfiles required-sysfiles extended-system-dictionary))
          (iter (for sysfile in (append required-sysfiles also-sysfiles))
                (for name in (append required-names also-names))
                (setf extended-system-dictionary (add-visible-system module name sysfile system-type extended-system-dictionary all-names))
                (when verbose
                  (format t "~@<;;;; ~@;After adding system definition ~A dictionary is: ~A.~:@>~%"
                          name extended-system-dictionary)))
          (iter (with modules)
                (when (not (iter (for (name wanted . satisfied) in extended-system-dictionary)
                                 (finding name such-that (and wanted (not satisfied)))))
                  (return (values (remove-duplicates modules) extended-system-dictionary)))
                ;; Progress is made .. why?
                (for previous-system-dictionary = (copy-tree extended-system-dictionary))
                (for (values modules-new new-extended-system-dictionary) = (satisfy-next-system module system-type modules extended-system-dictionary))
                (when (equal previous-system-dictionary new-extended-system-dictionary)
                  (error 'recursor-progress-halted :system-dictionary new-extended-system-dictionary))
                (setf (values modules extended-system-dictionary) (values modules-new new-extended-system-dictionary))))))))

(defgeneric satisfy-module (name locality system-type module-dictionary system-dictionary &key complete skip-present skip-missing verbose)
  (:method ((name symbol) locality system-type module-dictionary system-dictionary &key complete skip-present skip-missing verbose)
    (when verbose
      (syncformat t "~@<;;; ~@;modules: ~A~:@>~%" module-dictionary)
      (syncformat t "~@<;;; ~@;systems: ~A~:@>~%" system-dictionary))
    (let ((cell (or (cdr (assoc name module-dictionary))
                    (cdr (first (push (make-notprocessing-undone name) module-dictionary)))))  ; extend the dictionary
          (module (module name)))
      (cond
        ((car cell)
         (when (and skip-present (module-locally-present-p module locality))
           (setf (cdr cell) :done))
         (values module-dictionary system-dictionary))
        (t
         (setf (car cell) :processing)
         (let ((present-before-update-p (module-locally-present-p module locality)))
           (unless (or (and skip-present present-before-update-p)
                       (and skip-missing (not present-before-update-p)))
             (update module locality)))
         (cond
           ((module-locally-present-p module locality)
            (multiple-value-bind (module-deps new-system-dictionary) (module-dependencies module locality system-type complete system-dictionary verbose)
              (let* ((new-deps-from-this-module (remove-if (rcurry #'assoc module-dictionary) module-deps))
                     (new-module-dictionary (append module-dictionary (mapcar #'make-notprocessing-undone new-deps-from-this-module))))
                (syncformat t "~&~@<;; ~@;~S,~:[ no further dependencies~; added ~:*~A,~]~:@>~%" name new-deps-from-this-module)
                (multiple-value-prog1
                    (if new-deps-from-this-module
                        (satisfy-modules new-deps-from-this-module locality system-type new-module-dictionary new-system-dictionary nil
                                         :complete complete :skip-present skip-present :verbose verbose)
                        (values new-module-dictionary new-system-dictionary))))))
           (skip-missing
            (syncformat t "~@<;;; ~@;Module ~A is missing, but SKIP-MISSING was specified, moving on with fingers crossed.~:@>~%" name)
            (setf (cdr cell) :done)
            (values module-dictionary system-dictionary))
           (t
            (module-error (module name) "~@<Couldn't obtain module ~A.~:@>" name)))))))
  (:method :around (name locality system-type module-dictionary system-dictionary &key complete skip-present skip-missing verbose)
    (declare (ignore complete skip-present skip-missing verbose))
    (multiple-value-bind (new-module-dictionary new-system-dictionary) (call-next-method)
      (let ((cell (cdr (assoc name new-module-dictionary))))
        (assert cell)
        (syncformat t "~&~@<;; ~@;Done processing ~S, ~D left~:@>~%" name (count-if-not #'cddr new-module-dictionary))
        (setf (cdr cell) :done)
        (values new-module-dictionary new-system-dictionary)))))

(defun satisfy-modules (module-names locality system-type module-dictionary system-dictionary toplevelp &key complete skip-present skip-missing verbose)
  (iter (for module-name in module-names)
        (for (values updated-module-dictionary updated-system-dictionary) = (satisfy-module module-name locality system-type module-dictionary system-dictionary
                                                                                            :complete complete :skip-present skip-present :skip-missing skip-missing :verbose verbose))
        (setf (values module-dictionary system-dictionary) (values updated-module-dictionary updated-system-dictionary))
        (finally
         (when-let ((undone (and toplevelp (mapcar #'car (remove-if #'cddr module-dictionary)))))
           (format t "WARNING: after all gyrations following modules were left unsatisfied:~{ ~S~}~%" undone))
         (return (values module-dictionary system-dictionary)))))

(defun desire (desires &key complete skip-present skip-missing (seal t) verbose)
  "Satisfy module DESIRES and return the list of names of updated modules.

Desire satisfaction means:
   - for specified missing modules, retrieval,
   - for specified present modules, update, unless SKIP-PRESENT is
     non-nil,

In all cases, systems present in the set union of specified and
depended upon modules are ensured to be loadable. See also the
documentation of the COMPLETE keyword.

When individual desires are symbols, they are interpreted as module names.
When they are lists, their first element is interpreted as the source
distributor, from which the rest of the list is supposed to be fetched.
These two forms can be mixed in the list of desires.
The distributor specification is currently ignored, though.

Defined keywords:
   - SKIP-PRESENT - whether to skip updating specified modules which are 
     already present, defaults to nil,
   - SEAL - whether to commit any definition changes, and,
   - COMPLETE - whether to obtain all modules' systems, even those not
     part of main module systems' complete dependency graphs."
  (let* ((interpreted-desires (mapcar (curry #'xform-if-not #'consp (lambda (m) (list nil m))) desires)))
    (when-let ((desired-module-names (mapcar #'canonicalise-name (mapcan #'rest interpreted-desires))))
      (let ((module-dictionary nil)
            (system-dictionary (mapcar #'make-unwanted-present *implementation-provided-systems*)))
        (syncformat t "; Satisfying desire for ~D module~:*~P:~%" (length desired-module-names))
        (satisfy-modules desired-module-names (gate *self*) *default-system-type* module-dictionary system-dictionary :sure-as-hell
                         :complete complete :skip-present skip-present :skip-missing skip-missing :verbose verbose)
      
        (when *unsaved-definition-changes-p*
          (syncformat t "; Definitions modified, writing~:[~; and committing~] changes.~%" seal)
          (save-definitions :seal seal :commit-message (format nil "Added~{ ~A~} and ~:[their~;its~] dependencies."
                                                               desired-module-names (endp (rest desired-module-names)))))
        (syncformat t "; All done.~%")
        (values)))))

(defun lust (&rest desires)
  "A spread interface function for DESIRE.
Updates present specified modules and skips present depended ones."
  (desire desires))
