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


(define-reported-condition recursor-progress-halted (recursor-error)
  ((system-dictionary :reader condition-system-dictionary :initarg :system-dictionary))
  (:report (system-dictionary previous-system-dictionary)
           "~@<Progress halted while processing system dictionary ~S.~:@>" system-dictionary))

;; system dictionary
(defun make-unwanted-missing (name) (cons name (cons nil nil)))
(defun make-wanted-missing (name) (cons name (cons :wanted nil)))
(defun make-unwanted-present (name) (cons name (cons nil :present)))
;; module dictionary
(defun make-notprocessing-undone (name) (cons name (cons nil nil)))
(defun make-processing-undone (name) (cons name (cons :processing nil)))

(defun discover-direct-module-dependencies (module &optional
                                            (required-systems (list (module-central-system module)))
                                            (system-type *default-system-type*)
                                            system-dictionary verbose &aux
                                            (module (coerce-to-module module))
                                            (required-systems (mapcar #'coerce-to-system required-systems)))
  (labels ((subj (c) (car c))
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
           (mark-system-wanted (name dictionary)
             (if-let ((entry (entry dictionary name #'string=)))
               (setf (cellar entry) :wanted
                     dictionary dictionary)
               (cons (make-wanted-missing name) dictionary)))
           (mark-system-unwanted (name dictionary)
             (if (entry dictionary name #'string=)
                 dictionary
                 (cons (make-unwanted-missing name) dictionary)))
           (add-system-dependencies (module system modules system-dictionary)
             "Given a MODULE's SYSTEM, detect its dependencies and, accordingly, extend the sets
              of known REQUIRED systems, MODULES and MISSING unknown systems, returning them
              as multiple values."
             ;; System ontology:
             ;; - undefined :: missing from both DEFINITIONS and the runtime
             ;; - unknown :: represented in the runtime, has no module associated;
             ;;              it's not decided yet whether such systems are worth of a DEFINITIONS entry
             ;; - not locally present :: represented in DEFINITIONS and in the runtime,
             ;;                          has a module associated, but the module is not locally present
             ;; - locally present :: as above, but module is present and DEFINITION-PATHNAME is bound
             (multiple-value-bind (defined undefined) (unzip (rcurry #'system :if-does-not-exist :continue)
                                                             (direct-system-dependencies system))
               (when verbose
                 (format t "~@<;;; ~@;Di~@<rect defined sysdeps of ~A:~{ ~A~}~:@>~:@>~%" (name system) defined)
                 (format t "~@<;;; ~@;Un~@<defined sysdeps of ~A:~{ ~A~}~:@>~:@>~%" (name system) undefined))
               (multiple-value-bind (known unknown) (unzip #'system-known-p
                                                           (mapcar #'system defined))
                 (multiple-value-bind (known-local known-external) (unzip (compose (feq module) #'system-module)
                                                                          known)
                   (multiple-value-bind (host-provided-external truly-external) (unzip #'system-host-p
                                                                                       known-external)
                     (declare (ignore host-provided-external))
                     (let ((extended-system-dictionary system-dictionary))
                       (dolist (newdep (append (mapcar (compose #'string #'name) (append known-local unknown))
                                               undefined))
                         (setf extended-system-dictionary (mark-system-wanted newdep extended-system-dictionary)))
                       ;; NOTE: on module boundaries we lose precise system dependency names
                       (values (remove-duplicates
                                (append (mapcar (compose #'name #'system-module) truly-external)
                                        modules))
                               extended-system-dictionary)))))))
           (satisfy-next-system (module system-type &optional modules system-dictionary)
             "Given a MODULE and a list of its REQUIRED systems, pick one and try to handle
              the fallout. Return the modified sets of known REQUIRED systems, MODULES and
              MISSING unknown systems, returning them as multiple values."
             (if-let* ((name (next-unsatisfied-system system-dictionary)))
               (let ((system (ensure-system name)))
                 (when verbose
                   (format t ";;;; processing known system ~A~%" name))
                 (unless (typep system system-type)
                   (recursor-error "~@<While calculating dependencies of module ~A, in ~A mode, ~
                                       encountered an ~A ~A~:[~; at ~S~].~:@>"
                                   (name module) system-type (type-of system) name
                                   (system-locally-present-p system)
                                   (when (system-locally-present-p system)
                                     (system-definition-pathname system))))
                 (setf (system-satisfiedp system-dictionary name) :present) ; made loadable, hiddens uncovered, deps about to be added
                 (add-system-dependencies module system modules system-dictionary))
               (values modules system-dictionary))))
    (let* ((all-systems (module-systems module))
           (other-systems (set-difference all-systems required-systems))
           (required-names (mapcar #'name required-systems))
           (other-names (mapcar #'name other-systems)))
      ;; This doesn't deal with other modules providing same systems. Will silently break.
      (dolist (name required-names)
        (setf system-dictionary (mark-system-wanted (string name) system-dictionary)))
      (dolist (name other-names)
        (setf system-dictionary (mark-system-unwanted (string name) system-dictionary)))
      (when verbose
        (format t "~@<;;;; ~@;Determining dependencies of module ~A.  ~
                              Required system files: ~A.  Other system files: ~A.  ~
                              System dictionary: ~A.~:@>~%"
                (name module) required-names other-names system-dictionary))
      (iter (with modules)
            (when (not (iter (for (name wanted . satisfied) in system-dictionary)
                             (finding name such-that (and wanted (not satisfied)))))
              (return (values (remove-duplicates modules) system-dictionary)))
            ;; Progress is made .. why?
            (for previous-system-dictionary = (copy-tree system-dictionary))
            (for (values modules-new new-system-dictionary) = (satisfy-next-system module system-type modules system-dictionary))
            (when (equal previous-system-dictionary new-system-dictionary)
              (error 'recursor-progress-halted :system-dictionary new-system-dictionary))
            (setf (values modules system-dictionary) (values modules-new new-system-dictionary))))))

(defgeneric satisfy-module (name locality system-type module-dictionary system-dictionary &key systems complete skip-present skip-missing verbose)
  (:method ((name symbol) locality system-type module-dictionary system-dictionary &key (systems nil systems-specified-p) complete skip-present skip-missing verbose)
    (when verbose
      (format t "~@<;;; ~@;modules: ~A~:@>~%" module-dictionary)
      (format t "~@<;;; ~@;systems: ~A~:@>~%" system-dictionary))
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
             (update module locality))
           (cond
             ((module-locally-present-p module locality)
              ;; Discover and register system definitions.
              ;; Don't try to use the full-information dependency resolver, as it will likely fail.
              (notice-module-repository module nil locality)
              (let ((required-systems (cond (complete
                                             (module-systems module))
                                            (systems-specified-p
                                             (mapcar #'coerce-to-system systems))
                                            (t
                                             (list (module-central-system module))))))
                (when verbose
                  (syncformat t "~@<;; ~@;~A: ~@<required systems:~{ ~A~}~:@>~:@>~%" name (mapcar #'name required-systems)))
                (multiple-value-bind (module-deps new-system-dictionary)
                    (discover-direct-module-dependencies module required-systems system-type system-dictionary verbose)
                  (let* ((new-deps-from-this-module (remove-if (rcurry #'assoc module-dictionary) module-deps))
                         (new-module-dictionary (append module-dictionary (mapcar #'make-notprocessing-undone new-deps-from-this-module))))
                    (syncformat t "~&~@<;; ~@;~A,~:[ no further dependencies~; added ~:*~A,~]~:@>~%" name new-deps-from-this-module)
                    (multiple-value-prog1
                        (if new-deps-from-this-module
                            (satisfy-modules (mapcar #'list new-deps-from-this-module) ; create default specs
                                             locality system-type new-module-dictionary new-system-dictionary nil
                                             :complete complete :skip-present skip-present :verbose verbose)
                            (values new-module-dictionary new-system-dictionary)))))))
             (skip-missing
              (format t "~@<;;; ~@;Module ~A is missing, but SKIP-MISSING was specified, moving on with fingers crossed.~:@>~%" name)
              (setf (cdr cell) :done)
              (values module-dictionary system-dictionary))
             (t
              (module-error (module name) "~@<Couldn't obtain module ~A.~:@>" name))))))))
  (:method :after ((name symbol) locality system-type module-dictionary system-dictionary &key &allow-other-keys)
    (let ((module (module name)))
      (module-post-install name module locality (module-pathname module locality))))
  (:method :around (name locality system-type module-dictionary system-dictionary &key &allow-other-keys)
    (multiple-value-bind (new-module-dictionary new-system-dictionary) (call-next-method)
      (let ((cell (cdr (assoc name new-module-dictionary))))
        (assert cell)
        (syncformat t "~&~@<;; ~@;Done processing ~S, ~D left~:@>~%" name (count-if-not #'cddr new-module-dictionary))
        (setf (cdr cell) :done)
        (values new-module-dictionary new-system-dictionary)))))

(defun satisfy-modules (module-specs locality system-type module-dictionary system-dictionary toplevelp
                        &key complete skip-present skip-missing verbose)
  (iter (for (name . required-system-names) in module-specs)
        (for (values updated-module-dictionary updated-system-dictionary) =
             (apply #'satisfy-module name locality system-type module-dictionary system-dictionary
                    :complete complete
                    :skip-present skip-present :skip-missing skip-missing
                    :verbose verbose
                    (when required-system-names
                      (list :systems required-system-names))))
        (setf (values module-dictionary system-dictionary) (values updated-module-dictionary updated-system-dictionary))
        (finally
         (when-let ((undone (and toplevelp (mapcar #'car (remove-if #'cddr module-dictionary)))))
           (format t "WARNING: after all gyrations following modules were left unsatisfied:~{ ~S~}~%" undone))
         ;; satisfy the constraint of all loadable systems having dependencies slot boundp
         (when toplevelp
           (when verbose
             (format t "~@<;;; ~@;Recomputing system dependencies...~:@>~%"))
           (update-system-set-dependencies
            (lret ((involved-systems (mapcar (compose #'system #'first) updated-system-dictionary)))
              (format t "~@<;;; ~@;Finally, recalculating fulldeps of following systems: ~A~:@>~%"
                      (mapcar #'name (remove-if #'system-host-p involved-systems))))
            :verbose verbose))
         (return (values module-dictionary system-dictionary)))))

(defun desire (desires &key complete skip-present skip-missing (seal t) verbose)
  "Satisfy module DESIRES and return the list of names of updated modules.

Desire satisfaction means:
   - for specified missing modules, retrieval,
   - for specified present modules, update, unless SKIP-PRESENT is
     non-nil,

COMPLETE designates the breadth of system satisfaction -- when COMPLETE
is non-NIL, it means that every system present in every module is a source
of potential dependencies, which need to be satisfied.  When COMPLETE is NIL,
only the system chosen (heuristically) by MODULE-CENTRAL-SYSTEM is considered
as a source of dependencies. 

When individual desires are symbols, they are interpreted as module names.
When they are lists, their first element is interpreted as a module name,
and the rest as names of systems which need to be satisfied, which presents
a more controlled alternative to the more blunt COMPLETE required system
specification method.

Defined keywords:
   - SKIP-PRESENT - whether to skip updating specified modules which are 
     already present, defaults to NIL;
   - SEAL - whether to commit any definition changes, default is T;
   - COMPLETE - whether to obtain all modules' systems, even those not
     part of main module systems' complete dependency graphs, default is NIL."
  (when-let ((module-system-specs (mapcar (curry #'xform-if-not #'consp #'list) (map-list-tree #'canonicalise-name desires))))
    (syncformat t "; Satisfying desire for ~D module~:*~P:~%" (length module-system-specs))
    (satisfy-modules module-system-specs (gate *self*) *default-system-type* nil (mapcar #'make-unwanted-present
                                                                                         *implementation-provided-system-names*)
                     :sure-as-hell
                     :complete complete :skip-present skip-present :skip-missing skip-missing :verbose verbose)
      
    (when *unsaved-definition-changes-p*
      (syncformat t "; Definitions modified, writing~:[~; and committing~] changes.~%" seal)
      (save-definitions :seal seal :commit-message (format nil "Added~{ ~A~} and ~:[their~;its~] dependencies."
                                                           (mapcar #'car module-system-specs) (endp (rest module-system-specs)))))
    (syncformat t "; All done.~%")
    (values)))

(defun lust (&rest desires)
  "A spread interface function for DESIRE.
Updates present specified modules and skips present depended ones."
  (desire desires))
