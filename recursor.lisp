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

;; system dictionary
(defun make-unwanted-missing (name) (cons name (cons nil nil)))
(defun make-wanted-missing (name) (cons name (cons :wanted nil)))
(defun make-unwanted-present (name) (cons name (cons nil :present)))
;; module dictionary
(defun make-notprocessing-undone (name) (cons name (cons nil nil)))
(defun make-processing-undone (name) (cons name (cons :processing nil)))

(defun discover-module-dependencies (module &optional (system-type *default-system-type*) complete system-dictionary verbose)
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
           (add-system-dependencies (module system modules system-dictionary)
             "Given a MODULE's SYSTEM, detect its dependencies and, accordingly, extend the sets
              of known REQUIRED systems, MODULES and MISSING unknown systems, returning them
              as multiple values."
             (multiple-value-bind (known unknown) (unzip (rcurry #'system :if-does-not-exist :continue) (direct-system-dependencies system))
               (multiple-value-bind (local-newdeps othermodule-newdeps) (unzip (compose (feq module) #'system-module)
                                                                               (mapcar #'system known))
                 (let ((extended-system-dictionary system-dictionary))
                   (dolist (newdep (append (mapcar (compose #'string #'name) local-newdeps)
                                           unknown))
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
             (if-let ((name (next-unsatisfied-system system-dictionary)))
               (let ((system (system name)))
                 (when verbose
                   (format t ";;;; processing known system ~A~%" name))
                 (unless (typep system system-type)
                   (recursor-error "~@<While operating in ~A mode, encountered an ~A at ~S.~:@>" system-type (type-of system) (system-pathname system)))
                 (unless (typep system system-type)
                   (recursor-error "~@<While operating in ~A mode, encountered an ~A.~:@>" system-type (type-of system)))
                 (setf (system-satisfiedp system-dictionary name) :present) ; made loadable, hiddens uncovered, deps about to be added
                 (add-system-dependencies module system modules system-dictionary))
               (values modules system-dictionary))))
    (let* ((all-systems (module-systems module))
           (main-system (module-central-system module))
           (other-systems (remove main-system all-systems)))
      ;; This doesn't deal with other modules providing same systems. Will silently break.
      (let* ((required-systems (xform main-system (curry #'cons main-system) (when complete other-systems)))
             (also-systems (unless complete other-systems))
             (required-names (mapcar #'name required-systems))
             (also-names (mapcar #'name also-systems))
             (extended-system-dictionary (append (mapcar #'make-wanted-missing required-names)
                                                 (mapcar #'make-unwanted-missing also-names)
                                                 system-dictionary)))
        (when verbose
          (format t "~@<;;;; ~@;Determining dependencies of module ~A.  Main system file: ~A.  ~
                       Other system files: ~A.  Required system files: ~A.  System ~
                       dictionary: ~A.~:@>~%"
                  (name module) (name main-system) (mapcar #'name other-systems) required-names extended-system-dictionary))
        (iter (with modules)
              (when (not (iter (for (name wanted . satisfied) in extended-system-dictionary)
                               (finding name such-that (and wanted (not satisfied)))))
                (return (values (remove-duplicates modules) extended-system-dictionary)))
              ;; Progress is made .. why?
              (for previous-system-dictionary = (copy-tree extended-system-dictionary))
              (for (values modules-new new-extended-system-dictionary) = (satisfy-next-system module system-type modules extended-system-dictionary))
              (when (equal previous-system-dictionary new-extended-system-dictionary)
                (error 'recursor-progress-halted :system-dictionary new-extended-system-dictionary))
              (setf (values modules extended-system-dictionary) (values modules-new new-extended-system-dictionary)))))))

(defgeneric satisfy-module (name locality system-type module-dictionary system-dictionary &key complete skip-present skip-missing verbose)
  (:method ((name symbol) locality system-type module-dictionary system-dictionary &key complete skip-present skip-missing verbose)
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
             (update module locality)
             ;; Discover and register system definitions.
             ;; Don't try to use the full-information dependency resolver, as it will likely fail.
             (notice-module-repository module nil locality)))
         (cond
           ((module-locally-present-p module locality)
            (multiple-value-bind (module-deps new-system-dictionary) (discover-module-dependencies module system-type complete system-dictionary verbose)
              (let* ((new-deps-from-this-module (remove-if (rcurry #'assoc module-dictionary) module-deps))
                     (new-module-dictionary (append module-dictionary (mapcar #'make-notprocessing-undone new-deps-from-this-module))))
                (syncformat t "~&~@<;; ~@;~S,~:[ no further dependencies~; added ~:*~A,~]~:@>~%" name new-deps-from-this-module)
                (multiple-value-prog1
                    (if new-deps-from-this-module
                        (satisfy-modules new-deps-from-this-module locality system-type new-module-dictionary new-system-dictionary nil
                                         :complete complete :skip-present skip-present :verbose verbose)
                        (values new-module-dictionary new-system-dictionary))))))
           (skip-missing
            (format t "~@<;;; ~@;Module ~A is missing, but SKIP-MISSING was specified, moving on with fingers crossed.~:@>~%" name)
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
         ;; satisfy the constraint of all loadable systems having dependencies slot bound
         (when verbose
           (format t "~@<;;; ~@;Recomputing system dependencies...~:@>~%"))
         (recompute-full-system-dependencies-set
          (iter (for (name nil . satisfied) in updated-system-dictionary)
                (unless (member name *implementation-provided-systems* :test #'string=)
                  (collect (system name)))))
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
