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

;; system vocabulary
(defun make-unwanted-missing (name) (cons name (cons nil nil)))
(defun make-wanted-missing (name) (cons name (cons :wanted nil)))
(defun make-unwanted-present (name) (cons name (cons nil :present)))
;; module vocabulary
(defun make-notprocessing-undone (name) (cons name (cons nil nil)))
(defun make-processing-undone (name) (cons name (cons :processing nil)))

(defun module-dependencies (module &optional (locality (gate *self*)) (system-type *default-system-type*) complete system-vocabulary)
  (let ((syspath (make-hash-table :test #'equal)))
    (labels ((syspath (name) (gethash name syspath))
             (set-syspath (name value) (setf (gethash name syspath) value))
             (subj (c) (car c))
             (cell (c) (cdr c))
             ((setf celldr) (v c) (setf (cddr c) v))
             (entry (vocab subj &optional (test #'eq))
               (assoc subj vocab :test test))
             (vocab-maybe-add-wanted-missing (vocab subj &optional (test #'eq))
               (if (entry vocab subj test)
                   vocab
                   (cons (make-wanted-missing subj) vocab)))
             (syscell-wanted-missingp (c) (and (car c) (not (cdr c))))
             (next-unsatisfied-system (system-vocabulary)
               (subj (find-if #'syscell-wanted-missingp system-vocabulary :key #'cell)))
             ((setf system-satisfiedp) (val system-vocabulary sysname)
               (setf (celldr (entry system-vocabulary sysname #'string=)) val))
             (register-new-system (name path type module)
               (syncformat t ";; Registering a previously unknown system ~A~%" name)
               (setf *unsaved-definition-changes-p* t)
               (make-instance type :name name :module module
                              :definition-pathname-name (when-let* ((pathname-name (pathname-name path))
                                                                    (hidden-p (not (equal pathname-name (downstring name)))))
                                                          pathname-name)))
             (add-system-dependencies (module system modules system-vocabulary)
               "Given a MODULE's SYSTEM, detect its dependencies and, accordingly, extend the sets
              of known REQUIRED systems, MODULES and MISSING unknown systems, returning them
              as multiple values."
               (multiple-value-bind (new-sysdeps new-missing) (system-dependencies system)
                 (multiple-value-bind (local-newdeps othermodule-newdeps) (unzip (compose (feq module) #'system-module) new-sysdeps)
                   (let ((extended-system-vocabulary system-vocabulary))
                     (dolist (newdep (append (mapcar (compose #'string #'name) local-newdeps)
                                             new-missing))
                       (if-let ((cell (cell (entry system-vocabulary newdep #'string=))))
                         (setf (car cell) :wanted)
                         (setf extended-system-vocabulary (cons (make-wanted-missing newdep) extended-system-vocabulary))))
                     ;; NOTE: on module boundaries we lose precise system dependency names
                     (values (append (mapcar (compose #'name #'system-module) othermodule-newdeps) modules)
                             extended-system-vocabulary)))))
             (satisfy-next-system (module system-type &optional modules system-vocabulary)
               "Given a MODULE and a list of its REQUIRED systems, pick one and try to handle
              the fallout. Return the modified sets of known REQUIRED systems, MODULES and
              MISSING unknown systems, returning them as multiple values."
               (declare (special *syspath*))
               (if-let ((name (next-unsatisfied-system system-vocabulary)))
                 (if-let ((system (system name :if-does-not-exist :continue)))
                   (let* ((path (or (syspath name)
                                    (error "~@<Internal invariant violation during dependency resolution: failed to find system ~S among syspathed ~S~:@>~%"
                                           name (hash-table-keys *syspath*))))
                          (actual-type (system-definition-type path)))
                     (unless (subtypep system-type actual-type)
                       (error "~@<While operating in ~A mode, encountered an ~A at ~S.~:@>" system-type actual-type path))
                     (unless (typep system system-type)
                       (error "~@<While operating in ~A mode, encountered an ~A.~:@>" system-type (type-of system)))
                     (setf (system-satisfiedp system-vocabulary name) :present) ; made loadable, hiddens uncovered, deps about to be added
                     (add-system-dependencies module system modules system-vocabulary))
                   (if-let ((cell (cell (entry system-vocabulary name #'string=))))
                     (progn
                       (setf (car cell) :wanted)
                       (values modules (cons cell (remove cell system-vocabulary))))
                     (error "~@<Encountered a non-local dependency on an unknown system ~A.~:@>" name)))
                 (values modules system-vocabulary)))
             (add-visible-system (module name path type vocabulary known-visible &optional (actual-type (system-definition-type path)))
               (unless (eq type actual-type)
                 (error "~@<While operating in ~A mode, encountered an ~A at ~S.~:@>" type actual-type path))
               (let ((system (or (lret ((system (system name :if-does-not-exist :continue)))
                                   (when system
                                     (unless (typep system type)
                                       (error "~@<During dependency resolution: asked for a system ~S of type ~S, got one of type ~S~:@>"
                                              type name (type-of system)))))
                                 (register-new-system (intern (string-upcase name)) path type module))))
                 (set-syspath name path)
                 (ensure-system-loadable system path t locality)
                 (append vocabulary
                         (when (typep system 'asdf-system)
                           ;; A hidden system is a system definition residing in a file named differently from main system's name.
                           ;; Find them.
                           (iter (for hidden-system-name in (set-difference (asdf-hidden-system-names system) known-visible :test #'equal))
                                 (set-syspath hidden-system-name path)
                                 (let ((hidden-system (or (system hidden-system-name :if-does-not-exist :continue)
                                                          (register-new-system (intern hidden-system-name) path type module))))
                                   (ensure-system-loadable hidden-system path nil locality))
                                 (collect (if complete
                                              (make-wanted-missing hidden-system-name)
                                              (make-unwanted-missing hidden-system-name)))))))))
      (let* ((all-sysfiles (module-system-definitions module system-type locality))
             (main-sysfile (central-module-system-definition-pathname module system-type locality))
             (other-sysfiles (remove main-sysfile all-sysfiles)))
        ;; This doesn't deal with other modules providing same systems. Will silently break.
        (let* ((required-sysfiles (xform main-sysfile (curry #'cons main-sysfile) (when complete other-sysfiles)))
               (also-sysfiles (unless complete other-sysfiles))
               (required-names (mapcar (curry #'system-definition-name system-type) required-sysfiles))
               (also-names (mapcar (curry #'system-definition-name system-type) also-sysfiles))
               (all-names (append required-names also-names))
               (extended-system-vocabulary (append (mapcar #'make-wanted-missing required-names)
                                                   (mapcar #'make-unwanted-missing also-names)
                                                   system-vocabulary)))
          (iter (for sysfile in (append required-sysfiles also-sysfiles))
                (for name in (append required-names also-names))
                (setf extended-system-vocabulary (add-visible-system module name sysfile system-type extended-system-vocabulary all-names)))
          (iter (with modules)
                ;; Progress is made because NEXT-UNSATISFIED-SYSTEM proceeds from the head of the vocabulary,
                ;; where we've appended our required systems.
                (for (values modules-new new-extended-system-vocabulary) = (satisfy-next-system module system-type modules extended-system-vocabulary))
                (setf (values modules extended-system-vocabulary) (values modules-new new-extended-system-vocabulary))
                (when (not (iter (for (name wanted . satisfied) in extended-system-vocabulary)
                                 (finding name such-that (and wanted (not satisfied)))))
                  (return (values (remove-duplicates modules) extended-system-vocabulary)))))))))

(defgeneric satisfy-module (name &optional locality system-type complete skip-present module-vocabulary system-vocabulary)
  (:method ((name symbol) &optional (locality (gate *self*)) (system-type *default-system-type*) complete skip-present module-vocabulary system-vocabulary)
    (let ((cell (or (cdr (assoc name module-vocabulary))
                    (cdr (first (push (make-notprocessing-undone name) module-vocabulary)))))  ; extend the vocabulary
          (module (module name)))
      (cond
        ((car cell)
         (when (and skip-present (module-locally-present-p module locality))
           (setf (cdr cell) :done))
         (values module-vocabulary system-vocabulary))
        (t
         (setf (car cell) :processing)
         (when (not (and skip-present (module-locally-present-p module locality))) 
           (update module locality))
         (multiple-value-bind (module-deps new-system-vocabulary) (module-dependencies module locality system-type complete system-vocabulary)
           (let* ((new-deps-from-this-module (remove-if (rcurry #'assoc module-vocabulary) module-deps))
                  (new-module-vocabulary (append module-vocabulary (mapcar #'make-notprocessing-undone new-deps-from-this-module))))
             (syncformat t "~&~@<;; ~@;~S,~:[ no further dependencies~; added ~:*~A,~]~:@>~%" name new-deps-from-this-module)
             (multiple-value-prog1
                 (if new-deps-from-this-module
                     (satisfy-modules new-deps-from-this-module locality system-type complete skip-present new-module-vocabulary new-system-vocabulary)
                     (values new-module-vocabulary new-system-vocabulary)))))))))
  (:method :around ((name symbol) &optional (locality (gate *self*)) (system-type *default-system-type*) complete skip-present module-vocabulary system-vocabulary)
    (declare (ignore locality system-type complete skip-present module-vocabulary system-vocabulary))
    (multiple-value-bind (new-module-vocabulary new-system-vocabulary) (call-next-method)
      (let ((cell (cdr (assoc name new-module-vocabulary))))
        (assert cell)
        (syncformat t "~&~@<;; ~@;Done processing ~S, ~D left~:@>~%" name (count-if-not #'cddr new-module-vocabulary))
        (setf (cdr cell) :done)
        (values new-module-vocabulary new-system-vocabulary)))))

(defun satisfy-modules (module-names locality system-type complete skip-present module-vocabulary system-vocabulary &optional toplevel verbose)
  (iter (for module-name in module-names)
        (when verbose
          (syncformat t "~@<;;; ~@;modules: ~A~:@>~%" module-vocabulary)
          (syncformat t "~@<;;; ~@;systems: ~A~:@>~%" system-vocabulary))
        (for (values updated-module-vocabulary updated-system-vocabulary) = (satisfy-module module-name locality system-type complete skip-present module-vocabulary system-vocabulary))
        (setf (values module-vocabulary system-vocabulary) (values updated-module-vocabulary updated-system-vocabulary))
        (finally
         (when-let ((undone (and toplevel (mapcar #'car (remove-if #'cddr module-vocabulary)))))
           (format t "WARNING: after all gyrations following modules were left unsatisfied:~{ ~S~}~%" undone))
         (return (values module-vocabulary system-vocabulary)))))

(defun desire (desires &key complete skip-present (seal t) verbose)
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

Defined keywords:
   - SKIP-PRESENT - whether to skip updating specified modules which are 
     already present, defaults to nil,
   - SEAL - whether to commit any definition changes, and,
   - COMPLETE - whether to obtain all modules' systems, even those not
     part of main module systems' complete dependency graphs."
  (let* ((interpreted-desires (mapcar (curry #'xform-if-not #'consp (lambda (m) (list (name (module-best-distributor m)) m))) desires)))
    (iter (for (distributor-name . modules) in interpreted-desires)
          (for distributor = (distributor distributor-name))
          (when-let ((missing (remove-if (curry #'distributor-module-enabled-remote distributor) modules)))
            (error "~@<Distributor ~S does not provide following modules: ~S~:@>" distributor missing)))
    (let ((*desires* (substitute-desires *desires* (remove-if-not #'consp desires))) ; currently unused
          (desired-module-names (mapcar #'canonicalise-module-name (mapcan #'rest interpreted-desires)))
          (module-vocabulary nil)
          (system-vocabulary (mapcar #'make-unwanted-present *implementation-provided-systems*)))
      (syncformat t "; Satisfying desire for ~D module~:*~P:~%" (length desired-module-names))
      (satisfy-modules desired-module-names (gate *self*) *default-system-type* complete skip-present module-vocabulary system-vocabulary :sure-as-hell verbose))
    (when (and *unsaved-definition-changes-p* seal)
      (syncformat t "; Definitions modified and sealing was requested, committing changes.~%")
      (save-definitions :seal t))
    (syncformat t "; All done.~%")
    t))

(defun lust (&rest desires)
  "A spread interface function for DESIRE.
Updates present specified modules and skips present depended ones."
  (desire desires))
