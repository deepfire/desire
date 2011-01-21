;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
;;;
;;;  (c) copyright 2007-2011 by
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


(defparameter *vcs-appendage-types* '(darcs hg))

(defun ensure-root-sanity (directory)
  (unless (directory-exists-p directory)
    (desire-error "~@<The specified storage area at ~S does not exist.~:@>" directory))
  (ensure-directories-exist (subdirectory* directory "tmp"))
  directory)

(defun ensure-committer-identity ()
  (unless (gitvar 'user.name)
    (let ((username (format nil "Desire operator on ~A" (down-case-name *self*))))
      (syncformat t "~@<;;; ~@;Setting git user name to ~S~:@>~%" username)
      (setf (gitvar 'user.name nil t) username))))

(defun module-wildfile (module name &key type &aux
                        (module (coerce-to-module module)))
  (and (or (module-locally-present-p module (gate *self*) t)
           (update module)
           t)
       (first (directory (subfile (module-pathname module) (list :wild-inferiors name) :type type)))))

(defgeneric try-ensure-importer-executable (type)
  (:method :around (o)
    (ignore-errors
      (call-next-method)))
  (:method ((o (eql 'darcs)))
    (when-let ((executable (module-wildfile :bzr-fastimport "darcs-fast-export")))
      (setf (executable 'darcs-fast-export) executable)))
  (:method ((o (eql 'hg)))
    (when-let ((executable (module-wildfile :bzr-fastimport "hg-fast-export" :type "py")))
      (setf (executable 'hg-fast-export) executable))))

(defun determine-tools-and-update-remote-accessibility ()
  "Find out which and where VCS tools are available and disable correspondingly inaccessible remotes."
  (let ((present (cons *gate-vcs-type* (unzip #'find-and-register-tools-for-remote-type (set-difference *supported-vcs-types* (list *gate-vcs-type*))))))
    (do-remotes (r)
      (setf (remote-disabled-p r) (not (member (vcs-type r) present))))
    (find-executable 'wget)
    (find-executable 'make)
    (find-executable 'cp)
    (find-executable 'gzip)
    (find-executable 'gpg)))

(defun init (path &key
             as
             bootstrap-url
             (wishmaster-name *default-bootstrap-wishmaster-name*)
             http-proxy
             (wishmaster-http-suffix *default-bootstrap-wishmaster-http-suffix*)
             (merge-remote-wishmasters *merge-remote-wishmasters*) (wishmaster-branch :master) verbose)
  "Make Desire fully functional, with PATH chosen as storage location.

AS, when specified, will be interpreted as a distributor name, whose
definition will be looked up. Consequently, an attempt to establish
an identity relationship with that definition will be performed,
by looking up locally the modules defined for export. The rest of
locally present modules will be marked as converted."
  (flet ((default-bootstrap-wishmaster-urls ()
           (values (or bootstrap-url (strconcat* "git://" wishmaster-name "/"))
                   (strconcat* "http://" wishmaster-name "/" wishmaster-http-suffix))))
    (let* ((path (fad:pathname-as-directory path))
           (gate-path (lret ((gate-path (if (pathname-absolute-p path)
                                            path
                                            (merge-pathnames path))))
                        (ensure-root-sanity (parse-namestring gate-path))))
           (meta-path (merge-pathnames #p".meta/" gate-path))
           (localmeta-path (merge-pathnames #p".local-meta/" gate-path))
           (need-bootstrap-p (not (metastore-present-p meta-path '(definitions)))))
      (clear-definitions)
      ;;
      ;; Set up tools
      ;;
      ;; NOTE: there's some profound crap there
      (with-class-slot (git hg darcs cvs svn tarball) required-executables
        (setf git '(git) hg '(hg python)  darcs '(darcs darcs-fast-export wget) cvs '(rsync git cvs) svn '(rsync git) tarball '(git)))
      (with-class-slot (git hg darcs cvs svn tarball) enabled-p
        (setf git nil hg nil darcs nil cvs nil svn nil tarball nil))
      (unless (find-and-register-tools-for-remote-type *gate-vcs-type*)
        (desire-error "The executable of gate VCS (~A) is missing, and so, DESIRE is of no use." *gate-vcs-type*))
      (ensure-committer-identity)
      ;;
      ;; Bootstrap domain knowledge
      ;;
      (when need-bootstrap-p
        (multiple-value-bind (bootstrap-url bootstrap-http-url) (default-bootstrap-wishmaster-urls)
          (syncformat t "~@<;;; ~@;No metastore found in ~S, bootstrapping from ~S ~
                                  (with HTTP fallback to ~S~:[~;, and proxy ~:*~S~])~:@>~%"
                      meta-path bootstrap-url bootstrap-http-url http-proxy)
          (setf *http-proxy* http-proxy)
          (clone-metastore bootstrap-url bootstrap-http-url meta-path wishmaster-branch)))
      (syncformat t "~@<;;; ~@;Loading definitions from ~S~:@>~%" (metafile-path 'definitions meta-path))
      (let ((last-author (read-definitions :force-source t :metastore meta-path)))
        (syncformat t "~@<;;; ~@;Last author of DEFINITIONS just read: ~A~:@>~%"
                    (if last-author (name last-author) "Unknown")))
      ;;
      ;; Set up *SELF* and complete definitions
      ;;
      (setf *self* (with-measured-time-lapse (sec)
                       (if-let ((d (and as (distributor as))))
                         (progn (syncformat t "~@<;;; ~@;Trying to establish self as ~A~:@>~%" as)
                                (change-class d 'local-distributor :root gate-path :meta meta-path :localmeta localmeta-path))
                         (let ((local-name (choose-local-name gate-path)))
                           (syncformat t "~@<;;; ~@;Establishing self as non-well-known distributor ~A~:@>~%" local-name)
                           (make-instance 'local-distributor :name local-name :root gate-path :meta meta-path :localmeta localmeta-path)))
                     (when verbose
                       (syncformat t ";;; Scanned locality for module presence in ~D seconds.~%" sec))))
      (ensure-metastore localmeta-path :required-metafiles '(definitions) :public nil)
      (read-local-definitions :metastore localmeta-path)
      (reestablish-metastore-subscriptions meta-path)
      (when merge-remote-wishmasters
        (syncformat t ";;; Merging definitions from remote wishmasters...~%")
        (merge-remote-wishmasters))
      (setf *unsaved-definition-changes-p* nil)
      ;;
      ;; Set up tools for import
      ;;
      (syncformat t ";;; Determining available import-related tools and deducing accessible remotes~%")
      (dolist (type *vcs-appendage-types*)
        (try-ensure-importer-executable type))
      (determine-tools-and-update-remote-accessibility)
      ;;
      ;; Generic part of system loadability
      ;;
      (syncformat t "~@<;;; ~@;Registering gate locality ~S with system backend ~A~:@>~%" (locality-pathname (gate *self*)) *default-system-type*)
      (register-locality-with-system-backend *default-system-type* (gate *self*))
      ;;
      ;; Per-repository branch model maintenance, system discovery and loadability
      ;;
      (syncformat t ";;; Enumerating present modules and systems~%")
      (enumerate-present-modules-and-systems :verbose verbose)
      (format t "~@<;;; ~@;Mod~@<ules present locally:~{ ~A~}~:@>~:@>~%"
              (sort (do-present-modules (m)
                      (collect (name m)))
                    #'string<))
      ;;
      ;; Evolve definitions.
      ;;
      (save-definitions :commit-message "Init-time seal." :seal t)
      ;;
      ;; Quirks
      ;;
      (syncformat t ";;; Tweaking environment for CL-LAUNCH~%")
      (setenv "LISP_FASL_CACHE" "NIL")
      (syncformat t ";;; All done~%")
      (values))))

(defun reinit ()
  "Execute INIT with the arguments that were passed to it last time."
  (init (root *self*) :as (when (distributor (name *self*) :if-does-not-exist :continue)
                            (name *self*))))

(defun reinstall-old-self-and-fix-gate (self)
  (lret ((incumbent-gate (gate (distributor (name self))))
         (old-gate (gate self)))
    (change-class incumbent-gate 'git-gate-locality :pathname (locality-pathname old-gate))
    (setf (distributor (name self)) self
          (gate self) incumbent-gate
          (remote-distributor incumbent-gate) self
          (distributor-remotes self) (list incumbent-gate))
    (update-local-distributor-conversions self (gate-converted-module-names old-gate))))

(defun carry-over-module-locality-presence-cache-from-old-gate (old-gate self)
  (let ((fixed-gate (gate self)))
    ;; restore module locality presence cache
    (let ((oldmodule-scapolos (make-hash-table :test 'eq))
          (old-gate-modules (append (location-module-names old-gate)
                                    (gate-converted-module-names old-gate)
                                    (gate-unpublished-module-names old-gate)
                                    (gate-hidden-module-names old-gate)))
          (fixed-gate-modules (append (location-module-names fixed-gate)
                                      (gate-converted-module-names fixed-gate)
                                      (gate-unpublished-module-names fixed-gate)
                                      (gate-hidden-module-names fixed-gate))))
      (with-container oldmodule-scapolos (oldmodule-scapolos :type list)
        (dolist (mname old-gate-modules)
          (when-let ((m (module mname :if-does-not-exist :continue)))
            (setf (oldmodule-scapolos mname) (module-scan-positive-localities m))))
        (dolist (mname fixed-gate-modules)
          (setf (module-scan-positive-localities (module mname)) (when (oldmodule-scapolos mname)
                                                                   (list fixed-gate))))))))

(defun reload-definitions (&key reset-metastore)
  "Lose all unsaved definitions by replacing them with those from the metastore,
without losing *SELF*.
This is a complex operation, because not losing *SELF* implies that we have
to patch the newfangled world according to that."
  (clear-definitions)
  (when reset-metastore
    (reset-metastore))
  (read-definitions :force-source t)
  (let ((old-gate (reinstall-old-self-and-fix-gate *self*)))
    ;; now we can refill the hidden and unpublished slots
    (read-local-definitions)
    (carry-over-module-locality-presence-cache-from-old-gate old-gate *self*))
  ;; Metastore subscriptions?
  )

(defun self-check ()
  (eq *self* (distributor :git.feelingofgreen.ru)))