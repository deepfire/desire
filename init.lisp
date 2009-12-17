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


(defun enumerate-host-systems ()
  (let* ((name (canonicalise-name ""))
         (host-module (make-instance 'host-module :name name :umbrella name :last-sync-time (get-universal-time) :synchronised-p t
                                     :scan-positive-localities (list (gate *self*)))))
    (dolist (sname *implementation-provided-system-names*)
      (make-instance 'asdf-system :name (canonicalise-name sname) :module host-module :direct-dependency-names nil :dependencies nil))))

(defun ensure-root-sanity (directory)
  (unless (directory-exists-p directory)
    (desire-error "~@<The specified root at ~S does not exist.~:@>" directory))
  (let ((gitroot (subdirectory* directory "git")))
    (when (and (probe-file gitroot) (not (directory-exists-p gitroot)))
      (desire-error "~@<The specified root at ~S contains a file named 'git', which violates the requirement for a sane root.~:@>" directory))
    (ensure-directories-exist gitroot)
    (ensure-directories-exist (subdirectory* directory "tmp")))
  directory)

(defun ensure-committer-identity ()
  (unless (gitvar 'user.name)
    (let ((username (format nil "Desire operator on ~A" (down-case-name *self*))))
      (syncformat t "~@<;;; ~@;Setting git user name to ~S~:@>~%" username)
      (setf (gitvar 'user.name nil t) username))))

(defun determine-tools-and-update-remote-accessibility ()
  "Find out which and where VCS tools are available and disable correspondingly inaccessible remotes."
  (let ((present (cons *gate-vcs-type* (unzip #'find-and-register-tools-for-remote-type (set-difference *supported-vcs-types* (list *gate-vcs-type*))))))
    (do-remotes (r)
      (setf (remote-disabled-p r) (not (member (vcs-type r) present))))
    (find-executable 'make)
    (find-executable 'cp)))

(defun init (path &key as (merge-remote-wishmasters *merge-remote-wishmasters*) (wishmaster-branch :master) verbose)
  "Make Desire fully functional, with PATH chosen as storage location.

AS, when specified, will be interpreted as a distributor name, whose
definition will be looked up. Consequently, an attempt to establish
an identity relationship with that definition will be performed,
by looking up locally the modules defined for export. The rest of
locally present modules will be marked as converted."
  (let* ((path (fad:pathname-as-directory path))
         (absolute-path (if (pathname-absolute-p path)
                            path
                            (merge-pathnames path))))
    ;;
    ;; Storage
    ;;
    (ensure-root-sanity (parse-namestring absolute-path))
    (let* ((gate-path (merge-pathnames (make-pathname :directory (list :relative (downstring *gate-vcs-type*))) absolute-path))
           (meta-path (merge-pathnames #p".meta/" gate-path))
           (localmeta-path (merge-pathnames #p".local-meta/" gate-path)))
      ;;
      ;; Reset
      ;;
      (clear-definitions)
      ;;
      ;; Set up tools
      ;;
      ;; NOTE: there's some profound crap there 
      (with-class-slot (git hg darcs cvs svn tarball) required-executables
        (setf git '(git) hg '(hg python)  darcs '(darcs darcs-to-git wget) cvs '(rsync git cvs) svn '(rsync git) tarball '(git)))
      (with-class-slot (git hg darcs cvs svn tarball) enabled-p
        (setf git nil hg nil darcs nil cvs nil svn nil tarball nil))
      (unless (find-and-register-tools-for-remote-type *gate-vcs-type*)
        (desire-error "The executable of gate VCS (~A) is missing, and so, DESIRE is of no use." *gate-vcs-type*))
      (ensure-committer-identity)
      ;;
      ;; Get initial definitions
      ;;
      (unless (metastore-present-p meta-path '(definitions))
        (syncformat t "~@<;;; ~@;No metastore found in ~S, bootstrapping from ~S~:@>~%" meta-path *bootstrap-wishmaster-url*)
        (clone-metastore *bootstrap-wishmaster-url* gate-path wishmaster-branch))
      (syncformat t "~@<;;; ~@;Loading definitions from ~S~:@>~%" (metafile-path 'definitions meta-path))
      (read-definitions :force-source t :metastore meta-path)
      ;;
      ;; Set up *SELF* and complete definitions
      ;;
      (setf *self* (with-measured-time-lapse (sec)
                       (if-let ((d (and as (distributor as))))
                         (progn (syncformat t "~@<;;; ~@;Trying to establish self as ~A~:@>~%" as)
                                (change-class d 'local-distributor :root absolute-path :meta meta-path :localmeta localmeta-path))
                         (let ((local-name (canonicalise-name (machine-instance))))
                           (syncformat t "~@<;;; ~@;Establishing self as non-well-known distributor ~A~:@>~%" local-name)
                           (make-instance 'local-distributor :name local-name :root absolute-path :meta meta-path :localmeta localmeta-path
                                          :omit-registration t)))
                     (when verbose
                       (syncformat t ";;; Scanned locality for module presence in ~D seconds.~%" sec))))
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
      (determine-tools-and-update-remote-accessibility)
      ;;
      ;; Generic part of system loadability
      ;;
      (syncformat t "~@<;;; ~@;Registering gate locality ~S with system backend ~A~:@>~%" (locality-pathname (gate *self*)) *default-system-type*)
      (register-locality-with-system-backend *default-system-type* (gate *self*))
      ;;
      ;; Per-repository branch model maintenance, system discovery and loadability
      ;;
      (syncformat t ";;; Massaging present modules~%")
      (with-measured-time-lapse (sec)
          (do-present-modules (module)
            (when verbose
              (syncformat t ";;; Processing module ~A~%" (name module)))
            (notice-module-repository module nil))
        (when verbose
          (syncformat t ";;; Ensured branches and performed system discovery in ~D seconds.~%" sec)))
      ;;
      ;; System dependency calculation, in bulk
      ;;
      (with-measured-time-lapse (sec) (recompute-direct-system-dependencies :verbose verbose)
        (when verbose
          (syncformat t ";;; Queried direct system dependencies in ~D seconds.~%" sec)))
      (enumerate-host-systems)
      (with-measured-time-lapse (sec) (recompute-full-system-dependencies :verbose verbose)
        (when verbose
          (syncformat t ";;; Computed full system dependencies in ~D seconds.~%" sec)))
      (format t "~@<;;; ~@;Mod~@<ules present locally:~{ ~A~}~:@>~:@>~%" 
              (sort (do-modules (m)
                      (when (module-scan-positive-localities m)
                        (collect (string (name m)))))
                    #'string<))
      ;;
      ;; Quirks
      ;;
      (syncformat t ";;; Tweaking environment for CL-LAUNCH~%")
      (sb-posix:putenv "LISP_FASL_CACHE=NIL")
      (syncformat t ";;; All done~%")
      (values))))

(defun reinit ()
  "Execute INIT with the arguments that were passed to it last time."
  (init (root *self*) :as (when (distributor (name *self*) :if-does-not-exist :continue)
                            (name *self*))))

(defun reload-definitions (&key reset-metastore)
  "Lose all unsaved definitions by replacing them with those from the metastore."
  (clear-definitions)
  (when reset-metastore
    (reset-metastore))
  (read-definitions :force-source t)
  ;; Metastore subscriptions?
  )

(defun self-check ()
  #+sbcl
  (= (sb-kernel::get-lisp-obj-address (distributor :git.feelingofgreen.ru))
     (sb-kernel::get-lisp-obj-address *self*)))