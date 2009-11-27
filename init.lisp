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


(defun ensure-root-sanity (directory)
  (unless (directory-exists-p directory)
    (error "~@<The specified root at ~S does not exist.~:@>" directory))
  (let ((gitroot (subdirectory* directory "git")))
    (when (and (probe-file gitroot) (not (directory-exists-p gitroot)))
      (error "~@<The specified root at ~S contains a file named 'git', which violates the requirement for a sane root.~:@>" directory))
    (ensure-directories-exist gitroot)
    (ensure-directories-exist (subdirectory* directory "tmp")))
  directory)

(defun init (path &key as (merge-remote-wishmasters *merge-remote-wishmasters*) (wishmaster-branch :master))
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
    (ensure-root-sanity (parse-namestring absolute-path))
    (let* ((gate-path (merge-pathnames (make-pathname :directory (list :relative (downstring *gate-vcs-type*))) absolute-path))
           (meta-path (merge-pathnames #p".meta/" gate-path))
           (localmeta-path (merge-pathnames #p".local-meta/" gate-path)))
      (clear-definitions)
      ;; this is crap
      (with-class-slot (git hg darcs cvs svn tarball) required-executables
        (setf git '(git) hg '(hg python)  darcs '(darcs darcs-to-git wget) cvs '(rsync git cvs) svn '(rsync git) tarball '(git)))
      ;; this is crap just as well
      (with-class-slot (git hg darcs cvs svn tarball) enabled-p
        (setf git nil hg nil darcs nil cvs nil svn nil tarball nil))
      (unless (find-and-register-tools-for-remote-type *gate-vcs-type*)
        (error "The executable of gate VCS (~A) is missing, and so, DESIRE is of no use." *gate-vcs-type*))
      (unless (metastore-present-p meta-path '(definitions))
        (syncformat t ";;; no metastore found in ~S, bootstrapping from ~S~%" meta-path *bootstrap-wishmaster-url*)
        (clone-metastore *bootstrap-wishmaster-url* gate-path wishmaster-branch))
      (syncformat t ";;; loading definitions from ~S~%" (metafile-path 'definitions meta-path))
      (read-definitions :force-source t :metastore meta-path)
      (setf *self* (if-let ((d (and as (distributor as))))
                     (progn (syncformat t ";;; trying to establish self as ~A~%" as)
                            (change-class d 'local-distributor :root absolute-path :meta meta-path :localmeta localmeta-path))
                     (let ((local-name (intern (string-upcase (machine-instance)) #.*package*)))
                       (syncformat t ";;; establishing self as non-well-known distributor ~A~%" local-name)
                       (make-instance 'local-distributor :name local-name :root absolute-path :meta meta-path :localmeta localmeta-path
                                      :omit-registration t))))
      (ensure-metastore localmeta-path :required-metafiles '(definitions) :public nil)
      (read-local-definitions :metastore localmeta-path)
      (unless (gitvar 'user.name)
        (let ((username (format nil "Desire operator on ~A" (down-case-name *self*))))
          (syncformat t ";;; setting git user name to ~S~%" username)
          (setf (gitvar 'user.name nil t) username)))
      (reestablish-metastore-subscriptions meta-path)
      (when merge-remote-wishmasters
        (syncformat t ";;; merging definitions from remote wishmasters...~%")
        (merge-remote-wishmasters))
      (setf *unsaved-definition-changes-p* nil)
      (syncformat t ";;; determining available tools and deducing accessible remotes~%")
      (determine-tools-and-update-remote-accessibility)
      ;; do some gate maintenance
      (let ((gate (gate *self*)))
        (syncformat t ";;; registering ~S with ASDF~%" (locality-asdf-registry-path gate))
        (locality-register-with-asdf gate)
        (syncformat t ";;; massaging present modules~%")
        (do-present-modules (module gate)
          (let ((repo-dir (module-pathname module gate)))
            (ensure-tracker-branch repo-dir (ref-value '("heads" "master") repo-dir))
            (ensure-module-systems-loadable module gate))))
      (syncformat t ";;; tweaking environment for CL-LAUNCH~%")
      (sb-posix:putenv "LISP_FASL_CACHE=NIL")
      (syncformat t ";;; all done~%")
      (values))))

(defun reinit ()
  "Execute INIT with the arguments that were passed to it last time."
  (init (root *self*) :as (when (distributor (name *self*) :if-does-not-exist :continue)
                            (name *self*))))