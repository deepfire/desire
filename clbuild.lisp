;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
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


(defparameter *known-clbuild-remote-dictionary* '((get_svn_clnet      . common-lisp.net-svn-rsync)
                                                  (get_cvs_clnet      . common-lisp.net-cvs-rsync)
                                                  (get_ediware        . common-lisp.net-luis-ediware)
                                                  (get_lichteblau_com . www.lichteblau.com)
                                                  (get_b9_com         . b9.com)
                                                  (get_clbuild_mirror . common-lisp.net-clbuild-mirror)
                                                  (get_xach_com       . git.xach.com)
                                                  (get_cvs_sfnet      . sourceforge-cvs-native)
                                                  (get_lispbuilder    . googlecode.com-svn-http)))

(defun recognise-clbuild-remote (remote-nickname)
  (if-let ((remote-name (cdr (assoc remote-nickname *known-clbuild-remote-dictionary*))))
    (remote remote-name)
    (let ((specifier (string remote-nickname)))
      (if (< (length specifier) 5)
          (error "~@<Malformed remote specifier ~S.~:@>" specifier)
          (let ((type (find-symbol (string-upcase (subseq specifier 4)) :desire)))
            (cond ((subtypep type 'vcs-type-mixin)
                   (values nil type))
                  ((string= "GET_CVS_FULL" specifier)
                   (values nil 'cvs))
                  (t
                   (error "~@<Malformed remote specifier ~S.~:@>" specifier))))))))

(defun clbuild-file-pathname (name)
  (subfile* (module-pathname :clbuild) name))

(defun steal-clbuild-projects-file (&optional filename verbose &aux
                                    (filename (or filename (clbuild-file-pathname "projects"))))
  (let ((*package* #.*package*)
        (*read-eval* nil)
        (*verbose-internalisation* verbose)
        (new-remotes (make-hash-table)))
    (with-container new-remotes (new-remotes :type list :iterator do-new-remotes :iterator-bind-key t :if-does-not-exist :continue :if-exists :continue)
      (with-open-file (s filename)
        (iter (for raw-string = (read-line s nil nil))
              (while raw-string)
              (for string = (subseq raw-string 0 (position #\# raw-string)))
              (when (zerop (length string))
                (next-iteration))
              (for (values module-name offset0) = (read-from-string string nil nil :start 0))
              (unless module-name
                (next-iteration))
              (format t ";; Module ~A, ~A~%" module-name (subseq string offset0))
              (for module = (module module-name :if-does-not-exist :continue))
              (for (values remote-name offset1) = (read-from-string string nil nil :start offset0))
              (unless remote-name
                (definition-error "~@<Malformed directive: no remote specifier for module ~A~:@>" module-name))
              ;; We get either the full information or just the VCS type.
              (for (values known-remote vcs-type) = (recognise-clbuild-remote remote-name))
              (let* ((spacepos (position #\Space string :start offset1))
                     (url (subseq string offset1 spacepos))
                     (posturl (let ((*readtable* (copy-readtable)))
                                (setf (readtable-case *readtable*) :preserve)
                                (when spacepos (when-let ((posturl (read-from-string string nil nil :start spacepos)))
                                                 (princ-to-string posturl))))))
                (multiple-value-bind (remote cred i maybe-umbrella-name created-p)
                    (or known-remote (ensure-url-remote url module-name :vcs-type-hint vcs-type))
                  (declare (ignore i))
                  (unless remote
                    (next-iteration))
                  (when created-p
                    (push (list remote url module-name) (new-remotes (remote-distributor remote))))
                  (unless (find module-name (location-module-names remote))
                    (ensure-remote-module remote module-name (or maybe-umbrella-name module-name) :credentials cred)
                    (when (typep remote 'cvs)
                      (let* ((default-cvs-module-name (down-case-string module-name))
                             (cvs-module-name (or posturl default-cvs-module-name)))
                        (unless (string= cvs-module-name default-cvs-module-name)
                          (push (list module-name cvs-module-name) (wrinkles remote))))))))))
      (do-new-remotes (distributor new-remotes)
        (flet ((module-url-reformulable (name url current remotes)
                 (multiple-value-bind (type cred hostname port path) (parse-remote-namestring url :slashless (typep current 'cvs-native-remote) :type-hint (vcs-type current))
                   (declare (ignore cred)) ; XXX: is it the right thing to do?
                   (match-module-url-components-against-remote-set hostname nil port path nil (down-case-string name) ; XXX: do we waste the *UMBRELLA* matcher here?
                                                                   (remove-if-not (curry #'remote-types-compatible-p type) remotes)))))
          (iter (for (new-remote module-url module-name) in new-remotes)
                (when-let* ((module-name (when (null (rest (location-module-names new-remote))) ; we're after single-module remotes
                                           (first (location-module-names new-remote))))
                            (better-remote (module-url-reformulable module-name module-url new-remote
                                                                    (remove new-remote (mapcar #'car new-remotes)))))
                  (syncformat t ";; =======================================~%")
                  (format t "~@<;; ~@;remote ~A is excessive, its module (~A) is better reformulated in ~A/~S~:@>~%"
                          (name new-remote) module-name (name better-remote) (remote-path better-remote))
                  (remote-link-module better-remote (module module-name)
                                      :module-module (when (typep new-remote 'cvs)
                                                       (remote-module-wrinkle new-remote module-name)))
                  (remove-remote new-remote :keep-modules t))))))))