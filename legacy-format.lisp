;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLING; Base: 10 -*-
;;;
;;;  (c) copyright 2007-2008 by
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

(in-package :cling)

(defmacro defdistributor (dist-name &rest definitions)
  "The legacy CL world description format. Still working."
  (flet ((collate (method)
           (ecase method
             (git-native (values 'git-native-remote 'git))
             (git-http (values 'git-http-remote 'git))
             (hg-http (values 'hg-http-remote 'hg))
             (darcs-http (values 'darcs-http-remote 'darcs))
             ;; XXX:
             ;; The story below sums up as follows:
             ;; - we want to be able to mix the unified transport type and differentiable RCS types
             ;; - both cvs and svn have unified transport -- rsync, yet obviously differentiate on the latter
             (rsync (values 'rsync-remote 'rsync))
             ((cvs-rsync svn-rsync) (values 'rsync-remote 'rsync)))))
    (let* ((path-forms (rest (find :url-schemas definitions :key #'first)))
           (mod-specs (iter (for (rcs-proto . mod-specs) in (rest (find :modules definitions :key #'first)))
                            (collect (cons rcs-proto (iter (for mod-spec in mod-specs)
                                                           (appending
                                                            (destructuring-bind (umbrella-spec . module-specs) ;; attach an umbrella spec to each module spec
                                                                (if (consp mod-spec) mod-spec (list mod-spec mod-spec))
                                                              (iter (for module-spec in module-specs)
                                                                    (collect (list* umbrella-spec (ensure-cons module-spec))))))))))))
      (with-gensyms (remote-var)
        `(progn
           (unless (distributor ',dist-name :if-does-not-exist :continue)
             (make-instance 'distributor :name ',dist-name))
           ,@(iter (for (remote-type-spec . remote-path-spec) in path-forms)
                   (for (values remote-type nil) = (collate remote-type-spec))
                   (destructuring-bind ((repovar &rest params &key name &allow-other-keys) &rest body) remote-path-spec
                     (appending `((if-let* ((,remote-var (find '((,repovar) ,@body) (distributor-remotes (distributor ',dist-name))
                                                               :test #'equal :key #'remote-path-form)))
                                           (warn "~@<skipping redefinition of remote with path form ~S~:@>" '((,repovar) ,@body))
                                           (make-instance ',remote-type ,@(when name `(:name ',name))
                                                          :distributor (distributor ',dist-name)
                                                          :path-form '((,repovar) ,@body)
                                                          :path-fn (lambda (,repovar) (list ,@body))
                                                          ,@(remove-from-plist params :name)))))))
           ;; collate the default remote name for the module
           ,@(iter (for (rcs-proto-spec . module-spec-list) in mod-specs)
                   (for (rcs-proto . params) = (ensure-cons rcs-proto-spec)) ;; destructure the protocol-level module groupings
                   (for (values nil rcs-type) = (collate rcs-proto))
                   (for default-remote-name-from-protocol-grouping = (or (getf params :remote) (default-remote-name dist-name rcs-type)))
                   (appending
                    (iter (for (umbrella-spec modname . args) in module-spec-list) ;; destructure the umbrella-level module groupings
                          (destructuring-bind (&rest rest &key (systems (list modname)) &allow-other-keys) args
                            (when-let ((unhandled (remove-from-plist rest :systems)))
                              (warn "Unhandled module parameters: ~S~%" unhandled))
                            (destructuring-bind (umbrella &key (remote default-remote-name-from-protocol-grouping)) (ensure-cons umbrella-spec)
                              (appending (list* `(if-let ((remote (find ',remote (distributor-remotes (distributor ',dist-name)) :key #'name)))
                                                         (push ',modname (location-modules remote))
                                                         (error "~@<Unable to find remote ~S in distributor ~S~:@>" ',remote (distributor ',dist-name)))
                                                `(unless (module ',modname :if-does-not-exist :continue)
                                                   (make-instance 'module :name ',modname :umbrella ',umbrella))
                                                (iter (for system in systems)
                                                      (destructuring-bind (name &rest rest &key relativity &allow-other-keys) (ensure-cons system)
                                                        (collect `(make-instance 'system :name ',name :module (module ',modname)
                                                                                 ,@(when relativity `(:relativity ',relativity))
                                                                                 ,@(remove-from-plist rest :relativity)))))))))))))))))

(defmacro define-module-dependencies (&body body)
  `(iter (for (module-name . dependencies) in '(,@body))
         (for module = (module module-name))
         (setf (nonleaf module-name) module)
         (dolist (dep dependencies)
           (setf (leaf dep) (module dep))
           (depend module (module dep)))))