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


(define-executable patch)

(defvar *xcvbifier-base-uri* "http://common-lisp.net/project/xcvb/releases/patches/")
(defvar *xcvbifiable-module-set* '())

(defun git-apply-diff (filename &optional directory (add-to-index t) (error-on-failure t))
  (maybe-within-directory directory
    (multiple-value-bind (successp output) (with-explanation ("applying gitdiff ~S in ~S" filename *default-pathname-defaults*)
                                             (with-shell-predicate
                                                 (apply #'git "apply" filename (append (when add-to-index '("--index"))))))
      (cond (successp)
            (t
             (git-set-branch-index-tree)
             (if error-on-failure
                 (error "~@<Failed to apply ~S in ~S:~%~A.~:@>" filename *default-pathname-defaults* output)
                 (values nil output)))))))

(defun xcvbify-module (module &optional break-on-patch-failure)
  (update-module module)       ; leaves the repo in inconsistent state
  (within-directory ((module-pathname module))
    (git-set-branch-index-tree)
    (git-set-head-index-tree :master :if-changes :reset)
    (git-set-branch :xcvbify)
    (git-set-head-index-tree :xcvbify :if-changes :reset)
    (unless (file-exists-p "build.xcvb")
      (with-file-from-www (".xcvbifier.diff" `(,*xcvbifier-base-uri* ,(down-case-name module) ".diff"))
        (multiple-value-bind (successp output) (git-apply-diff ".xcvbifier.diff" nil t nil)
          (cond (successp
                 (with-explanation ("committing xcvbification change")
                   (git "commit" "-m" "Xcvbify.")))
                (t
                 (git-set-head-index-tree :master nil :if-changes :reset)
                 (git-remove-branch :xcvbify)
                 (let ((control-string "~@<;; ~@;failed to apply XCVBification diff to ~A:~%~A~:@>~%"))
                   (if break-on-patch-failure
                       (break control-string (name module) output)
                       (format t control-string (name module) output)))
                 (values nil output))))))))

(defun update-xcvbification (&optional break-on-patch-failure)
  (find-executable 'patch)
  (let ((available-diff-names (remove-if-not (curry #'search ".diff") (list-www-directory *xcvbifier-base-uri*))))
    (multiple-value-bind (modules unknown-names) (iter (for diff-name in available-diff-names)
                                                       (let ((name (string-upcase (subseq diff-name 0 (- (length diff-name) 5)))))
                                                         (if-let ((m (module name :if-does-not-exist :continue)))
                                                           (collect m into modules)
                                                           (collect name into module-names))
                                                         (finally (return (values modules module-names)))))
      (format t "~@<; ~@;will XCVBify following modules, using Fare's patches: ~{ ~A~}~:@>~%" (mapcar #'name modules))
      (when unknown-names
        (format t "~@<; ~@;following XCVBifiable modules are unknown: ~{ ~A~}~:@>~%" unknown-names))
      (dolist (m modules)
        (xcvbify-module m break-on-patch-failure)))))