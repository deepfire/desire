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

(defun xcvbify-module (module &optional break-on-patch-failure &aux
                       (repo (module-pathname module)))
  "'I know what I do' mode: silently resets stuff."
  (update module)
  (gittage:with-repository-write-access (new-p) repo
    (declare (ignore new-p))
    (let ((saved-head (gittage:get-head nil repo))
          (saved-xcvbify (gittage:ref-value '("heads" "xcvbify") nil :if-does-not-exist :continue)))
      (gittage:set-head-index-tree :tracker :reset)
      (gittage:set-branch :xcvbify)
      (gittage:set-head-index-tree :xcvbify :error)
      (unless (file-exists-p "build.xcvb")
        (with-file-from-www (".xcvbifier.diff" `(,*xcvbifier-base-uri* ,(down-case-name module) ".diff"))
          (multiple-value-bind (successp output) (gittage:apply-diff ".xcvbifier.diff" *repository* t nil)
            (cond (successp
                   (with-explanation ("committing xcvbification change")
                     (git *repository* "commit" "-m" "Xcvbify.")))
                  (t
                   (gittage:set-head-index-tree saved-head :reset)
                   (if saved-head
                       (gittage:set-branch :xcvbify nil saved-xcvbify)
                       (gittage:remove-branch :xcvbify))
                   (let ((control-string "~@<;; ~@;failed to apply XCVBification diff to ~A:~%~A~:@>~%"))
                     (if break-on-patch-failure
                         (break control-string (name module) output)
                         (format t control-string (name module) output)))
                   (values nil output)))))))))

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