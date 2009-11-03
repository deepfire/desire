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

(defun list-www-directory (url)
  (with-explanation ("listing contents of WWW directory ~S" url)
    (let ((output (nth-value 1 (with-captured-executable-output
                                 (wget url "-O" "-")))))
      (nthcdr 5 (iter (with posn = 0)
                      (for href-posn = (search "HREF=\"" output :start2 posn))
                      (while href-posn)
                      (for start-posn = (+ href-posn 6))
                      (for close-posn = (position #\" output :start start-posn))
                      (when close-posn
                        (collect (subseq output start-posn close-posn)))
                      (setf posn (1+ close-posn)))))))

(defun invoke-with-file-from-www (filename url fn)
  (unwind-protect (progn (wget url "-O" filename)
                         (funcall fn))
    (delete-file filename)))

(defmacro with-file-from-www ((filename url) &body body)
  `(invoke-with-file-from-www ,filename ,url (lambda () ,@body)))

(defun apply-diff (filename &optional directory &key inhibit-rejects (if-fails :error))
  (maybe-within-directory directory
    (cond ((with-valid-exit-codes ((1 nil)
                                   (2 nil))
             (with-explanation ("applying diff ~S in ~S" filename *default-pathname-defaults*)
               (apply #'patch "-p1" "-i" filename (when inhibit-rejects
                                                    '(#-win32 "--global-rejects-file=/dev/null"
                                                      #+win32 "nul"))))))
          (t
           (git-repository-reset-hard)
           (ecase if-fails
             (:error (error "~@<Failed to apply ~S in ~S.~:@>" filename *default-pathname-defaults*))
             (:continue nil))))))

(defun xcvbify-module (module)
  (update-module module)       ; leaves the repo in inconsistent state
  (within-directory ((module-pathname module))
    (git-repository-reset-hard)
    (set-gitbranch :xcvbify *default-pathname-defaults*)
    (checkout-gitbranch :xcvbify *default-pathname-defaults*)
    (with-file-from-www (".xcvbifier.diff" `(,*xcvbifier-base-uri* ,(down-case-name module) ".diff"))
      (unless (apply-diff ".xcvbifier.diff" nil :inhibit-rejects t :if-fails :continue)
        (remove-gitbranch :xcvbify)
        (format t ";; failed to apply XCVBification diff to ~A" (name module))))
    (with-explanation ("committing xcvbification change")
      (git "commit" "-a" "-m" "Xcvbify."))))

(defun update-xcvbification ()
  (find-executable 'patch)
  (let ((available-diff-names (remove-if-not (curry #'search ".diff") (list-www-directory *xcvbifier-base-uri*))))
    (multiple-value-bind (modules unknown-names) (iter (for diff-name in available-diff-names)
                                                       (let ((name (string-upcase (subseq diff-name 0 (- (length diff-name) 5)))))
                                                         (if-let ((m (module name :if-does-not-exist :continue)))
                                                           (collect m into modules)
                                                           (collect name into module-names))
                                                         (finally (return (values modules module-names)))))
      (format t "~@<; ~@;will XCVBify following modules, using Fare's patches:~{ ~A~}~:@>~%" (mapcar #'name modules))
      (format t "~@<; ~@;following XCVBifiable modules are unknown: ~{ ~A~}~:@>~%" unknown-names)
      (dolist (m modules)
        (xcvbify-module m)))))