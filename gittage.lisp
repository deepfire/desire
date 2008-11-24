;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :desire)


;;; Avoid a dependency.
(defun split-sequence (delimiter seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (test nil test-supplied) (test-not nil test-not-supplied) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied 
                             (list :test test))
                           (when test-not-supplied 
                             (list :test-not test-not))
                           (when key-supplied 
                             (list :key key)))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position delimiter seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position delimiter seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

(defun module-bare-p (module &optional (locality (master 'git)))
  (null (directory-exists-p (subdirectory* (module-locality-path module locality) ".git"))))

(defun (setf module-bare-p) (val module &optional (locality (master 'git)))
  (within-module-repository (dir module locality)
    (if val
        (error "not implemented")
        (progn
          (let ((git-files (directory (make-pathname :directory '(:relative) :name :wild))))
            (sb-posix:mkdir ".git" #o755)
            (dolist (filename git-files)
              (move-to-directory filename (make-pathname :directory '(:relative ".git") :name (pathname-name filename) :type (pathname-type filename)))))
          (git "config" "--replace-all" "core.bare" "false")
          (git "checkout" "master")
          (git "reset" "--hard")
          nil))))

(defun world-readable-p (module &optional (locality (master 'git)))
  (file-exists-p (subfile* (module-locality-path module locality) ".git" "git-daemon-export-ok")))

(defun (setf world-readable-p) (val module &optional (locality (master 'git)))
  (let ((path (subfile* (module-locality-path module locality) ".git" "git-daemon-export-ok")))
    (if val
        (with-open-file (s path :if-does-not-exist :create) t)
        (and (delete-file path) nil))))

(defun repo-var (module var &optional (locality (master 'git)))
  (declare (type git-locality locality) (type symbol var))
  (within-module-repository (dir module locality)
    (multiple-value-bind (status output)
        (run-external-program 'git (list "config" (string-downcase (symbol-name var))) :valid-exit-codes `((0 . nil) (1 . :unset)) :output t)
      (or status (string-right-trim '(#\Return #\Newline) output)))))

(defun (setf repo-var) (val module var &optional (locality (master 'git)))
  (declare (type git-locality locality) (type symbol var) (type string val))
  (within-module-repository (dir module locality)
    (run-external-program 'git (list "config" "--replace-all" (string-downcase (symbol-name var)) val))))

(defun module-gitremotes (module &optional (locality (master 'git)))
  (within-module-repository (dir module locality)
    (let ((output (external-program-output-as-string 'git "remote")))
      (mapcar (compose #'intern #'string-upcase) (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output))))))

(defun module-add-gitremote (module remote &aux (locality (master 'git)))
  (check-type remote git-remote)
  (within-directory (dir (module-locality-path module locality))
    (git "remote" "add" (down-case-name remote) (url remote module))))

(defun ensure-module-gitremote (module remote &aux (locality (master 'git)))
  (check-type remote git-remote)
  (unless (member (name remote) (module-gitremotes module locality))
    (module-add-gitremote module remote)))
