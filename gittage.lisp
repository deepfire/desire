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


(define-condition vcs-condition ()
  ((vcs :accessor condition-vcs :initarg :vcs)))

(define-condition git-condition (vcs-condition)
  ()
  (:default-initargs :vcs 'git))

(define-condition git-error (error git-condition)
  ())

(define-simple-error git-error)

;;;
;;; Repositories
;;;
(defun git-repository-present-p (&optional directory)
  "See if MODULE repository and source code is available at LOCALITY."
  (and (if directory (directory-exists-p directory) t)
       (maybe-within-directory directory
         (and (directory-exists-p (subdirectory* nil ".git"))
              (not (null (git-branches)))))))

(defun git-repository-bare-p (&optional directory)
  (maybe-within-directory directory
    (null (directory-exists-p (subdirectory* nil ".git")))))

(defun (setf git-repository-bare-p) (val directory)
  (within-directory (directory)
    (if val
        (git-error "~@<Couldn't make git repository at ~S bare: not implemented.~:@>" directory)
        (progn
          (let ((git-files (directory (make-pathname :directory '(:relative) :name :wild))))
            (sb-posix:mkdir ".git" #o755)
            (dolist (filename git-files)
              (move-to-directory filename (make-pathname :directory '(:relative ".git") :name (pathname-name filename) :type (pathname-type filename)))))
          (setf (gitvar 'core.bar) "false")
          (git-set-branch-index-tree)
          nil))))

(defun git-repository-world-readable-p (&optional directory)
  "See, whether or not MODULE within LOCALITY is allowed to be exported
   for the purposes of git-daemon."
  (file-exists-p (subfile* directory ".git" "git-daemon-export-ok")))

(defun (setf git-repository-world-readable-p) (val &optional directory)
  "Change git-daemon's idea about world-readability of DIRECTORY."
  (let ((path (subfile* directory ".git" "git-daemon-export-ok")))
    (if val
        (open path :direction :probe :if-does-not-exist :create)
        (delete-file path))
    val))

(defun git-repository-unstaged-changes-p (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("determining whether git repository at ~S has unstaged changes" *default-pathname-defaults*)
      (with-avoided-executable-output
        (not (with-shell-predicate (git "diff" "--exit-code")))))))

(defun git-repository-staged-changes-p (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("determining whether git repository at ~S has staged changes" *default-pathname-defaults*)
      (with-avoided-executable-output
        (not (with-valid-exit-codes ((0 t)
                                     (128 nil))
               (git "diff" "--cached" "--exit-code")))))))

(defun git-repository-changes-p (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("determining whether git repository at ~S has staged or unstaged changes" *default-pathname-defaults*)
      (with-avoided-executable-output
        (not (with-shell-predicate (git "diff" "HEAD" "--exit-code")))))))

;;;
;;; Config variables
;;;
(defun gitvar (var &optional directory)
  (declare (type symbol var))
  (maybe-within-directory directory
    (with-captured-executable-output
      (multiple-value-bind (setp output)
          (with-explanation ("getting value of git variable ~A" (symbol-name var))
            (with-shell-predicate
              (git "config" (string-downcase (symbol-name var)))))
        (when setp
          (string-right-trim '(#\Return #\Newline) output))))))

(defun (setf gitvar) (val var &optional directory globalp)
  (declare (type symbol var) (type string val))
  (maybe-within-directory directory
    (with-explanation ("setting git variable ~A to ~A" (symbol-name var) val)
      (apply #'git "config" (xform globalp (curry #'cons "--global") (list "--replace-all" (string-downcase (symbol-name var)) val))))))

;;;
;;; Remotes
;;;
(defun gitremotes (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("listing git remotes in ~S" *default-pathname-defaults*)
      (let* ((output (execution-output-string 'git "remote"))
             (remote-names (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output))))
        (mapcar (compose #'intern #'string-upcase) remote-names)))))

(defun git-remote-present-p (name &optional directory)
  (member name (gitremotes directory) :test #'string=))

(defun git-add-remote (name url &optional directory)
  (maybe-within-directory directory
    (with-explanation ("adding a git remote ~A (~A) in ~S" name url *default-pathname-defaults*)
      (git "remote" "add" (downstring name) url))))

(defun ensure-gitremote (name url &optional directory)
  (unless (git-remote-present-p name directory)
    (git-add-remote name url directory)))

(defun fetch-gitremote (remote-name &optional directory)
  (maybe-within-directory directory
    (with-explanation ("fetching from git remote ~A in ~S" remote-name *default-pathname-defaults*)
      (git "fetch" (downstring remote-name)))))

;;;
;;; Raw refs
;;;
(defun ref-path (ref &optional directory)
  (subfile directory (list* ".git" "refs" (if (ref-shortp ref)
                                              (list* "heads" ref)
                                              ref))))

(defun path-ref (pathname directory)
  (let ((translated (translate-pathname pathname (merge-pathnames #p".git/refs/**/*" directory) #p"/**/*")))
    (append (rest (pathname-directory translated)) (list (pathname-name translated)))))

(defun %read-ref (pathname)
  (lret ((refval (parse-integer (file-as-string pathname) :radix #x10)))
    (unless (not (minusp refval))
      (git-error "~@<Bad value in ref ~S: ~S.~:@>" pathname refval))))

(defun parse-refval (string)
  (if (equalp (subseq string 0 5) "ref: ")
      (values (split-sequence #\/ (subseq string 5)))
      (values nil (parse-integer string :radix #x10))))

(defun cook-refval (refvalue &optional prepend-refs)
  (etypecase refvalue
    (integer (format nil "~40,'0X" refvalue))
    (list (flatten-path-list (xform prepend-refs (curry #'cons "refs") refvalue)))))

(defun ref-shortp (ref)
  (endp (rest ref)))

(defun ref-headp (ref)
  (or (ref-shortp ref) (string= "heads" (first ref))))

(defun ref-remotep (ref)
  (and (not (ref-shortp ref)) (string= "remotes" (first ref))))

(defun make-remote-ref (remote-name branch)
  `("remotes" ,(downstring remote-name) ,(downstring branch)))

;;;
;;; Raw refs & iteration
;;;
(defun head-pathnames (directory)
  (remove nil (directory (subwild directory `(".git" "refs" "heads") :name :wild :type :wild)
                         #+sbcl :resolve-symlinks #+sbcl nil)
          :key #'pathname-name))

(defun all-remote-head-pathnames (directory)
  (remove-if (lambda (p &aux (name (pathname-name p))) (or (null name) (string= name "HEAD")))
             (directory (subwild directory `(".git" "refs" "remotes") :name :wild :type :wild)
                        #+sbcl :resolve-symlinks #+sbcl nil)))

(defun remote-head-pathnames (remote directory)
  (remove-if (lambda (p &aux (name (pathname-name p))) (or (null name) (string= name "HEAD")))
             (directory (subwild directory `(".git" "refs" "remotes" ,(downstring remote)) :name :wild :type :wild)
                        #+sbcl :resolve-symlinks #+sbcl nil)))

(defmacro do-packed-refs ((ref refval directory) &body body)
  (with-gensyms (packed-refs-path s line 1read-offt)
    `(let ((,packed-refs-path (subfile ,directory '(".git" "packed-refs"))))
       (when (probe-file ,packed-refs-path)
         (with-open-file (,s ,packed-refs-path)
           (let ((*read-eval* nil)
                 (*read-base* #x10))
             (iter (for ,line = (read-line ,s nil nil))
                   (while ,line)
                   (when (char= #\# (aref ,line 0)) (next-iteration))
                   (for (values ,refval ,1read-offt) = (read-from-string ,line nil nil))
                   (unless ,refval (next-iteration))
                   (for ,ref = (cdr (split-sequence #\/ (downstring (read-from-string ,line nil nil :start ,1read-offt)))))
                   ,@body)))))))

(defun map-packed-refs (predicate fn directory)
  (do-packed-refs (ref refval directory)
    (when (funcall predicate ref refval)
      (collect (funcall fn ref refval)))))

(defun map-pathnames-full-refs (fn pathnames directory)
  (iter (for pathname in pathnames)
        (collect (funcall fn (path-ref pathname directory) (%read-ref pathname)))))

(defun map-heads (fn directory)
  (append (map-pathnames-full-refs fn (head-pathnames directory) directory)
          (map-packed-refs (lambda (r v) (declare (ignore v)) (string= (first r) "heads")) fn directory)))

(defun map-all-remote-heads (fn directory)
  (append (map-pathnames-full-refs fn (all-remote-head-pathnames directory) directory)
          (map-packed-refs (lambda (r v) (declare (ignore v)) (string= (first r) "remotes")) fn directory)))

(defun map-remote-heads (fn remote directory)
  (append (map-pathnames-full-refs fn (remote-head-pathnames remote directory) directory)
          (map-packed-refs (lambda (r v) (declare (ignore v)) (and (string= (first r) "remotes") (string= (second r) remote))) fn directory)))

(defun refs-by-value (refval directory)
  (flet ((ref-if-= (r v) (when (= v refval) r)))
    (remove nil (append (map-heads #'ref-if-= directory)
                        (map-all-remote-heads #'ref-if-= directory)))))

(defun ref-value (ref directory &key (if-does-not-exist :error))
  (let ((path (ref-path ref directory)))
    (if (probe-file path)
        (%read-ref path)
        (or (car (remove nil (map-packed-refs (lambda (r v) (when (string= "master" (first r)) v)) #'identity directory)))
            (ecase if-does-not-exist
              (:error (git-error "~@<Ref named ~S doesn't exist in git repository at ~S.~:@>" ref directory))
              (:continue nil))))))

(defun reffile-value-full (pathname &optional directory)
  (multiple-value-bind (ref refval) (parse-refval (file-line pathname))
    (let* ((normalised-ref (rest ref)) ; strip the "refs" component
           (refval (or refval (ref-value normalised-ref directory))))
      (values refval normalised-ref))))

(defun set-reffile-value-full (pathname value)
  (with-output-to-file (reffile pathname)
    (when (consp value)
      (write-string "ref: " reffile))
    (write-string (cook-refval value t) reffile)))
;;;
;;; HEAD operation
;;;
(defun head-pathname (&optional directory remote)
  (subfile directory `(".git" ,@(when remote `("refs" "remotes" ,(downstring remote))) "HEAD")))

(defun get-head (&optional directory remote)
  (reffile-value-full (head-pathname directory remote) directory))

(defun set-head (new-value &optional directory remote)
  (declare (type (or cons (integer 0)) new-value))
  (set-reffile-value-full (head-pathname directory remote) new-value))

(defun git-detach-head (&optional directory)
  (maybe-within-directory directory
    (set-head (get-head))))

(defun invoke-with-detached-head (fn directory)
  (maybe-within-directory directory
    (let ((current-head (get-head)))
      (unwind-protect (progn (git-detach-head)
                             (funcall fn))
        (when current-head
          (set-head current-head))))))

(defmacro with-detached-head ((&optional directory) &body body)
  `(invoke-with-detached-head (lambda () ,@body) ,directory))

;;;
;;; Branches
;;;
(defun git-branches (&optional directory)
  (maybe-within-directory directory
    (with-explanation ("listing git branches in ~S" *default-pathname-defaults*)
      (let ((output (execution-output-string 'git "branch")))
        (remove :* (mapcar (compose #'make-keyword #'string-upcase) 
                           (mapcan (curry #'split-sequence #\Space)
                                   (split-sequence #\Newline (string-right-trim '(#\Return #\Newline) output)))))))))


(defun git-branch-present-p (name &optional directory)
  (member name (git-branches directory) :test #'string=))

(defun git-remove-branch (name &optional directory)
  (maybe-within-directory directory
    (with-explanation ("removing git branch ~A in ~S" name *default-pathname-defaults*)
      (nth-value 0 (git "branch" "-d" (downstring name))))))

(defun git-set-noncurrent-branch (branchname &optional directory (refvalue (get-head directory)))
  (declare (type (or list (integer 0)) refvalue))
  (maybe-within-directory directory
    (with-explanation ("moving non-current ref ~A to ~:[~40,'0X~;~A~] in ~S" branchname (consp refvalue) refvalue *default-pathname-defaults*)
      (nth-value 0 (git "branch" "-f" (downstring branchname) (cook-refval refvalue))))))

(defun git-set-branch (name &optional directory (refvalue (get-head directory)))
  (with-detached-head (directory)
    (git-set-noncurrent-branch name directory refvalue)))

(defun git-set-branch-index-tree (&optional ref directory)
  (maybe-within-directory directory
    (with-explanation ("hard-resetting repository in ~S~:[~; to ~:*~S~]" *default-pathname-defaults* ref)
      (apply #'git "reset" "--hard" (when ref (list (flatten-path-list ref) "--"))))))

(defun git-set-head-index-tree (ref &optional (if-changes :error) directory)
  (let ((ref (ensure-cons ref))
        (ignore-changes (eq if-changes :ignore))
        (drop-changes (eq if-changes :reset)))
    (maybe-within-directory directory
      (with-retry-restarts ((hardreset-repository ()
                                                  :report "Clear all uncommitted changes, both staged and unstaged."
                                                  (git-set-branch-index-tree)))
        (unless (or ignore-changes drop-changes)
          (when (git-repository-changes-p directory)
            (ecase if-changes
              (:warn (warn "~@<WARNING: in git repository ~S: asked to check out ~S, but there were ~:[un~;~]staged changes. Proceeding, by request.~:@>"
                           (or directory *default-pathname-defaults*) ref (git-repository-staged-changes-p directory)))
              (:error (git-error "~@<In git repository ~S: asked to check out ~S, but there were ~:[un~;~]staged changes.~:@>"
                                 (or directory *default-pathname-defaults*) ref (git-repository-staged-changes-p directory))))))
        (with-explanation ("checking out ~S in ~S" ref *default-pathname-defaults*)
          (apply #'git "checkout" (prepend (when drop-changes "-f") (list (flatten-path-list ref)))))))))

(defun checkout-git-branch (name &optional directory reset-before-checkout &key (if-does-not-exist :error) default-ref (if-changes :error))
  (unless (git-branch-present-p name directory)
    (ecase if-does-not-exist 
      (:error (git-error "~@<Asked to check out a nonexistent branch ~A in ~S~:@>" name (or directory *default-pathname-defaults*)))
      (:create (if default-ref
                   (git-set-branch name directory default-ref)
                   (git-error "~@<While checking out branch ~A in ~S: branch doesn't exist and the default ref was not provided.~:@>" name (or directory *default-pathname-defaults*))))))
  (when reset-before-checkout
    (git-set-branch-index-tree nil directory))
  (git-set-head-index-tree `(,(downstring name)) if-changes directory))

(defun reset-git-branch-to-remote-branch (name qualified-remote-branch-name directory &optional reset-before-checkout)
  (let ((remote-ref (list* "remotes" qualified-remote-branch-name)))
    (checkout-git-branch name directory reset-before-checkout :if-does-not-exist :create :default-ref remote-ref)
    (git-set-branch-index-tree remote-ref directory)))
