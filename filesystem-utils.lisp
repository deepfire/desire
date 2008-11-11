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

(defvar *executable-search-path* #-win32 '(#p"/usr/bin/" #p"/bin/") #+win32 '(#p"c:\\program files\\git\\bin\\" #p"d:\\program files\\git\\bin\\"
                                                                              #p"e:\\program files\\git\\bin\\" #p"f:\\program files\\git\\bin\\"
                                                                              #p"g:\\program files\\git\\bin\\" #p"h:\\program files\\git\\bin\\"
                                                                              #p"i:\\program files\\git\\bin\\" #p"j:\\program files\\git\\bin\\"
                                                                              #p"k:\\program files\\git\\bin\\" #p"l:\\program files\\git\\bin\\"))
(defparameter *executables* (make-hash-table :test 'eq))

(defun executable (name)
  (or (gethash name *executables*) (error "~@<Executable ~S isn't known.~@:>" name)))

(define-condition executable-not-found (warning)
  ((name :accessor cond-name :initarg :name)
   (search-path :accessor cond-search-path :initarg :search-path))
  (:report (lambda (cond stream)
             (format stream "~@<an executable, named ~S, wasn't found in search path ~S~:@>" (cond-name cond) (cond-search-path cond)))))

(define-condition required-executable-not-found (error)
  ((name :accessor cond-name :initarg :name)
   (search-path :accessor cond-search-path :initarg :search-path))
  (:report (lambda (cond stream)
             (format stream "~@<a required executable, named ~D, wasn't found in search path ~S~:@>" (cond-name cond) (cond-search-path cond)))))

(defun subdir (pathname sub)
  (merge-pathnames (make-pathname :directory `(:relative ,sub)) pathname))

(defun find-executable (name &key (paths *executable-search-path*) critical &aux (realname (string-downcase (symbol-name name))))
  (iter (for path in paths)
        (for exec-path = (merge-pathnames path (make-pathname :name realname #+win32 #+win32 :type "exe")))
        (when (probe-file exec-path) 
          (leave (setf (gethash name *executables*) exec-path)))
        (finally (if critical
                     (error 'required-executable-not-found :name realname :search-path paths)
                     (warn 'executable-not-found :name realname :search-path paths)))))

(define-condition external-program-failure (serious-condition)
  ((program :accessor cond-program :initarg :program)
   (parameters :accessor cond-parameters :initarg :parameters)
   (status :accessor cond-status :initarg :status))
  (:report (lambda (cond stream)
             (format stream "~@<running ~A~{ ~A~} failed with exit status ~S~:@>" (cond-program cond) (cond-parameters cond) (cond-status cond)))))

(defun run-external-program (name parameters &key (valid-exit-codes (acons 0 t nil)) (output t) &aux (pathname (executable name)))
  "Run an external program at PATHNAME with PARAMETERS. Return a value associated with the exit code, by the means of VALID-EXIT-CODES, or signal a condition of type EXTERNAL-PROGRAM-FAILURE."
  (let ((exit-code (sb-ext:process-exit-code (sb-ext:run-program pathname parameters :output output))))
    (cdr (or (assoc exit-code valid-exit-codes)
             (signal 'external-program-failure :program pathname :parameters parameters :status exit-code)))))

(defmacro with-input-from-external-program ((stream-var name params) &body body)
  (with-gensyms (block str)
    `(block ,block
       (with-output-to-string (,str)
         (run-external-program ,name ,params :output ,str)
         (with-input-from-string (,stream-var (get-output-stream-string ,str))
           (return-from ,block (progn ,@body)))))))

(defvar *valid-exit-code-extension* nil)

(defmacro define-external-program (name &key critical)
  `(progn
     (find-executable ',name ,@(when critical `(:critical t)))
     (defun ,name (&rest parameters)
       (run-external-program ',name parameters :valid-exit-codes (acons 0 t *valid-exit-code-extension*)))))

(defmacro with-valid-exit-codes ((&rest bindings) &body body)
  `(let ((*valid-exit-code-extension* (list ,@(mapcar (curry #'cons 'cons) bindings))))
     ,@body))

(define-external-program git :critical t)
(define-external-program rm)
#-win32
(progn
  (define-external-program false)
  (define-external-program darcs)
  (define-external-program rsync)
  (define-external-program git-cvsimport)
  (define-external-program git-svn)
  (define-external-program darcs-to-git)
  (define-external-program gitk))

(define-condition about-to-purge (error)
  ((directory :accessor cond-directory :initarg :directory))
  (:report (lambda (cond stream)
             (format stream "~@<about to purge ~S~:@>" (cond-directory cond)))))

(defun purge-directory (pathname)
  (block wall
    (restart-bind ((purge (lambda () (return-from wall))
                     :report-function (lambda (stream) (format stream "~@<Proceed with purging the directory.~:@>"))
                     :test-function (lambda (cond) (typep cond 'about-to-purge))))
      (error 'about-to-purge :directory pathname)))
  (rm "-rf" pathname))

(defun change-directory (pathname)
  "Change both Lisp's *DEFAULT-PATHNAME-DEFAULTS* and the underlying OS's idea of the current directory."
  (declare (type pathname pathname))
  (setf *default-pathname-defaults* pathname)
  (zerop (sb-posix:chdir (namestring pathname))))

(defmacro with-changed-directory (dir &body body)
  (with-gensyms (old)
    (once-only (dir)
      `(let ((,old (sb-posix:getcwd))
             (*default-pathname-defaults* (parse-namestring ,dir)))
         (sb-posix:chdir ,dir)
         (unwind-protect (progn ,@body)
           (sb-posix:chdir ,old))))))

(defmacro do-directory-pathnames ((var (&rest directory-components)) &body body)
  `(dolist (,var (directory (make-pathname :directory '(,@directory-components) :name :wild)))
     ,@body))

(defun move-to-directory (pathname target-directory)
  (if (pathname-name pathname)
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (pathname-directory target-directory) :name (pathname-name pathname))))
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (append (pathname-directory target-directory) (list (lastcar (pathname-directory pathname)))))))))