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

(defvar *executable-search-path* #-win32 '(#p"/usr/bin/" #p"/bin/") #+win32 '(#p"c:\\program files\\git\\bin\\" #p"d:\\program files\\git\\bin\\"
                                                                              #p"e:\\program files\\git\\bin\\" #p"f:\\program files\\git\\bin\\"
                                                                              #p"g:\\program files\\git\\bin\\" #p"h:\\program files\\git\\bin\\"
                                                                              #p"i:\\program files\\git\\bin\\" #p"j:\\program files\\git\\bin\\"
                                                                              #p"k:\\program files\\git\\bin\\" #p"l:\\program files\\git\\bin\\"))
(defparameter *executables* (make-hash-table :test 'eq))

(defvar *run-external-programs-verbosely* nil
  "Whether to echo the invoked external programs to standard output.")

(defvar *run-external-programs-dryly* nil
  "Whether to substitute actual execution of external programs with
   mere printing of their paths and parameters.")

(defun executable (name)
  (or (gethash name *executables*) (error "~@<Executable ~S isn't known.~@:>" name)))

;;; SB-EXECUTABLE?
(define-reported-condition external-program-failure (serious-condition)
  ((program :accessor cond-program :initarg :program)
   (parameters :accessor cond-parameters :initarg :parameters)
   (status :accessor cond-status :initarg :status)
   (output :accessor cond-output :initarg :output))
  (:report (program parameters status output)
           "~@<running ~A~{ ~A~} failed with exit status ~S~:[~;, output:~@:_~:*~@<...~;~A~:@>~]~%~:@>" program parameters status output))

(define-reported-condition executable-not-found (warning)
  ((name :accessor cond-name :initarg :name)
   (search-path :accessor cond-search-path :initarg :search-path))
  (:report (name search-path)
           "~@<an executable, named ~S, wasn't found in search path ~S~:@>" name search-path))

(define-reported-condition required-executable-not-found (error)
  ((name :accessor cond-name :initarg :name)
   (search-path :accessor cond-search-path :initarg :search-path))
  (:report (name search-path)
           "~@<a required executable, named ~D, wasn't found in search path ~S~:@>" name search-path))

(defun find-executable (name &key (paths *executable-search-path*) &aux (realname (string-downcase (symbol-name name))))
  "See if executable with NAME is available in PATHS. When it is, associate NAME with that path and return the latter;
   otherwise, return NIL."
  (iter (for path in paths)
        (for exec-path = (merge-pathnames path (make-pathname :name realname #+win32 #+win32 :type "exe")))
        (when (probe-file exec-path) 
          (leave (setf (gethash name *executables*) exec-path)))
        (finally (warn 'executable-not-found :name realname :search-path paths))))

(defmacro with-dryly-ran-externals (&body body)
  `(let ((*run-external-programs-dryly* t))
     (declare (special *run-external-programs-dryly*))
     ,@body))

(defmacro with-verbosely-ran-externals (&body body)
  `(let ((*run-external-programs-verbosely* t))
     (declare (special *run-external-programs-verbosely*))
     ,@body))

(defun run-external-program (name parameters &key (valid-exit-codes (acons 0 t nil)) translated-error-exit-codes (output nil) (environment '("HOME=/tmp"))
                             &aux (pathname (executable name)))
  "Run an external program at PATHNAME with PARAMETERS. 
   Return a value associated with the exit code, by the means of 
   VALID-EXIT-CODES, or signal a condition of type
   EXTERNAL-PROGRAM-FAILURE."
  (declare (special *run-external-programs-dryly*))
  (flet ((note-execution (stream)
           (format stream ";;; ~S~{ ~S~}~%" pathname parameters)
           (finish-output stream)))
    (let* ((final-output (or output (make-string-output-stream)))
           (exit-code (progn
                        (when (or *run-external-programs-dryly* *run-external-programs-verbosely*)
                          (note-execution *standard-output*))
                        (if *run-external-programs-dryly*
                            (caar valid-exit-codes)
                            (sb-ext:process-exit-code (sb-ext:run-program pathname parameters :output final-output :environment environment))))))
      (cdr (or (assoc exit-code valid-exit-codes)
               (when-let ((error (assoc exit-code translated-error-exit-codes)))
                 (apply #'error (list* :program pathname :parameters parameters :status exit-code :output (get-output-stream-string final-output)
                                       (cdr error))))
               (error 'external-program-failure :program pathname :parameters parameters :status exit-code :output (get-output-stream-string final-output)))))))

(defmacro with-input-from-external-program ((stream-var name params) &body body)
  (with-gensyms (block str)
    `(block ,block
       (with-output-to-string (,str)
         (run-external-program ,name ,params :output ,str)
         (with-input-from-string (,stream-var (get-output-stream-string ,str))
           (return-from ,block (progn ,@body)))))))

(defun external-program-output-as-string (name &rest params)
  (with-output-to-string (str)
    (run-external-program name params :output str)))

(defvar *valid-exit-codes* nil)
(defvar *translated-error-exit-codes* nil)

(defmacro define-external-program (name &key may-want-display)
  `(progn
     (defun ,name (&rest parameters)
       (let (environment)
         (declare (ignorable environment))
         (with-retry-restarts ((retry () :report "Retry execution of the external program.")
                               ,@(when may-want-display
                                       `((retry (display)
                                                :report "Retry execution of the external program with DISPLAY set."
                                                :interactive (lambda ()
                                                               (format *query-io* "Enter value for the DISPLAY variable: ")
                                                               (finish-output *query-io*)
                                                               (list (read-line *query-io*)))
                                                (push (concatenate 'string "DISPLAY=" display) environment)))))
           (apply #'run-external-program ',name parameters
                  :valid-exit-codes (acons 0 t *valid-exit-codes*)
                  :translated-error-exit-codes *translated-error-exit-codes*
                  (when environment (list :environment environment))))))))

(defmacro with-valid-exit-codes ((&rest bindings) &body body)
  `(let ((*valid-exit-codes* (list ,@(mapcar (curry #'cons 'cons) bindings))))
     ,@body))

(defmacro with-exit-code-to-error-translation ((&rest bindings) &body body)
  `(let ((*translated-error-exit-codes* (list ,@(mapcar (curry #'cons 'list) bindings))))
     ,@body))

(defmacro exit-code-bind ((&rest bindings) &body body)
  `(handler-bind ((external-program-failure (lambda (cond)
                                              (case (cond-status cond)
                                                ,@bindings))))
     ,@body))

(define-external-program git :may-want-display t)
(define-external-program gitk :may-want-display t)
#-win32
(progn
  (define-external-program darcs)
  (define-external-program rsync)
  (define-external-program git-cvsimport)
  (define-external-program git-svn)
  (define-external-program darcs-to-git))

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
  (cl-fad:delete-directory-and-files pathname))

(defmacro do-directory-pathnames ((var (&rest directory-components)) &body body)
  `(dolist (,var (directory (make-pathname :directory '(,@directory-components) :name :wild)))
     ,@body))

(defun move-to-directory (pathname target-directory)
  (if (pathname-name pathname)
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (pathname-directory target-directory) :name (pathname-name pathname))))
      (sb-posix:rename (namestring pathname) (namestring (make-pathname :directory (append (pathname-directory target-directory) (list (lastcar (pathname-directory pathname)))))))))