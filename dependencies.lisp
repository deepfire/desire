;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil; show-trailing-whitespace: t -*-

(cl:defpackage #:elsewhere.0
  (:nicknames #:e.0)
  (:use :common-lisp :iterate :alexandria :pergamum :executor)
  (:export
   ;; FADdy
   #:pathname-absolute-p
   #:pathname>
   #:pathname<
   ;; Page size
   #:virtual-memory-page-size
   ;; from common-db/portability.lisp
   #:*globally-quitting*
   #:quit
   ;; REGISTERED
   #:registered #:registered-registrator #:fully-qualified-name
   ;; SYNCHRONISABLE
   #:synchronisable #:synchronised-p #:synchronisable-last-sync-time #:synchronisable-mirror
   ;; MINI-CLOSER
   #:class-prototype
   #:finalize-inheritance
   ;; Decoded time
   #:print-decoded-time
   ;;; encumbered
   ;; Strings: iterate
   #:extract-delimited-substrings #:downstring
   ;; PARSE-URI: strings
   #:parse-uri
   ;; Versions: pergamum
   #:princ-version-to-string #:bump-version-component #:next-version-variants
   ;; NAMED: alexandria
   #:named #:name #:coerce-to-name #:coerce-to-named #:sort-by-name #:down-case-name #:coerce-to-namestring #:print-aborted-named-object #:slot-or-abort-print-object
   ;; WWW: executor
   #:wget
   #:get-url-contents-as-string #:list-www-directory #:invoke-with-file-from-www #:with-file-from-www #:touch-www-file
   ;; NAMED-READTABLES
   #:readtable=
   ))

(cl:in-package :elsewhere.0)

;;;;
;;;; FADdy
;;;;
(defun pathname-absolute-p (pathname)
  (eq (car (pathname-directory pathname)) :absolute))

(defun pathname> (x y)
  (iter (for (xelt . xrest) on (rest (pathname-directory x)))
        (for (yelt . yrest) on (rest (pathname-directory y)))
        (cond ((string= xelt yelt)
               (cond ((and xrest (not yrest)) (return t))
                     ((and yrest (not xrest)) (return nil))))
              ((string> xelt yelt) (return t))
              (t                   (return nil)))))

(defun pathname< (y x)
  (iter (for (xelt . xrest) on (rest (pathname-directory x)))
        (for (yelt . yrest) on (rest (pathname-directory y)))
        (cond ((string= xelt yelt)
               (cond ((and xrest (not yrest)) (return t))
                     ((and yrest (not xrest)) (return nil))))
              ((string> xelt yelt) (return t))
              (t                   (return nil)))))
;;;;
;;;; Page size
;;;;
(defun virtual-memory-page-size ()
  #+sbcl (sb-sys:get-page-size)
  #-sbcl 4096)

;;;;
;;;; QUIT, stolen from common-db/portability.lisp
;;;;
(defvar *globally-quitting* nil
  "Whether or not we are leaving the executable.")

(defun quit (&optional (status 0))
  "Exit."
  #-(or sbcl ecl clisp ccl) (declare (ignore status))
  #-(or sbcl ecl clisp ccl) (not-implemented 'quit)
  (setf *globally-quitting* t)
  #+sbcl (sb-ext:quit :unix-status status)
  #+clisp (ext:quit status)
  #+ecl (si:quit status)
  #+ccl (progn
          ;; #'CCL:QUIT has a tendency to hang and busyloop.
          (finish-output)
          (exit status)))

;;;;
;;;; REGISTERED
;;;;
(defclass registered (named)
  ((registrator :accessor registered-registrator :type function :initarg :registrator))
  (:documentation
   "This mixin has got a problem when there is more than one subclass of
REGISTERED, in the mixing-in class precedence list, which provides the
:REGISTRATOR initarg."))

(defgeneric fully-qualified-name (o)
  (:documentation "A name which is supposed to be unique within the use domain.")
  (:method ((o registered)) (name o)))

(defmethod initialize-instance :after ((o registered) &key registrator omit-registration &allow-other-keys)
  "Designed for SETF."
  (unless omit-registration
    (funcall registrator o (fully-qualified-name o))))

;;;;
;;;; SYNCHRONISABLE
;;;;
(defclass synchronisable ()
  ((mirror :accessor synchronisable-mirror :initarg :mirror)
   (last-sync-time :accessor synchronisable-last-sync-time :initarg :last-sync-time)
   (synchronised-p :accessor synchronised-p :type boolean :initarg :synchronised-p))
  (:default-initargs
   :mirror nil
   :last-sync-time 0
   :synchronised-p nil))

;;;;
;;;; MINI-CLOSER
;;;;
(defgeneric class-prototype (class)
  (:method ((o symbol))
    (class-prototype (find-class o)))
  (:method ((o class))
    #+sbcl
    (sb-mop:class-prototype o)
    #+ccl
    (ccl:class-prototype o)
    #-(or sbcl ccl)
    (not-implemented 'CLASS-PROTOTYPE)))

(defgeneric finalize-inheritance (class)
  (:method ((o symbol))
    (finalize-inheritance (find-class o)))
  (:method ((o class))
    #+sbcl
    (sb-mop:finalize-inheritance o)
    #+ccl
    (ccl:finalize-inheritance o)
    #-(or sbcl ccl)
    (not-implemented 'FINALIZE-INHERITANCE)))

;;;
;;; Decoded time
;;;
(defun print-decoded-time (second minute hour date month year dow daylight-p zone)
  (declare (ignorable second daylight-p zone))
  (format nil "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~] ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~D ~D, ~2,'0D:~2,'0D"
          dow (1- month) date year hour minute))

;;;;
;;;; Strings
;;;;
(defun extract-delimited-substrings (string start-delim-string end-delim-char)
  (iter (with posn = 0)
        (for href-posn = (search start-delim-string string :start2 posn))
        (while href-posn)
        (for start-posn = (+ href-posn (length start-delim-string)))
        (for close-posn = (position end-delim-char string :start start-posn))
        (when close-posn
          (collect (subseq string start-posn close-posn))
          (setf posn (1+ close-posn)))
        (while close-posn)))

(defun downstring (x)
  (string-downcase (string x)))

;;;;
;;;; PARSE-URI
;;;;
(defun parse-uri (namestring &key slashless-header)
  "Given an URI namestring, produce its constituent schema, username,
password, hostname, port, split path and a boolean, specifying whether
the path refers to a directory, as multiple values.
The optional SLASHLESS-HEADER keyword enables tolerant mode, allowing
for CVS locations to be treated as URIs."
  (let* ((colon-pos (or (position #\: namestring :start 1) (error "~@<No colon in URI ~S.~:@>" namestring))))
    (unless (> (length namestring) (+ colon-pos (if slashless-header 1 3)))
      (error "~<@URI ~S is too short.~:@>" namestring))
    (unless (or (and (char= (aref namestring (+ 1 colon-pos)) #\/)
                     (char= (aref namestring (+ 2 colon-pos)) #\/))
                slashless-header)
      (error "~@<URI ~S is malformed.~:@>" namestring))
    (let* ((main-pos (+ colon-pos (if slashless-header 1 3)))
           (slash-pos (position #\/ namestring :start main-pos))
           (at-pos (position #\@ namestring :start main-pos :end slash-pos))
           (cred-colon-pos (when at-pos (position #\: namestring :start main-pos :end at-pos)))
           (port-colon-pos (position #\: namestring :start (if at-pos (1+ at-pos) main-pos) :end slash-pos)))
      (values (subseq namestring 0 colon-pos)
              (when at-pos (subseq namestring main-pos (or cred-colon-pos at-pos)))
              (when (and at-pos cred-colon-pos) (subseq namestring (1+ cred-colon-pos) at-pos))
              (subseq namestring (if at-pos (1+ at-pos) main-pos) (or port-colon-pos slash-pos))
              (when port-colon-pos (let ((port (subseq namestring (1+ port-colon-pos) slash-pos))
                                         (*break-on-signals* nil))
                                     (when (plusp (length port))
                                       (handler-case (parse-integer port)
                                         (error () (error "~@<Not a number in port position of URI ~S.~:@>" namestring))))))
              (when slash-pos (split-sequence:split-sequence #\/ (subseq namestring slash-pos) :remove-empty-subseqs t))
              (char= #\/ (aref namestring (1- (length namestring))))))))

;;;;
;;;; Versions
;;;;
(defun princ-version-to-string (version)
  "Return a textual representation of VERSION."
  (flatten-path-list (mapcar #'princ-to-string version) nil nil "."))

(defun bump-version-component (version n mode)
  "Increase Nth component of VERSION, counting from tail, by one, or, when
TEN-CEILING is non-NIL, just enough to make the component rounded by 10."
  (let ((last (butlast version n)))
    (case mode
      (:incf (incf (lastcar last)))
      (:ten-ceiling (setf (lastcar last) (* 10 (ceiling (1+ (lastcar last)) 10))))
      (:mult-ten (setf (lastcar last) (* 10 (lastcar last)))))
    (append last (make-list n :initial-element 0))))

(defun next-version-variants (version)
  "Produce plausible variants of versions following VERSION,
in order of strictly decreasing likelihood."
  (remove-duplicates (iter (for i from 0 below (length version))
                           (collect (append version (list 1)))
                           (collect (bump-version-component version i :incf))
                           (unless (= i (1- (length version)))
                             (let ((10-bumped (bump-version-component version i :ten-ceiling)))
                               (collect 10-bumped)
                               (collect (bump-version-component version i :mult-ten))
                               (appending (iter (for j from 0 below (1+ i))
                                                (collect (butlast 10-bumped j)))))))
                     :test #'equal))

;;;;
;;;; NAMED
;;;;
(defclass named ()
  ((name :accessor name :initarg :name)))

(defun coerce-to-name (o)
  (declare (type (or symbol named) o))
  (if (symbolp o) o (name o)))

(defun coerce-to-named (o)
  (declare (type (or symbol named) o))
  (if (symbolp o) (make-instance 'named :name o) o))

(defun sort-by-name (named-objects)
  "Sort object of class NAMED, lexicographically."
  (sort named-objects #'string< :key (compose #'symbol-name #'name)))

(defun down-case-name (x)
  (string-downcase (string (if (typep x 'named) (name x) x))))

(defun coerce-to-namestring (namespec)
  (declare (type (or symbol string named) namespec))
  (typecase namespec
    (named (string (name namespec)))
    (symbol (string-upcase (symbol-name namespec)))
    (string (string-upcase namespec))))

(defun print-aborted-named-object (stream o &optional slot-name)
  (declare (type stream stream) (type named o))
  (format stream "~@<#<ab~;orted printing object of type ~A with name ~S~:[~;, due to unbound or missing slot ~:*~A~]~;>~:@>"
          (type-of o)
          (if (slot-boundp o 'name)
              (name o)
              "#<not bound>")
          slot-name))

(defmacro slot-or-abort-print-object (stream o slot-name &optional (block 'print-object))
  (with-gensyms (bo bslot-name)
    `(let ((,bslot-name ,slot-name)
           (,bo ,o))
       (unless (slot-boundp ,bo ,bslot-name)
         (print-aborted-named-object ,stream ,o ,bslot-name)
         (return-from ,block nil))
       (slot-value ,bo ,bslot-name))))

;;;;
;;;; WWW
;;;;
(define-executable wget)

(defun get-url-contents-as-string (url)
  (with-explanation ("retrieving ~S" url)
    (nth-value 1 (let ((*executable-standard-output* :capture))
                   (wget url "-O" "-")))))

(defun list-www-directory (url)
  (nthcdr 5 (extract-delimited-substrings (get-url-contents-as-string url) "HREF=\"" #\")))

(defun invoke-with-file-from-www (filename url fn)
  (unwind-protect (with-explanation ("retrieving ~A" url)
                    (wget url "-O" filename)
                    (funcall fn))
    (delete-file filename)))

(defmacro with-file-from-www ((filename url) &body body)
  `(invoke-with-file-from-www ,filename ,url (lambda () ,@body)))

(defun touch-www-file (url)
  (with-valid-exit-codes ((1 nil) (2 nil) (3 nil) (4 nil) (5 nil) (6 nil) (7 nil) (8 nil)) (wget "--spider" url)))

;;;;
;;;; NAMED-READTABLES innards
;;;;
(defmacro with-readtable-iterator ((name readtable) &body body)
  (let ((it (gensym)))
    `(let ((,it (%make-readtable-iterator ,readtable)))
       (macrolet ((,name () `(funcall ,',it)))
         ,@body))))

#+sbcl
(defun %make-readtable-iterator (readtable)
  (let ((char-macro-array (sb-impl::character-macro-array readtable))
        (char-macro-ht    (sb-impl::character-macro-hash-table readtable))
        (dispatch-tables  (sb-impl::dispatch-tables readtable))
        (char-code 0))
    (with-hash-table-iterator (ht-iterator char-macro-ht)
      (labels ((grovel-base-chars ()
                 (declare (optimize sb-c::merge-tail-calls))
                 (if (>= char-code sb-int:base-char-code-limit)
                     (grovel-unicode-chars)
                     (let ((reader-fn (svref char-macro-array char-code))
                           (char      (code-char (shiftf char-code (1+ char-code)))))
                       (if reader-fn
                           (yield char reader-fn)
                           (grovel-base-chars)))))
               (grovel-unicode-chars ()
                 (multiple-value-bind (more? char reader-fn) (ht-iterator)
                   (if (not more?)
                       (values nil nil nil nil nil)
                       (yield char reader-fn))))
               (yield (char reader-fn)
                 (let ((disp-ht))
                   (cond
                     ((setq disp-ht (cdr (assoc char dispatch-tables)))
                      (let* ((disp-fn (get-macro-character char readtable))
                             (sub-char-alist))
                        (maphash (lambda (k v)
                                   (push (cons k v) sub-char-alist))
                                 disp-ht)
                        (values t char disp-fn t sub-char-alist)))
                     (t
                      (values t char reader-fn nil nil))))))
        #'grovel-base-chars))))

#+clozure
(defun %make-readtable-iterator (readtable)
  (let ((char-macro-alist (ccl::rdtab.alist readtable)))
    (lambda ()
      (if char-macro-alist
          (destructuring-bind (char . defn) (pop char-macro-alist)
            (if (consp defn)
                (values t char (car defn) t (cdr defn))
                (values t char defn nil nil)))
          (values nil nil nil nil nil)))))

;;; Written on ACL 8.0.
#+allegro
(defun %make-readtable-iterator (readtable)
  (declare (optimize speed))            ; for TCO
  (check-type readtable readtable)
  (let* ((macro-table     (first (excl::readtable-macro-table readtable)))
         (dispatch-tables (excl::readtable-dispatch-tables readtable))
         (table-length    (length macro-table))
         (idx 0))
    (labels ((grovel-macro-chars ()
               (if (>= idx table-length)
                   (grovel-dispatch-chars)
                   (let ((read-fn (svref macro-table idx))
			 (oidx idx))
                     (incf idx)
                     (if (or (eq read-fn #'excl::read-token)
                             (eq read-fn #'excl::read-dispatch-char)
                             (eq read-fn #'excl::undefined-macro-char))
                         (grovel-macro-chars)
                         (values t (code-char oidx) read-fn nil nil)))))
             (grovel-dispatch-chars ()
               (if (null dispatch-tables)
                   (values nil nil nil nil nil)
                   (destructuring-bind (disp-char sub-char-table)
                       (first dispatch-tables)
                     (setf dispatch-tables (rest dispatch-tables))
                     ;;; Kludge. We can't fully clear dispatch tables
                     ;;; in %CLEAR-READTABLE.
                     (when (eq (svref macro-table (char-code disp-char))
                               #'excl::read-dispatch-char)
                       (values t
                               disp-char
                               (svref macro-table (char-code disp-char))
                               t
                               (loop for subch-fn   across sub-char-table
                                     for subch-code from 0
                                     when subch-fn
                                       collect (cons (code-char subch-code)
                                                     subch-fn))))))))
      #'grovel-macro-chars)))


#-(or sbcl clozure allegro)
(eval-when (:compile-toplevel)
  (let ((*print-pretty* t))
    (format t
     "~&~@<  ~@;~A has not been ported to ~A. ~
       We fall back to a portable implementation of readtable iterators. ~
       This implementation has to grovel through all available characters. ~
       On Unicode-aware implementations this may come with some costs.~@:>"
     (package-name '#.*package*) (lisp-implementation-type))))

#-(or sbcl clozure allegro)
(defun %make-readtable-iterator (readtable)
  (check-type readtable readtable)
  (let ((char-code 0))
    #'(lambda ()
        (prog ()
           :GROVEL
           (when (< char-code char-code-limit)
             (let* ((char (code-char char-code))
                    (fn   (get-macro-character char readtable)))
               (incf char-code)
               (when (not fn) (go :GROVEL))
               (multiple-value-bind (disp? alist)
                   (handler-case ; grovel dispatch macro characters.
                       (values t
                               ;; Only grovel upper case characters to
                               ;; avoid duplicates.
                               (loop for code from 0 below char-code-limit
                                     for subchar = (let ((ch (code-char code)))
                                                     (when (or (not (alpha-char-p ch))
                                                               (upper-case-p ch))
                                                       ch))
                                     for disp-fn = (and subchar
                                                        (get-dispatch-macro-character
                                                            char subchar readtable))
                                     when disp-fn
                                       collect (cons subchar disp-fn)))
                     (error () nil))
                 (return (values t char fn disp? alist)))))))))

(defun function= (fn1 fn2)
  "Are reader-macro function-designators FN1 and FN2 the same?"
  #+ :clisp
  (let* ((fn1 (ensure-function fn1))
         (fn2 (ensure-function fn2))
         (n1 (system::function-name fn1))
         (n2 (system::function-name fn2)))
    (if (and (eq n1 :lambda) (eq n2 :lambda))
        (eq fn1 fn2)
        (equal n1 n2)))
  #+ :common-lisp
  (eq (ensure-function fn1) (ensure-function fn2)))

(defun readtable= (readtable1 readtable2)
  "Doesn't check for character terminatingness."
  (with-readtable-iterator (iter1 readtable1)
    (with-readtable-iterator (iter2 readtable2)
      (loop
         (multiple-value-bind (more1 char1 reader-fn-1 disp1? table1) (iter1)
           (multiple-value-bind (more2 char2 reader-fn-2 disp2? table2) (iter2)
             (when (and (not more1) (not more2))
               (return t))
             (unless (and more1 more2
                          (char= char1 char2)
                          (function= reader-fn-1 reader-fn-2)
                          (eq disp1? disp2?)
                          (equal table1 table2))
               (return nil))))))))
