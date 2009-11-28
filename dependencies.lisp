;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-

(cl:defpackage #:elsewhere.0
  (:nicknames #:e.0)
  (:use :common-lisp :iterate :alexandria :pergamum :executor)
  (:export
   ;; FADdy
   #:pathname-absolute-p
   ;; Page size
   #:virtual-memory-page-size
   ;; REGISTERED
   #:registered #:registered-registrator #:fully-qualified-name
   ;; SYNCHRONISABLE
   #:synchronisable #:synchronised-p #:synchronisable-last-sync-time #:synchronisable-mirror
   ;; MINI-CLOSER
   #:class-prototype
   ;; CLASS-SLOT
   #:define-marked-class #:instance-class-marked-value
   #:class-slot #:set-class-slot #:with-class-slot
   ;; Decoded time
   #:print-decoded-time
   ;;; encumbered
   ;; Strings: iterate
   #:extract-delimited-substrings #:downstring #:split-sequence
   ;; PARSE-URI: strings
   #:parse-uri
   ;; Versions: pergamum
   #:princ-version-to-string #:bump-version-component #:next-version-variants 
   ;; NAMED: alexandria
   #:named #:name #:coerce-to-name #:coerce-to-named #:sort-by-name #:down-case-name #:coerce-to-namestring #:print-aborted-named-object #:slot-or-abort-print-object
   ;; WWW: executor
   #:wget
   #:get-url-contents-as-string #:list-www-directory #:invoke-with-file-from-www #:with-file-from-www #:touch-www-file
   ))

(cl:in-package :elsewhere.0)

;;;;
;;;; FADdy
;;;;
(defun pathname-absolute-p (pathname)
  (eq (car (pathname-directory pathname)) :absolute))

;;;;
;;;; Page size
;;;;
(defun virtual-memory-page-size ()
  #+sbcl (sb-sys:get-page-size)
  #-sbcl 4096)

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
    #-(or sbcl)
    (not-implemented 'CLASS-PROTOTYPE)))

;;;;
;;;; CLASS-SLOT
;;;;
(defvar *class-slot-store* (make-hash-table :test 'equal))

(define-root-container *class-slot-store* %class-slot :type t :if-exists :continue)

(defun class-slot (&rest class-slot-name)
  (%class-slot class-slot-name))

(defun set-class-slot (class-name slot-name value)
  (setf (%class-slot (list class-name slot-name)) value))

(defsetf class-slot set-class-slot)

(defmacro with-class-slot (classes slot-name &body body)
  `(symbol-macrolet ,(iter (for class in classes) (collect `(,class (class-slot ',class ',slot-name))))
     ,@body))

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

;;;;
;;;; PARSE-URI
;;;;
(defun parse-uri (namestring &key slashless-header)
  "Given an URI namestring, produce its constituent schema, username, 
password, hostname, port, split path and a boolean specifying whether
the path refers to a directory as multiple values.
The optional SLASHLESS-HEADER keyword turns on a hacky mode allowing
treating CVS locations as URIs."
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
              (when slash-pos (split-sequence #\/ (subseq namestring slash-pos) :remove-empty-subseqs t))
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
    (nth-value 1 (with-captured-executable-output
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
  (with-valid-exit-codes ((8 nil)) (wget "--spider" url)))
