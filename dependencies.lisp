;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DESIRE; Base: 10; indent-tabs-mode: nil -*-

(in-package :desire)


;;;;
;;;; REPORT
;;;;
(defun report (stream format-control &rest args)
  (apply #'format stream format-control args)
  (finish-output stream))

;;;;
;;;; REPORT
;;;;
(defun pathname-absolute-p (pathname)
  (eq (car (pathname-directory pathname)) :absolute))

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

(defun downstring (x)
  (string-downcase (string x)))

(defun down-case-name (x)
  (string-downcase (string (xform-if (of-type 'named) #'name x))))

(defun coerce-to-namestring (namespec)
  (declare (type (or symbol string named) namespec))
  (typecase namespec
    (named (string (name namespec)))
    (symbol (string-upcase (symbol-name namespec)))
    (string (string-upcase namespec))))

;;;;
;;;; REGISTERED
;;;;
(defclass registered (named)
  ((registrator :accessor registered-registrator :type function :initarg :registrator)))

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

;;;;
;;;; SPLIT-SEQUENCE
;;;;
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
