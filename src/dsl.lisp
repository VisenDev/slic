(defpackage #:slic-0.0.1
  (:use :cl) (:export #:slic))

(in-package #:slic-0.0.1)


(defclass slic ()
  ((output-string :accessor output-string
		  :initform "")))
  ;(error-string :accessor error-string
		  ;:initform "")))

(defgeneric code (slic string))
(defmethod code ((slic slic) (string string))
  (setf (output-string slic)
	(concatenate 'string (output-string slic) string)))

(defgeneric get-code (slic))
(defmethod get-code ((slic slic))
  (output-string slic))

(defmacro slic (&body body)
  (handler-case 
      (let
	  ((slic (make-instance 'slic))
	   (input-string (identity body)))
	(dolist (top-level )
	  (let ((fn (first top-level)))
	    ;(format t "fn: ~a   " fn)
	    ;(format t "fn-type: ~a" (type-of fn))
	    (cond
	      ((string= fn 'DEF) #|(parse-def slic top-level)|# )
	      ((string= fn 'FIELD) nil)
	      (t (error
		  "Top-level-function must be 'DEF' or 'FIELD', found ~a" fn))
	))))
    (error (e)
      (format uiop:*stderr* "Error: ~a" e)
      (error e)
      )))

(defclass symbol-declaration ()
  ((name :accessor name :initarg :name :type string)
   (type-specifier :accessor type-specifier
		   :initarg :type-specifier :type string)))

(defparameter *types* (list "u32" "i32" "f32"))
(defparameter *name-type-delimiter* #\.)

(defun symbol-char-p (ch)
  "Returns whether a character contains only letters, numbers, or '-'"
  (or (alpha-char-p ch) (digit-char-p ch) (char= ch #\-)))

(defun parse-symbol-declaration (str)
  "Parse a symbol declaration of the form 'symbol-name:symbol-type'"
  (let*
       ((delimiter-count (loop for ch across str
			  when (char= ch *name-type-delimiter*) count 1)))
    (assert (equal delimiter-count 1)
	    (str) "Symbol declaration ~a missing a delimiter. Declaration should be of form 'symbol-name~asymbol-type'" str *name-type-delimiter* )
    (let* (
       (split-str (uiop:split-string :separator *name-type-delimiter*))
       (name (first split-str))
       (type (second split-str)))
      (assert (alpha-char-p (char name 0))
	      (name) "The first character of symbol ~a must be a letter" name)
      (assert (every #'symbol-char-p name)
	      (name) "Every character in symbol ~a must be a letter, number, or dash" name)
      (assert (member type *types*)
	      (type) "~a is not a valid type specifier" type)
      (make-instance 'symbol-declaration
		     :name name
		     :type-specifier type))))


(defun parse-def (slic def-expr)
  (assert (string= (first def-expr) 'DEF)
	  (def-expr) "def statement missing, this is an internal compiler error. ~a" def-expr) 
  ;(let ((decl (parse-symbol-declaration (identity (second def-expr)))))
    (format t "decl ~a" decl));)
   
