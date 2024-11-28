(defpackage #:slic-0.0.1
  (:use :cl) (:export #:slic))

(in-package #:slic-0.0.1)

(defclass compiler ()
  ((output-string :accessor output-string
		  :initform "")
  (symbol-table :accessor symbol-table :initform (make-hash-table))))

(defgeneric code (compiler string))
(defmethod code ((c compiler) (string string))
  (setf (output-string c)
	(concatenate 'string (output-string c) string)))

(defgeneric get-code (compiler))
(defmethod get-code ((c compiler))
  (output-string c))

(defun concatenate-symbols (sym1 sym2)
  (intern (concatenate 'string (symbol-name sym1) (symbol-name sym2))))

(defun colon-reader (stream char)
  (declare (ignore char))
  (let ((type (read stream)))
    (concatenate-symbols '@ type)))

;(defparameter *slic-readtable* (copy-readtable *readtable*))

(defmacro slic (&body body)
  (handler-case 
      (let
          ((compiler (make-instance 'compiler)))
        (dolist (top-level body)
          (let ((fn (first top-level)))
                                        (format t "fn: ~a   " fn)
                                        (format t "fn-type: ~a" (type-of fn))
            (cond
              ((string= fn 'DEF) (parse-def compiler top-level) )
              ((string= fn 'FIELD) nil)
              (t (error
                  "Top-level-function must be 'DEF' or 'FIELD', found ~a" fn))
              ))))
    ;(error (e)
    ;  (format uiop:*stderr* "Error: ~a" e)
   ;   (error e))
      ))

(defclass symbol-declaration ()
  ((name :accessor name :initarg :name :type string)
   (type-specifier :accessor type-specifier
		   :initarg :type-specifier :type string)))

(defparameter *types* (list "u32" "i32" "f32"))
(defparameter *name-type-delimiter*  '(#\@))

(defun symbol-char-p (ch)
  "Returns whether a character contains only letters, numbers, or '-'"
  (or (alpha-char-p ch) (digit-char-p ch) (char= ch #\-)))

(defun slic-symbol-p (symbol)
  "Returns whether a symbol is a valid slic symbol"
  (let ((str (identity symbol)))
    (and (alpha-char-p (char str 0))
         (every #'symbol-char-p str))))
#|
(defun parse-symbol-declaration (symbol)
  "Parse a symbol declaration of the form 'symbol-name:symbol-type'"
  (let*
      ((str (symbol-name symbol))
       (delimiter-count (loop for ch across str
			  when (char= ch *name-type-delimiter*) count 1)))
    (assert (equal delimiter-count 1)
	    (str) "Symbol declaration ~a missing a delimiter. Declaration should be of form 'symbol-name~asymbol-type'" str *name-type-delimiter* )
    (let* (
       (split-str (uiop:split-string str :separator *name-type-delimiter*))
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
|#

(defclass value ()
  ())
(defclass literal (value)
  ())
(defclass integer-literal (literal)
  ((value :initarg :value :accessor value :type integer)))
(defclass float-literal (literal)
  ((value :initarg :value :accessor value :type number)))

(defun parse-value (compiler value)
  "Parse a value. A value can be another symbol, a literal, or an expression)"
  (cond
    ;check for expression
    ((listp value))
    ;check for literal
    ((digit-char-p (char (identity value) 0))
     
     )
    ;check for symbol
    ((slic-symbol-p value))
    (t (error "Invalid value: ~a" value))))
     

;(defclass str
                   

(defun assert-symbol-declaration-uninterned (compiler symbol-declaration)
  "Assert that the symbol is not interned in the compilers symbol table."
  (with-slots ((name name)) symbol-declaration
    (with-slots ((syms symbol-table)) compiler
      (unless (null (gethash name syms))
        (error "Symbol ~a has already been defined" name)))))

(defun intern-symbol-declaration (compiler decl)
  "Intern a new symbol"
  (assert-symbol-declaration-uninterned compiler decl)
  (with-slots ((name name)) decl
    (with-slots ((syms symbol-table)) compiler
      (setf (gethash name syms) decl))))

(defun get-symbol-value (compiler symbol-name)
  "Returns the value associated with a given symbol.
  Asserts that the symbol is defined"
    (with-slots ((syms symbol-table)) compiler

      (let ((value (gethash symbol-name syms)))
        (when (null value)
          (error "Symbol ~a is undefined" symbol-name))
        value)))

(defun parse-def (compiler def-expr)
  (assert (string= (first def-expr) 'DEF)
          (def-expr)
          "def statement missing, this is an internal compiler error. ~a"
          def-expr) 
  (let
      ((decl (make-instance 'symbol-declaration
                            :name (symbol-name (second def-expr))
                            :type-specifier
                            (subseq (symbol-name (third def-expr)) 1))))
    (intern-symbol-declaration compiler decl)
    ;(assert-symbol-declaration-uninterned compiler decl)
    ;(setf (gethash (name decl)) 

    ))
   
