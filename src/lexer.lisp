(defpackage #:lexer
  (:use :cl)
  (:export #:slic-lexer
	   #:next-token!))

(in-package #:lexer)

(defclass lexer ()
  ((input-string
    :initarg :input-string
    :initform (error "input-string for lexer must be provided")
    :reader input-string 
    :type string)
   (index
    :initarg index
    :initform 0
    :accessor index
    :type integer)))

(defgeneric get-char (lexer)
  (:documentation "Return the current character from the input-string")
  (:method ((lexer lexer))
    (with-slots ((i index) (str input-string)) lexer
      (if (< i (length str))
	  (char str i)
	  nil))))

(defgeneric advance-char! (lexer)
  (:documentation "advance to the next character from the input-string")
  (:method ((lexer lexer))
    (incf (index lexer))))


;;; Lexer for slic
(defclass slic-lexer  (lexer) ()
  (:documentation "lexer implementation for slic"))

(defgeneric next-token! (lexer)
  (:documentation "Return the next token from the input-string"))

(defun collect-all-matches (lexer predicate)
  "Collect all matches to the predicate from the lexer"
  (coerce 'string
	  (loop
	    for char = (get-char lexer)
	    while (and (not (null char)) (apply predicate (list char)))
	    do (advance-char! lexer)
	    collect char)))

(defun whitespace-char-p (ch)
  (or
   (char= ch #\Space)
   (char= ch #\Tab)
   (char= ch #\Newline)))

(defmethod next-token! ((lexer slic-lexer))
  (let ((ch (get-char lexer)))
    (when (null ch) (return-from next-token! nil))
    (cond
      ((char= ch #\()
       (advance-char! lexer)
       "(")
      ((char= ch #\))
       (advance-char! lexer)
       ")")
      ((char= ch #\-)
       (advance-char! lexer)
       "-")
      ((char= ch #\:)
       (advance-char! lexer)
       ":")
      ((char= ch #\.)
       (advance-char! lexer)
       ".")
      ((whitespace-char-p ch)
       (collect-all-matches lexer #'whitespace-char-p) " ")
      ((digit-char-p ch)
       (collect-all-matches lexer #'digit-char-p))
      ((alpha-char-p ch)
       (collect-all-matches lexer #'alpha-char-p))
      (t (error "no match")))))
