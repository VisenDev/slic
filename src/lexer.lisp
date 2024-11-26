(defpackage #:lexer
  (:use :cl))

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

(defgeneric next-char (lexer)
  (:documentation "Return the next character from the input-string"))

(defmethod next-char ((lexer lexer))
  (if (>= (index lexer) (length (input-string lexer)))
      nil
      (let
	  ((result (char (input-string lexer) (index lexer))))
	(incf (index lexer))
	result)))


;;; Lexer for slic
(defclass slic-lexer  (lexer) ()
  (:documentation "lexer implementation for slic"))

(defgeneric next-token (lexer)
  (:documentation "Return the next token from the input-string"))

(defmethod next-token ((lexer slic-lexer))
  (let ((ch (next-char lexer)))
    (when (null ch) (return-from next-token nil))
    (cond
      ((char= ch #\() "(")
      ((char= ch #\)) ")")
      ((alpha-char-p ch)
       (concatenate 'string (string ch)
	(loop
	  for char = (next-char lexer)
	  while (and (not (null ch)) (alpha-char-p char))
	  collect char)))
      (t (error "no match")))))
