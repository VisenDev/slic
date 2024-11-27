(defpackage #:parser
  (:use :cl)
  (:export #:parser #:parse!)
  (:local-nicknames
   (#:lx #:lexer)))

(in-package #:parser)


(defclass parser ()
  ((lexer :initarg :lexer
	  :initform (error "Lexer must be provided")
          :reader lexer)
  (abstract-syntax-tree :initform (list)
			:accessor ast)
  (parse-error :initform "" 
	       :accessor parse-error))
  (:documentation "parser for slic"))

(defgeneric token! (parser)
  (:documentation "Get the next token from the parser"))
(defmethod token! ((parser parser))
  (lx:next-token! (lexer parser)))

(defgeneric report-parse-error! (parser message &optional severity)
  (:documentation "report a parse error"))
(defmethod report-parse-error!
    ((parser parser) (message string) &optional (severity :high))
  (with-slots ((err parse-error)) parser
    (setf err 
	  (concatenate 'string
		       err message " "
		       (coerce severity 'string) )))
  (error "Parse Error"))

;(defclass slic-parser (parser) ())
;		     
;(defgeneric parse (parser)
;  (:documentation "parse tokens to generate ast"))
;
;(defmethod parse ((parser slic-parser))
;  (with-slots ((lx lexer) (ast abstract-syntax-tree)) parser
;      (loop for token = (lx:next-token! lx)
;	    while (not (null token))
;	    do (format t "~a~@
;                          " token))))


(defun parse! (input-string)
  "Parse a string into an AST"
  (let ((parser (make-instance
		 'parser :lexer
		 (make-instance 'lx:slic-lexer
				:input-string input-string))))
    (handler-case
	(entry! parser)
      (error (e)
	(format t "~a : ~a ~%" e (parse-error parser))
	(uiop:quit)))
    parser))

(deftype parse-function-type () '(function (parser) boolean))

(declaim (ftype parse-function-type parse!))
(defun entry! (parser)
  "Entry Point to the parser"
  (loop while (top-level! parser)))


(declaim (ftype parse-function top-level!))
(defun top-level! (parser)
  "A top level statement"
  (let ((tok (token! parser)))

    ;ensure open parenthesis
    (unless (string= tok "(")
      (report-parse-error! parser "Top level missing open parenthesis"))
    (push :open-parenthesis (ast parser))
    (setf tok (token! parser))

    ;ensure valid next statement
    (cond
      ((string= "def" tok) (def! parser))
      ((string= "field" tok) (field! parser))
      (t (report-parse-error!
	  parser
	  "def and tok and the only two valid top level statements")))
    ))
    


;;; Grammar:
    #|

    Entry = TopLevel*

    TopLevel = '(' (Def | Field) ')'

    Def = 'def' Symbol Value

    Symbol = (Alpha | Num | '-' )*

    Alpha = 'a'...'z' | 'A'...'Z'

    Num = '0'...'9'

    Value = Expr | AnyLiteral

    Expr = '(' Symbol (Value)* ')'

    AnyLiteral = (StringLiteral | NumLiteral)

    StringLiteral = '"' (Alpha | Num)* '"'

    NumLiteral = Num (Num)* ['.'] (Num)*

    |#

