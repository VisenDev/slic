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

(defclass ast-node ()
  ((type :initarg :type
	 :initform (error "type must be provided for ast-node")
	 :accessor type
	 :type keyword)
   (value :initarg value
	  :initform nil
	  :accessor value))
  (:documentation "A node in the ast tree, representing both a type and a value"))

(defmethod append-ast-node! ((parser parser) ast-node-type &optional value)
  (push (make-instance 'ast-node :type ast-node-type :value value)
	(ast parser)))
;(defgeneric token! (parser)
;  (:documentation "Get the next token from the parser"))
;(defmethod token! ((parser parser))
;  (lx:next-token! (lexer parser)))

;(defgeneric token! (parser)
;  (:documentation "Get the next token from the parser"))
;(non-whitespace

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


(defun top-level! (parser)
  "A top level statement"
  (with-slots ((l lexer)) parser
    (let ((tok (lx:next-token-skip-whitespace! l)))

      ;;ensure open parenthesis
      (unless (string= tok "(")
	(report-parse-error! parser "Top level missing open parenthesis"))
      (append-ast-node! parser :open-parenthesis) ;update ast
      (setf tok (lx:next-token-skip-whitespace! l)) ;update tok

      ;;ensure valid next statement
      (cond
	((string= "def" tok) (def! parser))
	((string= "field" tok) (field! parser))
	(t (report-parse-error!
	    parser
	    "def and tok and the only two valid top level statements")))
      )))

(defun def! (parser)
  "Parse a def statement"
  ;note: the def token will have already been parsed before this function call
  (symbol! parser)
  (value! parser))

(defun symbol-char-p (ch)
  "Returns whether a character contains only letters, numbers, or '-'"
  (or (alpha-char-p ch) (digit-char-p ch) (char= ch #\-)))

(defun symbol! (parser)
  (with-slots ((l lexer)) parser
    (let ((tok (lx:next-token! l)))
    (unless (alpha-char-p (char tok 0))
	    (report-parse-error! parser
				"symbol must start with a letter"))
      (append-ast-node! parser
    (loop collect tok
	  while (every #'symbol-char-p tok)
	  do (setf tok (lx:next-token! l))
      )))))
    


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

