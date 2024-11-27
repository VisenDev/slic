(defpackage #:parser
  (:use :cl)
  (:export #:parser #:slic-parser #:parse)
  (:local-nicknames
   (#:lx #:lexer)))

(in-package #:parser)


(defclass parser ()
  ((lexer :initarg :lexer
	  :initform (error "Lexer must be provided")
          :reader lexer)
  (abstract-syntax-tree :initform (list)
			:accessor ast))
  (:documentation "parser for slic"))

(defclass slic-parser (parser) ())
		     
(defgeneric parse (parser)
  (:documentation "parse tokens to generate ast"))

(defmethod parse ((parser slic-parser))
  (with-slots ((lx lexer) (ast abstract-syntax-tree)) parser
      (loop for token = (lx:next-token! lx)
	    while (not (null token))
	    do (format t "~a~@
                          " token))))
