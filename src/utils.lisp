;(ql:quickload :closer-mop)

(defpackage :helpers
  (:use :cl)
  (:export
   :export-defun
   :make-dynamic-array
   :assert-not-null
   :declaim-type
   :declare-type
   :typed-defun))

(in-package :helpers)

(defmacro export-defun (name args &body body)
  "Defines a function and exports it from the current package."
  `(progn
     (defun ,name ,args ,@body)
     (export ',name)))

(defun make-dynamic-array (element-type) 
  (make-array
    0 
    :element-type element-type
    :adjustable t
    :fill-pointer 0))


(defun assert-not-null (&rest values)
  (dolist (value values)
    (assert (not (null value)))))

(defmacro declaim-type (name type)
  `(declaim (type (,type) ,name)))

(defmacro declare-type (name type)
  `(declare (type (,type) ,name)))

(defmacro declaim-signature (function-name parameter-types return-type)
  `(declaim (ftype (function ,parameter-types ,return-type) ,function-name)))

(defmacro typed-defun (return-type function-name parameters &body body)
  "defines a function and declaims its type signature"
  (assert (evenp (length parameters)))
  (format t "expanding macro...~%")
  (let
      ((parameter-types '())
       (parameter-names '())
       (parameter-state :expect-type))
    (dolist (type-or-name parameters)
      (case parameter-state
	(:expect-name
	 (assert (typep type-or-name 'symbol))
	 (push type-or-name parameter-names)
	 (setf parameter-state :expect-type))
	(:expect-type
	 (assert (typep type-or-name 'symbol))
	 (let ((type (find-symbol (symbol-name type-or-name))))
	   (push type parameter-types))
	 (setf parameter-state :expect-name))))
    `(progn
       (declaim (ftype (function ,(nreverse parameter-types) ,(find-symbol (symbol-name return-type))) ,function-name))
       (defun ,function-name (,@(nreverse parameter-names)) ,@body))))
	       








