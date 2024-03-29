;;;; File "capitalized-export-test.lisp"
;;;;
;;;; Testing exporting by capitalizing symbols.

(defpackage :capitalized-export-test
  (:use :cl))

(in-package :capitalized-export-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (capitalized-export:make-capitalized-export-readtable)))

(defclass Person ()
  ((%name :initarg :name :accessor Name :initform nil)
   (%age :initarg :age :accessor Age :initform nil)))

;; |...| is not exported, should instead be exported the normal way.
(defun |Make-person| ()
  (make-instance 'person))

'A ; Export
'b ; Don't export

;; Error:
;; (defun cl-user::Foo () 1)

(defparameter *Circular* '#1=(1 2 3 . #1#))

(loop :for symbol :being :each :external-symbol :in *package*
      :do (print symbol))

;; This form will be returned by CL:READ as if it had been typed at
;; the bottom of the file:
;;
;; (EXPORT '(PERSON NAME AGE A *CIRCULAR*))
