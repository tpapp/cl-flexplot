;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; time series

(defun ty (y &key (start 0) (by 1))
  "Return a vector of points, generated from Y and an index starting at START
and stepping by BY."
  (map 'vector (lambda (y)
                 (prog1 (point start y)
                   (incf start by)))
       y))

;;; functions

(defparameter *fx-ignorable-conditions* '(division-by-zero)
  "Conditions that result in NIL in FX.")

(defgeneric fx (f x &key ignorable-conditions &allow-other-keys)
  (:documentation "Calculate function and return a vectro of points.  If a
member of IGNORABLE-CONDITIONS is encountered, the condition is handled and
the point is removed from the line (causing a break).")
  (:method (f (x sequence)
            &key (ignorable-conditions *fx-ignorable-conditions*))
    (map 'vector (lambda (x)
                   (handler-case (point x (funcall f x))
                     (t (condition)
                       (if (member condition
                                   ignorable-conditions
                                   :test (function typep))
                           nil
                           (error condition)))))
         x))
  (:method (f (x interval)
            &key (ignorable-conditions *fx-ignorable-conditions*)
                 (length 100))
    (fx f (grid-in x length)
        :ignorable-conditions ignorable-conditions)))
