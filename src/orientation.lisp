;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

(deftype orientation ()
  "(Axis) orientations."
  '(member :top :left :bottom :right))

(defun o-horizontal? (orientation)
  "Test if ORIENTATION is horizontal.  Raises an error for invalid
orientations."
  (ecase orientation
    ((:left :right) nil)
    ((:top :bottom) t)))

(defun o-other? (orientation)
  "Test if orientation is `other' than the `natural' left and bottom."
  (ecase orientation
    ((:left :bottom) nil)
    ((:right :top) t)))

(defun o-opposite (orientation)
  "Return the opposite orientation."
  (ecase orientation
    (:left :right)
    (:right :left)
    (:top :bottom)
    (:bottom :top)))
