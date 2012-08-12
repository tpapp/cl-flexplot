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

(defun o-orthogonal-pair (orientation)
  "Return a pair of orthogonal orientations."
  (if (o-horizontal? orientation)
      '(:bottom . :top)
      '(:left . :right)))

(defstruct (orientation-dependent
            (:constructor orientation-dependent (left bottom
                                                 &key (right left) (top bottom))))
  "Container for quantities that depend on orientation."
  (left nil :read-only t)
  (bottom nil :read-only t)
  (right nil :read-only t)
  (top nil :read-only t))

(defgeneric orientation (orientation object)
  (:documentation "Return an orientation-dependent value of object.  If the
  object is not orientation-dependent, it is returned as is.")
  (:method (orientation object)
    (check-type orientation orientation)
    object)
  (:method (orientation (object orientation-dependent))
    (let+ (((&structure-r/o orientation-dependent- left bottom right top)
            object))
      (check-type orientation orientation)
      (ecase orientation
        (:left left)
        (:bottom bottom)
        (:right right)
        (:top top)))))
