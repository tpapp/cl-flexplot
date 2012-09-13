;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

(defstruct bounding-box
  "Bounding box (for plotting objects)."
  (x nil :type (or null interval) :read-only t)
  (y nil :type (or null interval) :read-only t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-structure-let+ (bounding-box) x y))

(defgeneric object-points (object)
  (:documentation "Return coordinates for a plotting object in a format
understood by EXTEND-BOUNDING-BOX.  All plotting objects should define a
method."))

(defgeneric extend-bounding-box (box object)
  (:documentation "Extend bounding box with coordinates of OBJECT, returning
the new bounding box.  Plotting objects should not define a method (and use
ONBJECT-POINTS instead), but containers or plotting objects should.")
  (:method ((box bounding-box) (object null))
    box)
  (:method ((box bounding-box) (sequence sequence))
    (map 'nil (lambda (o)
                (setf box (extend-bounding-box box o)))
         sequence)
    box)
  (:method ((box bounding-box) (object point))
    (let+ (((&bounding-box-r/o x y) box)
           ((&point-r/o x-o y-o) object))
      ;; NOTE currently the absolute part is ignored, should we do something
      ;; about that?  NOTE should we write own library functions for extending
      ;; intervals in the abs/rel space
      (make-bounding-box :x (extend-interval x (rel-part x-o))
                         :y (extend-interval y (rel-part y-o)))))
  (:method ((box bounding-box) (other-box bounding-box))
    (let+ (((&bounding-box-r/o x y) box)
           ((&bounding-box-r/o x-o y-o) other-box))
      (make-bounding-box :x (extend-interval x x-o)
                         :y (extend-interval y y-o))))
  (:method ((box bounding-box) object)
    (extend-bounding-box box (object-points object))))

(defgeneric bounding-box (object)
  (:documentation "Return a BOUNDING-BOX for OBJECT, which may be a container
of plotting objects or a single plotting object.")
  (:method (object)
    (extend-bounding-box (make-bounding-box :x nil :y nil) object)))

(defun nonempty-bounding-box? (bounding-box)
  (let+ (((&structure-r/o bounding-box- x y) bounding-box))
    (and (typep x 'finite-interval)
         (typep y 'finite-interval))))

(deftype nonempty-bounding-box ()
  '(satisfies nonempty-bounding-box?))

(defclass non-extending-object-mixin ()
  ()
  (:documentation "Mixin class for objects that do not extend the boun ding
  box."))

(defmethod extend-bounding-box ((box bounding-box)
                                (object non-extending-object-mixin))
  box)

(defstruct (strut (:constructor strut (&rest objects)))
  "Objects are only used for extending the bounding box, otherwise they are
not rendered."
  (objects nil :type list :read-only t))

(defmethod render (target (strut strut)))

(defmethod extend-bounding-box ((box bounding-box) (strut strut))
  (extend-bounding-box box (strut-objects strut)))
