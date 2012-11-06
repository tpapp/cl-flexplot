;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; QUESTION Currently bounding boxes only use the relative part of
;;; coordinates, not PT or EM.  Maybe we should do something about that?  Does
;;; that make sense?  Uneven extensions could make the plot look funny, the
;;; user could (should) just extend the relevant margin.

(defstruct bounding-box
  "Bounding box (for plotting objects)."
  (x nil :type (or null interval) :read-only t)
  (y nil :type (or null interval) :read-only t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-structure-let+ (bounding-box) x y))

(defgeneric object-points (object)
  (:documentation "Return coordinates for a plotting object in a format
understood by EXTEND-BOUNDING-BOX.  All plotting objects should define a
method unless EXTEND-BOUNDING-BOX is defined directly (which is more
tricky, OBJECT-POINTS should be easier)."))

(defgeneric extend-bounding-box (box object)
  (:documentation "Extend bounding box with coordinates of OBJECT, returning
the new bounding box.  Plotting objects should not define a method (and use
ONBJECT-POINTS instead), but containers or plotting objects should.")
  (:method ((box bounding-box) object)
    (extend-bounding-box box (object-points object)))
  (:method ((box bounding-box) (object null))
    box)
  (:method ((box bounding-box) (sequence sequence))
    (map 'nil (lambda (o)
                (setf box (extend-bounding-box box o)))
         sequence)
    box)
  (:method ((box bounding-box) (object point))
    (let+ (((&bounding-box-r/o x y) box)
           ((&point-r/o x-o y-o) object)
           ((&flet coordinate (coordinate)
              ;; NOTE currently the absolute part is ignored, should we do
              ;; something about that?
              (flex-relative (ensure-flex coordinate)))))
      (make-bounding-box :x (extend-interval x (coordinate x-o))
                         :y (extend-interval y (coordinate y-o)))))
  (:method ((box bounding-box) (other-box bounding-box))
    (let+ (((&bounding-box-r/o x y) box)
           ((&bounding-box-r/o x-o y-o) other-box))
      (make-bounding-box :x (extend-interval x x-o)
                         :y (extend-interval y y-o)))))

(defgeneric bounding-box (object)
  (:documentation "Return a BOUNDING-BOX for OBJECT, which may be a container
of plotting objects or a single plotting object.")
  (:method (object)
    (extend-bounding-box (make-bounding-box :x nil :y nil) object)))

(defun square-bounding-box (&rest objects)
  "Return a square bounding box for OBJECTS by first finding their bounding
box, then extending both intervals to their common hull.  The function can
also be used to square another bounding box."
  (let+ (((&bounding-box-r/o x y) (bounding-box objects))
         (h (interval-hull (list x y))))
    (make-bounding-box :x h :y h)))

(defun nonempty-bounding-box? (bounding-box)
  "Test if a bounding box is non-empty ("
  (let+ (((&structure-r/o bounding-box- x y) bounding-box))
    (and (typep x 'finite-interval)
         (typep y 'finite-interval))))

(deftype nonempty-bounding-box ()
  '(satisfies nonempty-bounding-box?))

(defclass non-extending-object-mixin ()
  ()
  (:documentation "Mixin class for objects that do not extend the bounding
box."))

(defmethod extend-bounding-box ((box bounding-box)
                                (object non-extending-object-mixin))
  box)

(defstruct (strut (:constructor make-strut%))
  "Objects that are only used for extending the bounding box, otherwise they
are not rendered."
  (bounding-box nil :type bounding-box :read-only t))

(defmethod render (target (strut strut)))

(defmethod extend-bounding-box ((box bounding-box) (strut strut))
  (extend-bounding-box box (strut-bounding-box strut)))

(defun strut (&rest objects)
  "Create a STRUT with the bounding box of OBJECTS."
  (make-strut% :bounding-box (bounding-box objects)))
