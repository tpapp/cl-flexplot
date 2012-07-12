;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; lines

(defclass lines (simple-print-object-mixin)
  ((points :type sequence :initarg :points :accessor points)
   (style :type style :initarg :style))
  (:documentation "Line segments."))

(defmethod object-points ((lines lines))
  (points lines))

(defmethod render ((drawing-area drawing-area) (lines lines))
  (let+ (((&slots-r/o points style) lines))
    (with-drawing-area (drawing-area project)
      (pgf-set-stroke-style style)
      (pgf-lines (map 'vector #'project points)))))

(defun lines (points &optional (style *stroke-style*))
  (make-instance 'lines :points points :style style))

;;; guides

(defparameter *guide-style* (make-stroke-style :width 0.5 :color +gray50+)
  "Default guide style.")

(defclass guide (simple-print-object-mixin non-extending-object-mixin)
  ((intercept :initarg :intercept :type real)
   (slope :initarg :slope :type (or real null))
   (style :initarg :style :type stroke-style))
  (:documentation "A guide on the locus y=intercept+slope*x.  When SLOPE is
NIL, the locus is x=INTERCEPT."))

(defmethod render ((da drawing-area) (guide guide))
  (let+ (((&slots-r/o intercept slope style) guide)
         (points
          (if slope
              (let+ (((&interval left right) (first (domain da)))
                     ((&flet p (x) (point x (+ intercept (* slope x))))))
                (list (p left) (p right)))
              (let+ (((&interval left right) (second (domain da))))
                (list (point intercept left)
                      (point intercept right))))))
    (render da (lines points style))))

(defun guide (intercept slope &optional (style *guide-style*))
  (make-instance 'guide :intercept intercept :slope slope :style style))

(defun guide-through2 (p q &optional (style *guide-style*))
  "Guide going through two points."
  (let+ (((&point px py) p)
         ((&point qx qy) q)
         (slope (/ (- qy py) (- qx px))))
    (guide (- py (* slope px)) slope style)))

(defun guide-through (p slope &optional (style *guide-style*))
  "Guide through a point, with given slope."
  (let+ (((&point x y) p))
    (guide (- y (* slope x)) slope style)))

(defun horizontal-guide (y &optional (style *guide-style*))
  (guide y 0 style))

(defun vertical-guide (x &optional (style *guide-style*))
  (guide x nil style))

(defun diagonal-guide (&optional (style *guide-style*))
  (guide 0 1 style))
