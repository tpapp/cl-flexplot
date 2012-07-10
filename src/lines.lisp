;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; line styles

(defstruct line-style
  (width 1)
  (color +black+)
  (dash nil)
  (phase 0))

(defun pgf-set-line-style (line-style)
  (let+ (((&structure-r/o line-style- width color dash phase) line-style))
    (pgf-set-line-width width)
    (pgf-set-stroke-color color)
    (pgf-set-dash dash phase)))

(defparameter *line-style* (make-line-style))

(defun line-style (&key (width (line-style-width *line-style*))
                        (color (line-style-color *line-style*))
                        (dash (line-style-dash *line-style*))
                        (phase (line-style-phase *line-style*)))
  (make-line-style :width width :color color :dash dash :phase phase))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun dash-even (spacing)
    "Evenly placed dash."
    (list spacing spacing)))

(define-constant +dash-solid+ '() :test #'equal)
(define-constant +dash-dot+ (dash-even 1) :test #'equal)
(define-constant +dash-line+ (dash-even 4) :test #'equal)

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
      (pgf-set-line-style style)
      (pgf-lines (map 'vector #'project points)))))

(defun lines (points &optional (style *line-style*))
  (make-instance 'lines :points points :style style))

;;; guides

(defparameter *guide-style* (make-line-style :width 0.5 :color +gray50+)
  "Default guide style.")

(defclass guide (simple-print-object-mixin non-extending-object-mixin)
  ((intercept :initarg :intercept :type real)
   (slope :initarg :slope :type (or real null))
   (style :initarg :style :type line-style))
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
