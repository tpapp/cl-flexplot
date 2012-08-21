;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;;

(defstruct (mark (:constructor mark (point object)))
  (point nil :type point :read-only t)
  (object nil :read-only t))

(defmethod object-points ((mark mark))
  (mark-point mark))

(defmethod render ((da drawing-area) (mark mark))
  (let+ (((&structure-r/o mark- point object) mark))
    (render (origin-drawing-area da point) object)))

(defun marks (points &optional (object (circle)))
  "Marks with given OBJECT at POINTS."
  (map 'vector (lambda (point)
                 (mark point object))
       points))

(defun mark-xy (x y &optional (object (circle)))
  (mark (point x y) object))

(defun marks-xy (xs ys &optional (object (circle)))
  "Marks with given OBJECT at (X,Y) pairs.."
  (map 'vector (lambda (x y)
                 (mark (point x y) object))
       xs ys))

;;;; circles

(defparameter *circle-size* 3)
(defparameter *circle-fill* nil)
(defparameter *circle-stroke* *stroke-style*)

(defstruct (circle
            (:constructor circle (&key (size *circle-size*)
                                       (fill *circle-fill*)
                                       (stroke *circle-stroke*))))
  (size nil :type real :read-only t)
  (fill nil :type fill-style* :read-only t)
  (stroke nil :type stroke-style* :read-only t))

(defmethod render ((da origin-drawing-area) (circle circle))
  (let+ (((&structure-r/o circle- size fill stroke) circle))
    (pgf-set-stroke-style stroke)
    (pgf-set-fill-style fill)
    (pgf-path-circle (project da +origin+) (/ size 2))
    (pgf-use-path :fill fill :stroke stroke)))

;;; label

(defparameter *label-color* +black+)

(defstruct (label
            (:constructor label (text &optional (color *label-color*))))
  (color nil :read-only t)
  (text nil :type string :read-only t))

(defmethod render ((da origin-drawing-area) (label label))
  (let+ (((&structure-r/o label- color text) label))
    (pgf-set-color color)
    (pgf-text (project da +origin+) text)))
