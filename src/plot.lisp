;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)



(defparameter *outer-gap* (pt 3)
  "Gap between axes and drawing area clip box.")

(defparameter *inner-gap* (pt 8)
  "Gap between drawing area and its clip box.")

(defclass plot ()
  ((bounding-box :reader bounding-box :initarg :bounding-box)
   (objects :initarg :objects)
   (x-axis :accessor x-axis :initarg :x-axis)
   (y-axis :accessor y-axis :initarg :y-axis)
   (x-other-axis :accessor x-other-axis :initarg :x-other-axis)
   (y-other-axis :accessor y-other-axis :initarg :y-other-axis)))

(defun plot (objects
             &key (x-axis (axis (math "x")))
                  (y-axis (axis (math "y")))
                  (x-other-axis (transformation :ticks :marks-only))
                  (y-other-axis (transformation :ticks :marks-only)))
  (let ((bounding-box (bounding-box objects)))
    (check-type bounding-box nonempty-bounding-box)
    (make-instance
     'plot
     :bounding-box bounding-box
     :objects objects
     :x-axis (ensure-axis x-axis)
     :y-axis (ensure-axis y-axis)
     :x-other-axis x-other-axis
     :y-other-axis y-other-axis)))

(defmethod render (frame (plot plot))
  (let+ (((&slots-r/o objects bounding-box x-axis y-axis
                      x-other-axis y-other-axis) plot)
         ((&bounding-box bb-x bb-y) bounding-box)
         ((center . sides)
          (split5 frame
                  (margin :left y-axis) (margin :bottom x-axis)
                  (margin :right y-other-axis) (margin :top x-other-axis)))
         (clip-box (shrink center *outer-gap*))
         (graph-box (shrink clip-box *inner-gap*))
         (bottom-scale (generate-scale :bottom x-axis bb-x))
         (left-scale (generate-scale :left y-axis bb-y)))
    (let* ((x-projection (projection bottom-scale))
           (y-projection (projection left-scale))
           (da (make-drawing-area graph-box x-projection y-projection)))
      (with-clip-to-frame clip-box
        (render da objects))
      (scale-frame center)
      (sides4 sides
              graph-box
              (cons (title y-axis) left-scale)
              (cons (title x-axis) bottom-scale)
              (cons (title y-other-axis)
                    (transform-scale left-scale y-other-axis))
              (cons (title x-other-axis)
                    (transform-scale bottom-scale x-other-axis))))))
