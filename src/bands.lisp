;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

(defparameter *band-style* (fill-style :color +gray70+))

(defclass vertical-band (simple-print-object-mixin non-extending-object-mixin)
  ((fill-style :initarg :fill-style)
   (horizontal-interval :initarg :horizontal-interval)))

(defun vertical-band (interval &optional (fill *band-style*))
  (make-instance 'vertical-band :horizontal-interval interval :fill-style fill))

(defmethod render ((da drawing-area) (band vertical-band))
  (with-drawing-area (da project)
    (let+ (((&ign vertical-interval) (domain da))
           ((&slots-r/o horizontal-interval fill-style) band)
           ((&flet pp (accessor)
              (project (point (funcall accessor horizontal-interval)
                              (funcall accessor vertical-interval))))))
      (pgf-set-fill-style fill-style)
      (pgf-rectangle (pp #'left) (pp #'right))
      (pgf-fill))))
