;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

(defclass margin-mixin ()
  ((margin :initarg :margin))
  (:documentation "Mixing class for objects which are placed on a margin.
When dividing up frames, the renderer queries the object for the desired
margin size using MARGIN."))

(defgeneric margin (orientation object)
  (:documentation "Return the desired margin size when OBJECT is rendered
using the given orientation.")
  (:method-combination +)
  (:method + (orientation object)
    (orientation orientation object))
  (:method + (orientation (m margin-mixin))
    (margin orientation (slot-value m 'margin)))
  (:method + (orientation (list list))
    (reduce #'+ list :key (curry #'margin orientation))))

(defun max-margin (orientation objects)
  "Largest margin among objects."
  (reduce #'max (aetypecase objects
                  (sequence it)
                  (array (flatten-array it)))
          :key (curry #'margin orientation)))

(defgeneric render-in-side (orientation frame object)
  (:documentation "Render OBJECT in FRAME, with the given orientation.")
  (:method (orientation frame (object null))
    (values)))

(defun side (orientation frame object)
  "Split off a margin, render the object in it, and return the remaining frame."
  (let+ (((&values side remainder) (split2 frame orientation
                                           (margin orientation object))))
    (render-in-side orientation side object)
    remainder))

(defun sides (orientation frame objects)
  "Similar to SIDE, with multiple objects.  When OBJECTS is a dotted list, the
last cdr is simply rendered, and not used for splitting the frame."
  (loop
    (atypecase objects
      (null (return frame))
      (cons (setf frame (side orientation frame (car objects))
                  objects (cdr objects)))
      (t (render-in-side orientation frame objects)
         (return nil)))))

(defun sides4 (frame-list left bottom &optional right top)
  "Convenience function for four sides (axes, titles, etc) of a plot."
  (let+ (((&flet do% (orientation frame objects)
            (when frame
              (sides orientation frame objects))))
         ((&optional left% bottom% right% top%) frame-list))
    (list
     (do% :left left% left)
     (do% :bottom bottom% bottom)
     (do% :right right% right)
     (do% :top top% top))))
