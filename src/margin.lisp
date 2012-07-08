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
  (:method (orientation object)
    (orientation orientation object))
  (:method (orientation (m margin-mixin))
    (margin orientation (slot-value m 'margin)))
  (:method (orientation (list list))
    (reduce #'flex+ list :key (curry #'margin orientation))))

(defun max-margin (orientation objects)
  "Largest margin among objects."
  (reduce #'flex-max (aetypecase objects
                       (sequence it)
                       (array (flatten-array it)))
          :key (curry #'margin orientation)))

(defgeneric render-with-orientation (orientation frame object)
  (:documentation "FIXME"))

(defun side (orientation frame replacement-frame object)
  "Split off a margin, render the object in it, and return the remaining frame."
  (let+ (((&values side remainder) (split2 frame orientation
                                           (margin orientation object))))
    (render-with-orientation orientation
                             (replace-with-orientation orientation side
                                                       replacement-frame)
                             object)
    remainder))

(defun sides (orientation frame replacement-frame objects)
  "Similar to SIDE, with multiple objects.  When OBJECTS is a dotted list, the
last cdr is simply rendered, and not used for splitting the frame."
  (loop
    (atypecase objects
      (null (return frame))
      (cons (setf frame (side orientation frame replacement-frame
                              (car objects))
                  objects (cdr objects)))
      (t (render-with-orientation orientation
                                  (replace-with-orientation orientation frame
                                                            replacement-frame)
                                  objects)
         (return nil)))))

(defun sides4 (frame-list replacement-frame left bottom &optional right top)
  "Convenience function for four sides (axes, titles, etc) of a plot."
  (let+ (((&flet do% (orientation frame objects)
            (when frame
              (sides orientation frame replacement-frame objects))))
         ((&optional left% bottom% right% top%) frame-list))
    (list
     (do% :left left% left)
     (do% :bottom bottom% bottom)
     (do% :right right% right)
     (do% :top top% top))))