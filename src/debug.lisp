;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

(let+ ((red 0.5)
       (green 0.5)
       (blue 0.5)
       (gap 0.1)
       ((&flet r (original)
          (mod (+ original gap (random (1c gap))) 1))))
  (defun random-color ()
    "Return a random color, taking care to make it different from the previous
one visually."
    (setf red (r red)
          green (r green)
          blue (r blue))
    (rgb red green blue)))

(defgeneric random-tint (object)
  (:documentation "Tint object (usually a frame or a collection of frames with
  a random color.  Return the original object.  Useful for debugging.")
  (:method ((frame frame))
    (pgf-frame-rectangle frame)
    (pgf-set-fill-color (random-color))
    (pgf-fill)
    frame)
  (:method ((array array))
    (map nil #'random-tint (flatten-array array))
    array)
  (:method ((sequence sequence))
    (map nil #'random-tint sequence)
    sequence))
