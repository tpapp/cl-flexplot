;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; stroke styles

(defstruct stroke-style
  "Line style."
  (width 1)
  (color +black+)
  (dash nil)
  (phase 0)
  (opacity 1 :type (real 0 1) :read-only t))

(deftype stroke-style* ()
  "Extended stroke style."
  '(or null stroke-style))

(defun pgf-set-stroke-style (stroke-style)
  (when stroke-style
    (let+ (((&structure-r/o stroke-style- width color dash phase opacity)
            stroke-style))
      (pgf-set-line-width width)
      (pgf-set-stroke-color color)
      (pgf-set-dash dash phase)
      (pgf-set-stroke-opacity opacity))))

(defparameter *stroke-style* (make-stroke-style)
  "Default stroke-style.")

(defun stroke-style (&key (width (stroke-style-width *stroke-style*))
                          (color (stroke-style-color *stroke-style*))
                          (dash (stroke-style-dash *stroke-style*))
                          (phase (stroke-style-phase *stroke-style*))
                          (opacity (stroke-style-opacity *stroke-style*)))
  "Create a stroke-style.  Defaults are taken from *STROKE-STYLE*."
  (make-stroke-style :width width :color color :dash dash :phase phase
                     :opacity opacity))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun dash-even (spacing)
    "Evenly placed dash."
    (list spacing spacing)))

(define-constant +dash-solid+ '() :test #'equal)
(define-constant +dash-dot+ (dash-even 1) :test #'equal)
(define-constant +dash-line+ (dash-even 4) :test #'equal)

;;; fill style

(defstruct fill-style
  "Fill style."
  (color +black+))

(deftype fill-style* ()
  "Extended fill style."
  '(or null fill-style))

(defun pgf-set-fill-style (fill-style)
  (when fill-style
    (let+ (((&structure-r/o fill-style- color) fill-style))
      (pgf-set-fill-color color))))

(defparameter *fill-style* (make-fill-style :color +black+))

(defun fill-style (&key (color (fill-style-color *fill-style*)))
  "Create a fill-style.  Defaults are taken from *FILL-STYLE*."
  (make-fill-style :color color))
