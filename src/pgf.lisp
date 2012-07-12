;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

(defun pgf-text-align-string (align)
  "Return a string for valid text alignment options."
  ;; Note: PGF's align orientation is the exact opposite of what this library
  ;; uses.
  (ecase align
    (:left "right")
    (:right "left")
    (:base "base")
    (:base-left "base,right")
    (:base-right "base,left")
    (:top "bottom")
    (:top-left "bottom,right")
    (:top-right "bottom,left")
    (:bottom "top")
    (:bottom-left "top,right")
    (:bottom-right "top,left")))

(defun pgf-text (position text &key align rotate)
  (latex
    (:pgftext &optional
              (latex "at=" position
                (when rotate
                  (latex ",rotate=" rotate))
                (when align
                  (let ((align-string (pgf-text-align-string align)))
                    (latex "," align-string))))
              text)
    :/))

(defun pgf-path-move-to (position)
  (latex (:pgfpathmoveto position) :/))

(defun pgf-path-line-to (position)
  (latex (:pgfpathlineto position) :/))

(defun pgf-rectangle (corner1 corner2)
  (latex (:pgfpathrectanglecorners corner1 corner2) :/))

(defun pgf-stroke ()
  (latex (:pgfusepath "stroke") :/))

(defun pgf-set-color (color)
  (let+ (((&rgb red green blue) (as-rgb color)))
    (latex (:pgfsys@color@rgb red green blue))))

(defun pgf-set-fill-color (color)
  (let+ (((&rgb red green blue) (as-rgb color)))
    (latex (:pgfsys@color@rgb@fill red green blue))))

(defun pgf-set-stroke-color (color)
  (let+ (((&rgb red green blue) (as-rgb color)))
    (latex (:pgfsys@color@rgb@stroke red green blue))))

(defun pgf-set-line-width (width)
  (latex (:pgfsetlinewidth (latex width "pt"))))

(defun pgf-set-dash (dimensions &optional (phase 0))
  (assert (divides? (length dimensions) 2) ()
          "Dash dimensions need to have an even length.")
  (latex (:pgfsetdash (loop for dimension in dimensions
                            do (latex dimension "pt")) phase) :/))

(defun pgf-fill ()
  (latex (:pgfusepath "fill") :/))

(defun print-comma-separated (stream &rest strings)
  (let ((first t))
    (loop for string in strings
          do (when string
               (if first
                   (setf first nil)
                   (princ #\, stream))
               (princ string stream)))))

(defun pgf-use-path (&key fill stroke clip)
  (latex (:pgfusepath (print-comma-separated *latex-output*
                                             (when fill "fill")
                                             (when stroke "stroke")
                                             (when clip "clip")))))

(defun pgf-reset-bounding-box ()
  (latex (:pgfresetboundingbox) :/))

(defun pgf-use-as-bounding-box ()
  (latex (:pgfusepath "use as bounding box") :/))


;;; convenience functions

(defun pgf-lines (points)
  "Stroke consecutive points.  NIL breaks the line."
  (let+ (open?
         ((&flet ensure-closed ()
            (when open?
              (pgf-stroke)
              (setf open? nil)))))
    (map nil (lambda (point)
               (if point
                   (if open?
                       (pgf-path-line-to point)
                       (progn
                         (pgf-path-move-to point)
                         (setf open? t)))
                   (ensure-closed)))
         points)
    (ensure-closed))
  (values))
