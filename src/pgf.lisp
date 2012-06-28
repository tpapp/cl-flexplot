;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

(defun pgf-text-align-string (align)
  "Return a string for valid text alignment options."
  (ecase align
    (:left "left")
    (:right "right")
    (:base "base")
    (:base-left "base,left")
    (:base-right "base,right")
    (:top "top")
    (:top-left "top,left")
    (:top-right "top,right")
    (:bottom "bottom")
    (:bottom-left "bottom,left")
    (:bottom-right "bottom,right")))

(defun pgf-text (position text &key align rotate)
  (latex
    (:pgftext &optional
              (latex "at=" position
                (when rotate
                  (latex ",rotate=" rotate))
                (when align
                  (latex "," (pgf-text-align-string align))))
              text)
    :/))

(defun pgf-move-to (position)
  (latex (:pgfmoveto position) :/))

(defun pgf-line-to (position)
  (latex (:pgflineto position) :/))

(defun pgf-rectangle (corner1 corner2)
  (latex (:pgfpathrectanglecorners corner1 corner2) :/))

(defun pgf-stroke ()
  (latex (:pgfusepath "stroke") :/))

(defun pgf-set-fill-color (color)
  (let+ (((&rgb red green blue) (as-rgb color)))
    (latex (:pgfsys@color@rgb@fill red green blue))))

(defun pgf-fill ()
  (latex (:pgfusepath "fill") :/))

(defun pgf-fill ()
  (latex (:pgfusepath "fill") :/))
