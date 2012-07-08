;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;;; Axes, ticks, and scales
;;;
;;; A SCALE is an object used directly in the plotting process.  It already
;;; contains all the information for plotting, including the projection, the
;;; orientation, and all the tickmarks and annotations.  SCALEs are usually
;;; automatically generated.
;;;
;;; TICKS is a collection of tickmarks.
;;;
;;; An AXIS summarizes all the information needed for the (auto) generation of
;;; axes.


;;;; margins and titles -- common parts

(defclass title (simple-print-object-mixin margin-mixin)
  ((string :initarg :string)
   (size :initarg :size :initform 12)
   (color :initarg :color :initform +black+)))

(defparameter *default-title-margin* (flex 0 20)
  "Default margin for titles.")

(defgeneric title (object &key &allow-other-keys)
  (:documentation "Create a title from OBJECT.")
  (:method (string &rest keys)
    (apply #'make-instance 'title :string string
                                  :margin *default-title-margin* keys))
  (:method ((n null) &key)
    (make-instance 'title :string "" :margin 0))
  (:method ((title title) &key)
    title))

(defclass title-mixin (simple-print-object-mixin)
  ((title :initarg :title :initform nil))
  (:documentation "Mixin class for objects which contain (or provide) a
title."))

(defmethod initialize-instance :after ((object title-mixin)
                                       &key &allow-other-keys)
  (let+ (((&slots title) object))
    (setf title (title title))))

(defmethod title ((object title-mixin) &key)
  (slot-value object 'title))

(defmethod margin (orientation (object title-mixin))
  (flex+ (margin orientation (slot-value object 'title))
         (margin orientation object)))

(defmethod render-with-orientation (orientation frame (title title))
  (let+ (((&slots-r/o string color) title))
    (unless string
      (return-from render-with-orientation))
    (pgf-set-fill-color color)         ; FIXME this probably won't affect text
    (pgf-text (center frame) string
              :rotate (ecase orientation
                        ((:top :bottom) 0)
                        (:left 90)
                        (:right 270)))))

;;; axis

(defclass axis (margin-mixin title-mixin simple-print-object-mixin)
  ((mark-length :initarg :mark-length)
   (annotation-distance :initarg :annotation-distance)
   (marks-number :initarg :marks-number)
   (interval :initarg :interval :type (or interval function)
             :documentation "An interval, or a function which is called on the
             bounding box.  Used to transform the interval obtained from the
             bounding box (NIL leaves it unaffected)."))
  (:documentation "FIXME."))

(defgeneric transform-interval (object interval)
  (:documentation "Transform interval using OBJECT (usually an axis).")
  (:method ((axis axis) interval)
    (transform-interval (slot-value axis 'interval) interval))
  (:method ((interval interval) ignored)
    interval)
  (:method ((function function) interval)
    (funcall function interval))
  (:method ((function null) interval)
    interval))

(defparameter *axis-margin* (orientation-dependent (flex 0 45) (flex 0 35))
  "Default decoration margin for axes.")

(defparameter *axis-mark-length* (absolute 3)
  "Length of tickmarks.")

(defparameter *axis-annotation-distance* (absolute 6)
  "Distance of annotation from axis line.")

(defparameter *axis-marks-number* 7
  "Default target for the number of marks.")

(defun axis (title
             &key (margin *axis-margin*)
                  (mark-length *axis-mark-length*)
                  (annotation-distance *axis-annotation-distance*)
                  (marks-number *axis-marks-number*)
                  interval)
  "Return an axis."
  (make-instance 'axis :title title :margin margin :mark-length mark-length
                       :annotation-distance annotation-distance
                       :marks-number marks-number :interval interval))

(defgeneric ensure-axis (object)
  (:documentation "Return OBJECT converted to an axis (or a transformation) if
  necessary.")
  (:method (title)
    (axis title))
  (:method ((axis axis))
    axis))

;;;; ticks and scales

(defclass ticks (simple-print-object-mixin)
  ((positions :initarg :positions)
   (marks :initarg :marks)
   (annotations :initarg :annotations))
  (:documentation "Collection of tickmarks and annotations."))

(defclass at-base (simple-print-object-mixin)
  ((base :type (integer 2) :initarg :base))
  (:documentation "Transform ticks at integers N to BASE^N, discarding the
rest."))

(defgeneric transform-ticks (ticks transformation)
  (:documentation "Transform ticks.  Use a list to chain them together.")
  (:method ((ticks ticks) (list list))
    (reduce #'transform-ticks list :from-end t :initial-value ticks))
  (:method ((ticks ticks) (transformation (eql :marks-only)))
    (let+ (((&slots-r/o positions marks annotations) ticks))
      (make-instance 'ticks :positions positions :marks marks
                            :annotations (mapcar (constantly nil)
                                                 annotations))))
  (:method ((ticks ticks) (at-base at-base))
    (let+ (((&slots-r/o positions marks) ticks)
           ((&slots-r/o base) at-base))
      (make-instance 'ticks :positions positions :marks marks
                            :annotations (mapcar
                                          (lambda (x)
                                            (typecase x
                                              ((integer * 1)
                                               (format nil "1/~d"
                                                       (expt base x)))
                                              ((integer 0 *)
                                               (format nil "~d"
                                                       (expt base x)))
                                              (t nil)))
                                          positions)))))



;;; FIXME documentation

(defclass scale (simple-print-object-mixin)
  ((projection :initarg :projection :reader projection)
   (ticks :initarg :ticks)
   (annotation-distance :initarg :annotation-distance))
  (:documentation "FIXME"))

(defparameter *scale-line-width* 0.5
  "Width of lines used when drainwg the tickmarks and the scale box.")

(defmethod render-with-orientation (orientation frame (scale scale))
  (let+ (((&slots-r/o projection ticks annotation-distance) scale)
         ((&slots-r/o positions marks annotations) ticks)
         ((&flet p (parallel orthogonal)
            (project frame
                     (ecase orientation
                       (:left (point (flex- 1 orthogonal)
                                     (project projection parallel)))
                       (:right (point orthogonal
                                      (project projection parallel)))
                       (:bottom (point (project projection parallel)
                                       (flex- 1 orthogonal)))
                       (:top (point (project projection parallel)
                                    orthogonal)))))))
    (pgf-set-line-width *scale-line-width*)
    (pgf-set-color +black+)
    (loop for position in positions
          for mark in marks
          for annotation in annotations
          do (pgf-lines (list (p position 0) (p position mark)))
             (when annotation
               (pgf-text (p position annotation-distance) annotation
                         :align orientation)))))

(defun scale-frame (frame)
  "Draw the rectangle that scales lie on."
  (pgf-set-line-width *scale-line-width*)
  (pgf-set-stroke-color +black+)
  (pgf-frame-rectangle frame)
  (pgf-stroke))

(defgeneric generate-ticks (marks-number axis interval)
  (:documentation "Return autogenerated TICKS, places in
INTERVAL (approximately, extended if necessary -- the the limits may not
coincide with those of INTERVAL), targeting MARKS-NUMBER tickmarks.  AXIS
determines the "))

(defgeneric generate-scale (orientation axis interval)
  (:documentation "Generate a SCALE object mapping from INTERVAL, using the
  AXIS specification.")
  (:method (orientation (axis axis) (interval interval))
    (let+ (((&slots-r/o annotation-distance marks-number) axis)
           (interval (transform-interval axis interval))
           (ticks (generate-ticks (orientation orientation marks-number)
                                  axis interval))
           (domain (interval-hull (list interval
                                        (slot-value ticks 'positions)))))
      (make-instance 'scale :projection (make-coordinate-projection domain
                                                                    :linear)
                            :ticks ticks
                            :annotation-distance (orientation orientation
                                                              annotation-distance)))))

(defclass scale-transformation (margin-mixin title-mixin simple-print-object-mixin)
  ((annotation-distance :initarg :annotation-distance)
   (tick-transformation :initarg :tick-transformation))
  (:documentation "Transforms a scale to another one.  Similar in concept to
AXIS."))

(defmethod transform-ticks (ticks (transformation scale-transformation))
  (transform-ticks ticks (slot-value transformation 'tick-transformation)))

(defun transformation (&key (ticks :marks-only) title
                            (margin (if (eq ticks :marks-only)
                                        *axis-annotation-distance*
                                        *axis-margin*))
                            (annotation-distance *axis-annotation-distance*))
  "Scale transformation."
  (make-instance 'scale-transformation :title title :margin margin
                                       :annotation-distance annotation-distance
                                       :tick-transformation ticks))

(defun transform-scale (scale transformation)
  "Transform scale."
  (remake-object scale
                 :ticks (transform-ticks (slot-value scale 'ticks)
                                         transformation)))


;;;; linear ticks

(defstruct (linear-ticks (:constructor linear-ticks (start step count)))
  "Linear ticks."
  start step count)

(defun flexible-ticks (interval m &key (qs #(1 5 2 10/4 4 3))
                                       only-loose?
                                       (simplicity-weight 1/4)
                                       (coverage-weight 1/5)
                                       (density-weight 1/2)
                                       (minimum-width
                                        (* least-positive-double-float 100)))
  "Find an optimal placement of tick marks on INTERVAL, returning (values
start step count).

Parameters:

  M is the target number of tick marks

  QS is a vector of 'nice' numbers between above 1 (weakly) and below 10,
    should be rationals if you want rational results

  Weights are use in the optimization algorithm.  Simplicity aims for 'nice'
  numbers that come earlier in the list, intervals containing zero, and step
  sizes that are smaller multiples of nice number.  Coverage aims to match the
  interval covered by the ticks to the data interval, with ONLY-LOOSE? forcing
  extreme ticks to lie (weakly) outside the interval.  Density matches the
  density of tickmarks to the density implicitly defined by m and the width of
  data-interval.  Intervals below MINIMUM-WIDTH are just divided evenly.

Notes:

  The implementation follows Talbot, J. and Lin, S. and Hanrahan, P. (2010).
  An Extension of Wilkinson's Algorithm for Positioning Tick Labels on Axes.
  IEEE Transactions on Visualization and Computer Graphics, 16(4), 1036--1043."
  (let+ (((&interval data-min data-max) interval)
         (range (- data-max data-min))
         (coverage-normalizer (* 2 (expt (/ range 10) 2)))
         (max-q-index (1- (length qs)))
         ((&flet weighted (simplicity density coverage)
            (+ (* simplicity simplicity-weight)
               (* density density-weight)
               (* coverage coverage-weight))))
         (best-score -2)
         best-min best-step best-count
         ((&flet prune? (simplicity &optional (density 1) (coverage 1))
            (and best-score
                 (< (weighted simplicity density coverage) best-score))))
         (m (max m 2)))
    (assert (<= 0 range))
    (when (< range minimum-width)
      ;; (return-from flexible-ticks (numseq data-min data-max :length m))
      (error "undefined"))
    (iter j-loop                        ; skip
      (for j :from 1)
      (iter                             ; q
        (for q :in-vector qs :with-index q-index)
        (for simplicity-max := (- 2 (/ q-index max-q-index) j))
        (when (prune? simplicity-max) (return-from j-loop))
        (iter                           ; number of ticks
          (for k :from 2)
          (for density-max := (if (>= k m) (- 2 (/ (1- k) (1- m))) 1))
          (when (prune? simplicity-max density-max) (return))
          (iter                         ; exponent
            (for z :from (ceiling (log (/ range (1+ k) j q) 10)))
            (for step := (* j q (expt 10 z)))
            (for coverage-max := (let* ((span (* step (1- k)))
                                        (half (/ (- span range) 2)))
                                   (if (plusp half)
                                       (- 1 (/ (* 2 (expt half 2))
                                               coverage-normalizer))
                                       1)))
            (when (prune? simplicity-max density-max coverage-max) (return))
            (let ((min-start (* j (- (floor (/ data-max step)) (1- k))))
                  (max-start (* j (ceiling (/ data-min step)))))
              (iter
                (for start :from min-start :to max-start)
                (for min = (* start (/ step j)))
                (for max = (+ min (* step (1- k))))
                (for score :=
                     (weighted (- (if (<= min 0 max) 2 1)
                                  (/ q-index max-q-index)
                                  j)
                               (let ((density (/ (1- k) (- max min)))
                                     (target (/ (1- m)
                                                (- (max max data-max)
                                                   (min min data-min)))))
                                 (- 2 (if (< density target)
                                          (/ target density)
                                          (/ density target))))
                               (- 1 (/ (+ (expt (- data-max max) 2)
                                          (expt (- data-min min) 2))
                                       coverage-normalizer))))
                (when (and (or (not best-score) (< best-score score))
                           (or (not only-loose?)
                               (and (<= min data-min)
                                    (>= max data-max))))
                  (setf best-score score
                        best-min min
                        best-step step
                        best-count k))))))))
    (assert best-min)
    (linear-ticks best-min best-step best-count)))

(defun format-rational (rational precision)
  "Return a string that contains the decimal representation of RATIONAL with
PRECISION digits after the decimal dot.  When PRECISION is not positive,
RATIONAL is rounded to an integer and printed as one. "
  (if (plusp precision)
      (let* ((digits (format nil "~v,'0d" precision
                             (round (* (abs rational) (expt 10 precision)))))
             (dot-index (- (length digits) precision)))
        (format nil "~A~A.~A"
                (if (minusp rational) "-" "")
                (if (zerop dot-index)
                    "0"
                    (subseq digits 0 dot-index))
                (subseq digits dot-index)))
      (format nil "~D" (round rational))))

(defun axis-annotation (position digits-after-decimal)
  "Return an object suitable for displaying as an axis annotation.
DIGITS-AFTER-DECIMAL gives the number of digits after the decimal point."
  (format nil "\\flexaxisannotation{~A}"
          (atypecase position
            (rational (format-rational it digits-after-decimal))
            (t (if (plusp digits-after-decimal)
                   (format nil "~,vf" digits-after-decimal position)
                   (format nil "~d" position))))))

(defun digits-after-decimal (rational)
  "Return the digits after the decimal point for a rational number."
  (aetypecase rational
    (integer 0)
    (rational (ceiling (log (denominator it) 10 )))))

(defmethod generate-ticks (marks-number (axis axis) (interval interval))
  (let+ (((&slots-r/o mark-length) axis)
         ((&structure-r/o linear-ticks- start step count)
          (flexible-ticks interval marks-number))
         (digits (max (digits-after-decimal step)
                      (digits-after-decimal start))))
    (iter
      (repeat count)
      (for position :from start :by step)
      (collect mark-length :into marks)
      (collect position :into positions)
      (collect (axis-annotation position digits) :into annotations)
      (finally
       (return
         (make-instance 'ticks :positions positions :marks marks
                               :annotations annotations))))))
