;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct continuous-quantile-rule
    alpha beta))

(defmacro define-continuous-quantile-rule (name alpha beta documentation)
  `(define-constant ,name (make-continuous-quantile-rule
                           :alpha ,alpha :beta ,beta)
     :test #'equalp
     :documentation ,documentation))

(define-continuous-quantile-rule qrule4 0 1 "")
(define-continuous-quantile-rule qrule5 1/2 1/2 "")
(define-continuous-quantile-rule qrule6 0 0 "")
(define-continuous-quantile-rule qrule7 1 1 "")
(define-continuous-quantile-rule qrule8 1/3 1/3 "")
(define-continuous-quantile-rule qrule9 3/8 3/8 "")

(defgeneric quantile-position (quantile-rule index n)
  (:documentation "Return the (plotting) position corresponding to
INDEX (starts at 0) with a total of N observations, according to the given
quantile rule.")
  (:method ((qrule continuous-quantile-rule) index n)
    (let+ (((&structure-r/o continuous-quantile-rule- alpha beta) qrule))
      (/ (- (1+ index) alpha)
         (- (1+ n) alpha beta)))))

(defun qy-thin (ys make-point make-line separate drop)
  "Helper function for QY, returns a list of object designators for quantile
plots.  The algorithm can be described as follows:

  1. Lines are broken when the distance between consecutive points is more
     than SEPARATE (relative to the total range of the data).

  2. Lines with a single point end up as points are passed to
    (MAKE-POINT INDEX Y).

  3. If a line has multiple points, points closer than (again, relative to the
     range) are dropped, what remains is passed to (MAKE-LINE INDEX YS).

Setting either DROP or SEPARATE to NIL disables the respective thinning
functionality.  Uses a one-pass algorithm.  YS must be a sorted vector."
  (let+ ((ys (ensure-sorted-vector ys))
         (min (aref ys 0))
         (max (last-elt ys))
         (width (- max min))
         (separate-gap (when separate
                         (* width separate)))
         (drop-gap (when drop
                     (* width drop)))
         ((&flet gap? (gap a b)
            (and gap (> (- a b) gap))))
         object                       ; list holding (index . y) pairs
         object-unused                ; last pair that didn't end up in object
         objects                      ; constructed objects
         ((&flet add (index-y)
            (if object
                (if (gap? drop-gap (cdr index-y) (cdar object))
                    (progn
                      (push index-y object)
                      (setf object-unused nil))
                    (setf object-unused index-y))
                (push index-y object))))
         ((&flet finish ()
            (swhen object-unused
              (push it object)
              (setf it nil))
            (when object
              (push (if (cdr object)
                        (funcall make-line (nreverse object))
                        (funcall make-point (car object)))
                    objects)
              (setf object nil)))))
    (iter
      (for y :in-vector ys :with-index index)
      (for y-p :previous y)
      (when (or (not y-p) (gap? separate-gap y y-p))
        (finish))
      (add (cons index y)))
    (finish)
    (nreverse objects)))

(defun qy (y &key (mark (circle :size 2)) (stroke *stroke-style*)
                  (drop 1/200) (separate 1/100) (qrule qrule8)
                  (function #'identity))
  "Construct quantile plot for a univariate sequence Y, thinning with QY-THIN
using MARK for points and LINE-STYLE for parts (see that function for
explanation on DROP and SEPARATE).  Uses QRULE to position quantile points.
FUNCTION is called on the X coordinates, and can be used for QQ plots."
  (let+ ((ys (ensure-sorted-vector y))
         (n (length ys))
         ((&flet qy (index-y)
            (point (funcall function (quantile-position qrule (car index-y) n))
                   (cdr index-y)))))
    (qy-thin ys
             (lambda (index-y)
               (mark (qy index-y) mark))
             (lambda (index-ys)
               (lines (map 'vector #'qy index-ys) stroke))
             separate
             drop)))



;;; quantiles for marking uncertainty

(defstruct (q5-y (:constructor q5-y%))
  x y1 y2 y3 y4 y5 mark stroke-style)

(defun q5-y (x ys &key (mark *default-mark*) (stroke-style *stroke-style*))
  (let+ ((#(y1 y2 y3 y4 y5) ys))
    (q5-y% :x x :y1 y1 :y2 y2 :y3 y3 :y4 y4 :y5 y5
           :mark mark :stroke-style stroke-style)))

(define-expansion (da q5-y :use-for-bounding-box t)
  (let+ (((&structure-r/o q5-y- mark x y1 y2 y3 y4 y5 stroke-style) q5-y))
    (list (segment-xy x y1 x y2 stroke-style)
          (segment-xy x y4 x y5 stroke-style)
          (mark-xy x y3 mark))))
