;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)



;;; basic layer

(with-displayed-picture ()
  ;; homogenous random color
  (random-tint +unit-frame+))

(defparameter *3div* (make-sequence 'vector 3
                                    :initial-element (flex-spacer)))

(with-displayed-picture ()
  ;; 3x3 checked pattern, random colors
  (random-tint (split +unit-frame+ *3div* *3div*)))

(with-displayed-picture ()
  ;; 3 vertical bands, random colors
  (random-tint (split-h +unit-frame+ *3div*)))

(with-displayed-picture ()
  ;; 3 horizontal bands, random colors
  (random-tint (split-v +unit-frame+ *3div*)))

(with-displayed-picture ()
  ;; rectangle in the middle, random color, surrounded by background frame
  (random-tint (shrink +unit-frame+ 1/4)))

(with-displayed-picture ()
  ;; rectangle in the middle, with horizontal and vertical grid lines
  (let ((frame (shrink +unit-frame+ 1/4)))
    (random-tint frame)
    (loop for u from 0 to 1 by (/ 10)
          do (pgf-lines (mapcar (curry #'project frame)
                                (list (point u 0) (point u 1)))))
    (loop for v from 0 to 1 by (/ 5)
          do (pgf-lines (mapcar (curry #'project frame)
                                (list (point 0 v) (point 1 v)))))))


;;; plotting layer
;;;
;;; testing primitives that plots are built from

(with-displayed-picture ()
  ;; rectangle in middle, grid lines, everything drawn using relative
  ;; coordinates
  (let* ((frame (shrink +unit-frame+ 1/4))
         (drawing-area (make-drawing-area
                        frame
                        (make-coordinate-projection (interval -1 1) :linear)
                        (make-coordinate-projection (interval 2 9) :linear))))
    (random-tint frame)
    (with-projection (drawing-area project)
      (loop for u from -1 to 1 by (/ 10)
            do (pgf-lines (mapcar #'project (list (point u 2) (point u 9)))))
      (loop for v from 2 to 9 by (/ 2)
            do (pgf-lines (mapcar #'project
                                  (list (point -1 v) (point 1 v))))))))

(with-displayed-picture ()
  ;; plotting area split 5 ways
  (let+ ((p (pt 10))
         (l (split5 +unit-frame+ p p)))
    (random-tint l)))

(defstruct (margin-test (:constructor margin-test (title margin)))
  title margin)

(defmethod margin flex+ (orientation (margin-test margin-test))
  (orientation orientation (margin-test-margin margin-test)))

(defmethod render-with-orientation (orientation (frame frame)
                                    (margin-test margin-test))
  (declare (ignore orientation))
  (pgf-set-stroke-style *stroke-style*)
  (let+ (((&flet p (x y)
            (project frame (point x y)))))
    (pgf-set-color +black+)
    (pgf-lines (list (p 0 0) (p 1 1)))
    (pgf-lines (list (p 0 1) (p 1 0)))
    (pgf-set-color +red+)
    (pgf-text (p 0.5 0.5) (margin-test-title margin-test))))

(with-displayed-picture ()
  (sides :left +unit-frame+ +unit-frame+
         (list* (margin-test "\\textbf{1}" (pt 50))
                (margin-test "\\textbf{2}" (em 10))
                (margin-test "\\textbf{rest}" (flex 40)))))

;; (with-displayed-picture ()
;;   (let+ (((center left bottom right top) (split5 +unit-frame+
;;                                                  (flex-spacer (flex 1/4 60))
;;                                                  (flex-spacer (flex 1/4 40))))
;;          (s-left (generate-scale :left (axis "left") (interval -2 3)))
;;          (s-bottom (generate-scale :bottom (axis "bottom") (interval -7 1)))
;;          (s-right (generate-scale :right (axis "right") (interval 0 9)))
;;          (s-top (generate-scale :top (axis "top") (interval -2 3))))
;;     (random-tint center)
;;     (render-with-orientation left s-left :left)
;;     (render-with-orientation right s-right :right)
;;     (render-with-orientation bottom s-bottom :bottom)
;;     (render-with-orientation top s-top :top)))

;;; plots

(displaying
 ;; simple plot, 3 standard guides, line
 (plot
  (list
   (horizontal-guide 0.5)
   (vertical-guide 0.2)
   (diagonal-guide)
   (lines (list (point 0.1 0)
                (point 1 1))))))

(displaying
 ;; line and vertical band
 (plot
  (list
   (vertical-band (interval 0.3 0.8))
   (lines (list (point 1 0)
                (point 0 1))))))

(displaying
 ;; sine function, with guide
 (plot
  (list
   (horizontal-guide 0)
   (lines (fx #'sin (interval (- pi) pi))))))

(displaying
 ;; cumulative sum of random numbers, series plot
 (plot
  (lines (ty (let* ((length 100)
                    (sum 0d0))
               (aprog1 (make-array length)
                 (loop for index below length
                       do (setf (aref it index) sum)
                          (incf sum (random 1d0)))))))
  :x-axis (math "t")
  :y-axis "cumulative sum"))

(displaying
 ;; 3 circles, testing mark
 (plot
  (let ((c (circle)))
    (list
     (mark (point 0 1) c)
     (mark (point 1 0) c)
     (mark +origin+ c)))))

(displaying
 ;; marks along a circle
 (plot
  (map 'list
       (let ((c (circle)))
         (lambda (theta)
           (mark (point (sin theta) (cos theta)) c)))
       (grid-in (interval 0 (* 2 pi)) 25))))


;;; quantiles

(require :cl-random)

;;; QQ plot
(let* ((y (generate-array 500 (lambda () (+ (random 1d0) (random 1d0))))))
  (displaying (plot
               (list
                (diagonal-guide)
                (qy y :function (curry #'quantile (rv:r-normal (mean y) (variance y)))))
               :x-axis "equivalent normal"
               :y-axis "quantile")))

;;; error bars
(displaying
 (plot
  (let ((index 0))
    (map 'list
         (lambda (center w1 w2)
           (prog1 (q5-y index
                        (vector (- center w2) (- center w1)
                                center
                                (+ center w1) (+ center w2))
                        :mark (label (format nil "~D" index)))
             (incf index)))
         (numseq 0 5 :length 6)
         (numseq 0.4 0.7 :length 6)
         (numseq 0.9 2 :length 6)))))


;;; functions

(let ((x (interval 0 10)))
  (displaying
   (plot
    (list
     (lines (fx #'sin x))
     (lines (fx (lambda (x) (* 2 (cos x))) x)
            (stroke-style :color +grey+))))))

;; ;;; panel

;; (let ((m (outer* #(1 2 3) #(3 5)
;;                  (lambda (a b)
;;                    (lines (vector (xy 0 a)
;;                                   (xy 1 b)))))))
;;   (displaying (400 300)
;;               (panel-plot m
;;                           :x-title (title "x")
;;                           :y-title (title "y")
;;                           :x-axes (axis nil)
;;                           :y-axes (axis nil)
;;                           :panel-titles (title "foo"))))

;; (displaying (400 300)
;;             (let ((titles (list "sin" "cos")))
;;               (panel-plot
;;                (make-array '(2 1) :initial-contents
;;                            (list (list (lines (fx #'sin (interval 0 (* 2 pi)))))
;;                                  (list (lines (fx #'cos (interval 0 (* 2 pi)))))))
;;                :panel-titles titles
;;                :y-axes (mapcar #'axis titles))))

;; (let* ((a (generate-array 100 (rv:generator (rv:r-normal))))
;;        (b (generate-array 50 (rv:generator (rv:r-normal 4))))
;;        (c (generate-array 80 (rv:generator (rv:r-normal 0 2))))
;;        (abc (sort-reals (concatenate 'vector a b c))))
;;   (displaying (400 300)
;;               (panel-plot
;;                (matrix (list (list
;;                               (lines (qq abc a))
;;                               (diagonal-guide))
;;                              (list
;;                               (lines (qq abc b))
;;                               (diagonal-guide)))
;;                        (list (list
;;                               (lines (qq abc c))
;;                               (diagonal-guide))
;;                              nil))
;;                :uniform-x? t :uniform-y? t)))

;; ;;; grid

;; (flet ((f (function y-title)
;;          (plot
;;           (lines (fx function (interval 0 (* 2 pi))))
;;           :y-axis (axis y-title))))
;;   (displaying (500 300)
;;               (grid (matrix (list (f #'sin "sin(x)"))
;;                             (list (f #'cos "cos(x)"))))))

;; (let+ (((&flet f (function y-title &optional (interval (interval 0 (* 2 pi))))
;;           (plot
;;            (lines (fx function interval))
;;            :y-axis (axis y-title))))
;;        ((&flet p (degree)
;;           (f (lambda (x) (expt x degree)) (format nil "x^~d" degree)
;;              (interval -1 1)))))
;;   (displaying (500 500)
;;               (grid (matrix (list (f #'sin "sin(x)") (f #'cos "cos(x)"))
;;                             (list (grid (matrix (list (p 2) (p 3))
;;                                                 (list (p 4) (p 5))))
;;                                   nil)))))


;;; categories

(displaying
    (plot (list (mark (point 0 0) (circle))
                (mark (point 0.5 0) (circle))
                (mark (point 0 1) (circle))
                (mark (point 1 1) (circle)))
     :y-axis (categories "y" '("foo" "bar"))))


;;; marks

(displaying
    (plot (list (mark (point 0 0) (circle :size 9))
                (mark (point 1 1) (circle :size 7 :stroke nil
                                          :fill (fill-style :color +blue+)))
                (mark (point 0.5 0.5) (label "label")))))
