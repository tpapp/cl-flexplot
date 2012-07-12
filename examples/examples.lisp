;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)



;;; basic layer

(with-displayed-picture ()
  (random-tint +unit-frame+))

(defparameter *3div* (make-sequence 'vector 3
                                    :initial-element (flex-spacer)))

(with-displayed-picture ()
  (random-tint (split +unit-frame+ *3div* *3div*)))

(with-displayed-picture ()
  (random-tint (split-h +unit-frame+ *3div*)))

(with-displayed-picture ()
  (random-tint (split-v +unit-frame+ *3div*)))

(with-displayed-picture ()
  (random-tint (shrink +unit-frame+ 1/4)))

(with-displayed-picture ()
  (let ((frame (shrink +unit-frame+ 1/4)))
    (random-tint frame)
    (loop for u from 0 to 1 by (/ 10)
          do (pgf-lines (mapcar (curry #'project frame)
                                (list (point u 0) (point u 1)))))
    (loop for v from 0 to 1 by (/ 5)
          do (pgf-lines (mapcar (curry #'project frame)
                                (list (point 0 v) (point 1 v)))))))


;;; plotting layer

(with-displayed-picture ()
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
  (let+ ((p (flex 0 10))
         (l (split5 +unit-frame+ p p)))
    (random-tint l)))

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
 (plot
  (list
   (lines (list (point 0.1 0)
                (point 1 1)))
   (horizontal-guide 0.5)
   (vertical-guide 0.2)
   (diagonal-guide))))

(displaying
 (plot
  (list
   (vertical-band (interval 0.3 0.8))
   (lines (list (point 1 0)
                (point 0 1))))))

(displaying
 (plot
  (list
   (horizontal-guide 0)
   (lines (fx #'sin (interval (- pi) pi))))))

(displaying
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
 (plot
  (let ((c (circle)))
    (list
     (mark (point 0 1) c)
     (mark (point 1 0) c)
     (mark +origin+ c)))))

(displaying
 (plot
  (map 'list
       (let ((c (circle)))
         (lambda (theta)
           (mark (point (sin theta) (cos theta)) c)))
       (grid-in (interval 0 (* 2 pi)) 25))))


;;; quantiles

(require :cl-random)

(let* ((y (generate-array 500 (lambda () (+ (random 1d0) (random 1d0)))))
       (qq ))
  (displaying (plot
               (list
                (diagonal-guide)
                (qy y :function (curry #'quantile (rv:r-normal (mean y) (variance y)))))
               :x-axis "equivalent normal"
               :y-axis "quantile")))

;; (displaying (500 400)
;;             (plot
;;              (let ((index 0))
;;                (map 'list
;;                     (lambda (center w1 w2)
;;                       (prog1 (q5 (xy index
;;                                      (vector (- center w2) (- center w1)
;;                                              center
;;                                              (+ center w1) (+ center w2)))
;;                                  :mark (annotation (format nil "~D" index)))
;;                         (incf index)))
;;                     (numseq 0 5 :length 6)
;;                     (numseq 0.4 0.7 :length 6)
;;                     (numseq 0.9 2 :length 6)))))

;;; function

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
