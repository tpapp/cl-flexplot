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
    (with-drawing-area (drawing-area project)
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


(defun displaying (object)
  (with-displayed-picture ()
    (render +unit-frame+ object)))

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
