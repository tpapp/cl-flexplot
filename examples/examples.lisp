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
  (let+ (((center left bottom right top) (split5 +unit-frame+
                                                 (flex-spacer (flex 1/4 60))
                                                 (flex-spacer (flex 1/4 40))))
         (s-left (generate-scale :left (axis "left") (interval -2 3)))
         (s-bottom (generate-scale :bottom (axis "bottom") (interval -7 1)))
         (s-right (generate-scale :right (axis "right") (interval 0 9)))
         (s-top (generate-scale :top (axis "top") (interval -2 3))))
    (random-tint center)
    (render left s-left)
    (render right s-right)
    (render bottom s-bottom)
    (render top s-top)))
