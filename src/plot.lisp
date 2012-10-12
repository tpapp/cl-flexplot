;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)



(defparameter *outer-gap* (pt 3)
  "Gap between axes and drawing area clip box.")

(defparameter *inner-gap* (pt 8)
  "Gap between drawing area and its clip box.")



;;; simple plot

(defclass plot ()
  ((bounding-box :reader bounding-box :initarg :bounding-box)
   (objects :initarg :objects)
   (x-axis :accessor x-axis :initarg :x-axis)
   (y-axis :accessor y-axis :initarg :y-axis)
   (x-other-axis :accessor x-other-axis :initarg :x-other-axis)
   (y-other-axis :accessor y-other-axis :initarg :y-other-axis)))

(defun plot (objects
             &key (x-axis (axis (math "x")))
                  (y-axis (axis (math "y")))
                  (x-other-axis (transformation :ticks :marks-only))
                  (y-other-axis (transformation :ticks :marks-only)))
  (let ((bounding-box (bounding-box objects)))
    (check-type bounding-box nonempty-bounding-box)
    (make-instance
     'plot
     :bounding-box bounding-box
     :objects objects
     :x-axis (ensure-axis x-axis)
     :y-axis (ensure-axis y-axis)
     :x-other-axis x-other-axis
     :y-other-axis y-other-axis)))

(defmethod render (frame (plot plot))
  (let+ (((&slots-r/o objects bounding-box x-axis y-axis
                      x-other-axis y-other-axis) plot)
         ((&bounding-box bb-x bb-y) bounding-box)
         ((center . sides)
          (split5 frame
                  (margin :left y-axis) (margin :bottom x-axis)
                  (margin :right y-other-axis) (margin :top x-other-axis)))
         (clip-box (shrink center *outer-gap*))
         (graph-box (shrink clip-box *inner-gap*))
         (bottom-scale (generate-scale :bottom x-axis bb-x))
         (left-scale (generate-scale :left y-axis bb-y)))
    (let* ((x-projection (projection bottom-scale))
           (y-projection (projection left-scale))
           (da (make-drawing-area graph-box x-projection y-projection)))
      (with-clip-to-frame clip-box
        (render da objects))
      (scale-frame center)
      (sides4 sides
              graph-box
              (cons (title y-axis) left-scale)
              (cons (title x-axis) bottom-scale)
              (cons (title y-other-axis)
                    (transform-scale left-scale y-other-axis))
              (cons (title x-other-axis)
                    (transform-scale bottom-scale x-other-axis))))))



;;; panel plot

;; (defun matrix-intervals (matrix &key uniform-row? uniform-col?)
;;   "Return the combined row and column bounding intervals as two values which
;; are vectors."
;;   (let+ (((nrow ncol) (array-dimensions matrix))
;;          (row-intervals (make-array nrow :initial-element nil))
;;          (col-intervals (make-array ncol :initial-element nil))
;;          ((&flet maybe-uniform (intervals uniform?)
;;             (if uniform?
;;                 (fill intervals (interval-hull intervals))
;;                 intervals))))
;;     (dotimes (row-index nrow)
;;       (dotimes (col-index ncol)
;;         (let+ (((&bounding-box-r/o x y)
;;                 (bounding-box (aref matrix row-index col-index))))
;;           (extendf-interval (aref row-intervals row-index) y)
;;           (extendf-interval (aref col-intervals col-index) x))))
;;     (values (maybe-uniform row-intervals uniform-row?)
;;             (maybe-uniform col-intervals uniform-col?))))

;; (defun alternating-scales (orientation boxes axes intervals transformation
;;                            scale-boxes1 scale-boxes2)
;;   "Alternating scales for panel plots."
;;   (let ((horizontal? (o-horizontal? orientation))
;;         (opposite (o-opposite orientation))
;;         (switch? nil))
;;     (map 'vector
;;          (lambda (box axis interval scale-box1 scale-box2)
;;            (let+ ((primary (generate-scale horizontal? box axis interval))
;;                   (secondary (transform-scale primary transformation)))
;;              (prog1 primary
;;                (when switch?
;;                  (rotatef primary secondary))
;;                (render-in-side orientation scale-box1 primary)
;;                (render-in-side opposite scale-box2 secondary)
;;                (setf switch? (not switch?)))))
;;          boxes axes intervals scale-boxes1 scale-boxes2)))

;; (defun panel-plot (objects-matrix
;;                    &key x-title y-title
;;                         (x-axes (axis "x"))
;;                         (y-axes (axis "y"))
;;                         (x-scale-transformation (transformation
;;                                                  :ticks :marks-only))
;;                         (y-scale-transformation nil)
;;                         panel-titles uniform-x? uniform-y?)
;;   ""
;;   (let+ (((&values y-intervals x-intervals)
;;           (matrix-intervals objects-matrix :key #'bounding-box
;;                                            :uniform-row? uniform-y?
;;                                            :uniform-col? uniform-x?))
;;          ((nrow ncol) (array-dimensions objects-matrix))
;;          (title-matrix
;;           (map1 #'title (ensure-array panel-titles
;;                                       (array-dimensions objects-matrix))))
;;          (x-div (loop for index below ncol
;;                       when (plusp index)
;;                         collect 3
;;                       collect (spacer)))
;;          (y-div (loop with spacer = (spacer)
;;                       with margin = (max-margin :top title-matrix)
;;                       for index below nrow
;;                       when (plusp index)
;;                         collect 3
;;                       collect spacer
;;                       collect margin))
;;          (x-skip (ivec* 0 nil 2))
;;          (y-skip (ivec* 0 nil 3))
;;          (y-skip-title (ivec* 1 nil 3))
;;          (x-title (title x-title))
;;          (y-title (title y-title))
;;          (x-axes (ensure-array x-axes ncol))
;;          (y-axes (reverse (ensure-array y-axes nrow))))
;;     (lambda (&optional (box *box*))
;;       (declare (optimize debug))
;;       (let+ ((x-margin (+ (margin :left y-title)
;;                           (max-margin :left y-axes)))
;;              (y-margin (+ (margin :bottom x-title)
;;                           (max-margin :bottom x-axes)))
;;              ((center . sides) (split5 box x-margin y-margin))
;;              (all-boxes (sub (split center x-div y-div) x-skip t))
;;              (title-boxes (visual-layout (sub all-boxes t y-skip-title)))
;;              (panels (visual-layout (sub all-boxes t y-skip)))
;;              (outer (map1 (lambda (f) (shrink f *outer-gap*)) panels))
;;              (inner (map1 (lambda (f) (shrink f *inner-gap*)) outer))
;;              ((left bottom right top) (sides4 sides
;;                                               (list y-title) (list x-title)
;;                                               (list y-title) (list x-title)))
;;              ((&flet side% (orientation box axes)
;;                 (map 'vector (lambda (box axis)
;;                                (side orientation box (title axis)))
;;                      (if (o-horizontal? orientation)
;;                          (sub (split-h box x-div) x-skip)
;;                          (sub (split-v box y-div) y-skip))
;;                      axes)))
;;              (left% (side% :left left y-axes))
;;              (right% (side% :right right y-axes))
;;              (bottom% (side% :bottom bottom x-axes))
;;              (top% (side% :top top x-axes))
;;              (x-scales (alternating-scales :bottom (sub inner 0 t) x-axes
;;                                            x-intervals x-scale-transformation
;;                                            bottom% top%))
;;              (y-scales (alternating-scales :left (sub inner t 0) y-axes
;;                                            y-intervals y-scale-transformation
;;                                            left% right%)))
;;         (iter
;;           (for x-scale :in-vector x-scales :with-index col-index)
;;           (iter
;;             (for y-scale :in-vector y-scales :with-index row-index)
;;             (awhen (aref objects-matrix row-index col-index)
;;               (scale-box (aref panels row-index col-index))
;;               (with-clip-to-box (aref outer row-index col-index)
;;                 (with-projection (xy (projection-of x-scale)
;;                                      (projection-of y-scale))
;;                   (render it)))
;;               (awhen (aref title-matrix row-index col-index)
;;                 (when (plusp (margin :top it))
;;                   (let ((box (aref title-boxes row-index col-index)))
;;                     (scale-box box)
;;                     (render-in-side :top box it)))))))))))
