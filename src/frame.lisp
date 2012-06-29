;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; FIXME documentation

;;; coordinate interval splitting

(defstruct (flex-spacer
            (:constructor flex-spacer (&optional (coordinate
                                                  (flex-coordinate 1 1)))))
  (coordinate nil :type flex-coordinate))

(defun split-flex-interval (lower upper divisions)
  (let ((spacer +flex-coordinate-zero+)
        (non-spacer +flex-coordinate-zero+))
    (map 'nil
         (lambda (division)
           (aetypecase division
             (flex-spacer
              (setf spacer (flex+ spacer (flex-spacer-coordinate it))))
             (flex-coordinate
              (setf non-spacer (flex+ non-spacer it)))))
         divisions)
    (let* ((remaining (flex- upper lower non-spacer))
           (spacer-scale (flex-coordinate-apply
                          (lambda (spacer remaining)
                            (if (plusp spacer)
                                (/ remaining spacer)
                                (prog1 0 ; will never be used
                                  (assert (zerop remaining)))))
                          spacer remaining)))
      (map 'vector
           (lambda (division)
             (let* ((division
                      (aetypecase division
                        (flex-spacer
                         (flex-coordinate-apply #'* spacer-scale
                                                (flex-spacer-coordinate it)))
                        (flex-coordinate it)))
                    (upper (flex+ lower division)))
               (aprog1 (list lower upper)
                 (setf lower upper))))
           divisions))))

(defun shrink-flex-interval (lower upper lower-padding upper-padding)
  (aref
   (split-flex-interval lower upper
                        (list lower-padding
                              (flex-spacer (flex-coordinate 1 1))
                              upper-padding))
   1))

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (frame
              (:constructor frame (left right bottom top)))
    (left nil :type flex-coordinate)
    (right nil :type flex-coordinate)
    (bottom nil :type flex-coordinate)
    (top nil :type flex-coordinate)))

(define-constant +unit-frame+ (frame (flex-coordinate 0 0)
                                     (flex-coordinate 1 0)
                                     (flex-coordinate 0 0)
                                     (flex-coordinate 1 0))
  :test #'equalp)

(define-structure-let+ (frame) left right bottom top)

(defun split (frame h-divisions v-divisions)
    "Split FRAME into a grid defined by the division sequences (see
SPLIT-FLEX-INTERVAL for semantics), returning a matrix."
  (let+ (((&frame-r/o left right bottom top) frame))
    (outer* (split-flex-interval left right h-divisions)
            (split-flex-interval bottom top v-divisions)
            (lambda+ ((left right) (bottom top))
              (frame left right bottom top)))))

(defun split5 (frame left bottom &optional (right left) (top bottom))
  "Split FRAME into a center and the four adjacent regions.  Useful for plots
with scales.  Return (list center left bottom right top)."
  (let+ ((#2A((&ign left &ign)
              (bottom center top)
              (&ign right &ign)) (split frame
                                       (list left (spacer) right)
                                       (list  bottom (spacer) top))))
    (list center left bottom right top)))

(defun split-h (frame divisions)
  "Split FRAME horizontally, returning a vector.  See SPLIT."
  (let+ (((&frame-r/o left right bottom top) frame))
    (map 'vector (lambda+ ((left right))
                   (frame left right bottom top))
         (split-flex-interval left right divisions))))

(defun split-v (frame divisions)
  "Split FRAME vertically, returning a vector.  See SPLIT."
  (let+ (((&frame-r/o left right bottom top) frame))
    (map 'vector (lambda+ ((bottom top))
                   (frame left right bottom top))
         (split-flex-interval bottom top divisions))))

(defun shrink (frame left &optional (bottom left) (right left) (top bottom))
  "Shrink FRAME.  Arguments can be relative terms."
  (let+ (((&frame-r/o left% right% bottom% top%) frame)
         ((left right) (shrink-flex-interval left% right% left right))
         ((bottom top) (shrink-flex-interval bottom% top% bottom top)))
    (frame left right bottom top)))

(defun split2 (frame orientation division)
  "Split FRAME in two, according to ORIENTATION.  The first value returned
corresponds to the frame with the given orientation, the second value is the
other frame."
  (flet ((split% (horizontal? first?)
           (let+ ((division (if first?
                           (list division (spacer))
                           (list (spacer) division)))
                  (#(first second) (if horizontal?
                                       (split-h frame division)
                                       (split-v frame division))))
             (if first?
                 (values first second)
                 (values second first)))))
    (ecase orientation
      (:top (split% nil nil))
      (:bottom (split% nil t))
      (:left (split% t t))
      (:right (split% t nil)))))

(defstruct (uv (:constructor uv (u v)))
  "Structure for representing coordinates within a frame, normalized to
[0,1]²."
  (u nil :type (real 0 1))
  (v nil :type (real 0 1)))

(define-structure-let+ (uv) u v)

(defun frame-coordinates (frame uv)
  "Map unit coordinates to coordiantes withi FRAME."
  (let+ (((&frame-r/o left right bottom top) frame)
         ((&uv u v) uv))
    (flex-point (flex-convex-combination left right u)
                (flex-convex-combination bottom top v))))


;; (defmacro with-clip-to-frame (frame &body body)
;;   `(pdf:with-saved-state
;;      (%rectangle ,frame)
;;      (pdf:clip-path)
;;      (pdf:end-path-no-op)
;;      ,@body))

;; (defgeneric %random-tint (object)
;;   (:documentation "Tint object (usually a frame or a collection of frames with
;;   a random color.  Return the original object.  Useful for debugging.")
;;   (:method ((xy xy))
;;     (%rectangle xy)
;;     (%set-fill-color (random-color))
;;     (%fill)
;;     xy)
;;   (:method ((array array))
;;     (map nil #'%random-tint (flatten-array array))
;;     array)
;;   (:method ((sequence sequence))
;;     (map nil #'%random-tint sequence)
;;     sequence))
