;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; DSL for coordinate interval splitting

(defstruct (flex-spacer
            (:constructor flex-spacer (&optional (weight +flex-unit+))))
  "A spacers will the available space, which is divided proportionally to
their weights."
  (weight nil :type coordinate))

(defun split-flex-interval (lower upper divisions)
  "Split the section between LOWER and UPPER into the specified divisions.  A
division is either a COORDINATE or a FLEX-SPACER: coordinates are used as is,
and the remaining section is divided between the spacers according to their
weight.  Return a vector of lists of 2 elements (the division endpoints)."
  (let+ ((spacer +flex-zero+)
         (non-spacer +flex-zero+))
    (map 'nil
         (lambda (division)
           (atypecase division
             (flex-spacer
              (setf spacer (flex+ spacer (flex-spacer-weight it))))
             (coordinate
              (setf non-spacer (flex+ non-spacer it)))))
         divisions)
    (let* ((remaining (flex- upper lower non-spacer))
           (spacer-scale (flex-apply
                          (lambda (spacer remaining)
                            (if (plusp spacer)
                                (/ remaining spacer)
                                (prog1 0 ; will never be used
                                  (assert (zerop remaining)))))
                          spacer remaining)))
      (map 'vector
           (lambda (division)
             (let* ((division
                      (atypecase division
                        (flex-spacer
                         (flex-apply #'* spacer-scale
                                     (flex-spacer-weight it)))
                        (coordinate it)))
                    (upper (flex+ lower division)))
               (aprog1 (list lower upper)
                 (setf lower upper))))
           divisions))))

(defun shrink-flex-interval (lower upper lower-padding upper-padding)
  "Shrink the section between LOWER and UPPER given the specified paddings.
Return the list of the two endpoints."
  (aref
   (split-flex-interval lower upper
                        (list lower-padding
                              (flex-spacer)
                              upper-padding))
   1))

;;; frames

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (frame
              (:constructor frame (left right bottom top)))
    "A frame corresponds to a rectangular area."
    (left nil :type coordinate)
    (right nil :type coordinate)
    (bottom nil :type coordinate)
    (top nil :type coordinate)))

(define-constant +unit-frame+ (frame 0 1 0 1)
  :test #'equalp
  :documentation "The unit frame, corresponds to the initial canvas.")

(define-structure-let+ (frame) left right bottom top)

(defun pgf-path-frame (frame)
  "PGF path for the rectangle of the frame."
  (let+ (((&frame-r/o left right bottom top) frame))
    (pgf-path-rectangle (point left bottom) (point right top))))

(defun split (frame h-divisions v-divisions)
  "Split FRAME into a grid defined by the division sequences (see
SPLIT-FLEX-INTERVAL for semantics), returning a matrix."
  (let+ (((&frame-r/o left right bottom top) frame))
    (aops:outer (lambda+ ((left right) (bottom top))
                  (frame left right bottom top))
                (split-flex-interval left right h-divisions)
                (split-flex-interval bottom top v-divisions))))

(defun split5 (frame left bottom &optional (right left) (top bottom))
  "Split FRAME into a center and the four adjacent regions.  Useful for plots
with scales.  Return (list center left bottom right top)."
  (let+ ((#2A((&ign left &ign)
              (bottom center top)
              (&ign right &ign)) (split frame
                                       (list left (flex-spacer) right)
                                       (list  bottom (flex-spacer) top))))
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
                           (list division (flex-spacer))
                           (list (flex-spacer) division)))
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

(defun replace-h (frame replacement-frame)
  "Return a new FRAME, taking horizontal coordinates from REPLACEMENT-FRAME
and the rest from FRAME."
  (if replacement-frame
      (let+ (((&frame-r/o &ign &ign bottom top) frame)
             ((&frame-r/o left right &ign &ign) replacement-frame))

        (frame left right bottom top))
      frame))

(defun replace-v (frame replacement-frame)
  "Return a new FRAME, taking vertical coordinates from REPLACEMENT-FRAME and
the rest from FRAME."
  (if replacement-frame
      (let+ (((&frame-r/o left right &ign &ign) frame)
             ((&frame-r/o &ign &ign bottom top) replacement-frame))
        (frame left right bottom top))
      frame))

(defun replace-with-orientation (orientation frame replacement-frame)
  "Replace the coordinates of frame specified by ORIENTATION.  See REPLACE-H
and REPLACE-V."
  (if (o-horizontal? orientation)
      (replace-h frame replacement-frame)
      (replace-v frame replacement-frame)))



(defgeneric project (mapping point)
  (:documentation "Project POINT using MAPPING.  See FLEX-PROJECT for
  explanation of the coordinate-wise semantics.")
  (:argument-precedence-order point mapping)
  (:method (mapping (point null))
    nil))

(defmethod project ((frame frame) (point point))
  (let+ (((&frame-r/o left right bottom top) frame)
         ((&point x y) point))
    (point (flex-project left right x) (flex-project bottom top y))))



(defgeneric render (target object)
  (:documentation "Render TARGET in FRAME, usually by emitting the appropriate
PGF commands using the PGF- functions.")
  (:method (target (sequence sequence))
    (map nil (curry #'render target) sequence)))

(defmacro with-clip-to-frame (frame &body body)
  "Clip to the rectangle of FRAME."
  `(pgf-scope
     (pgf-path-frame ,frame)
     (pgf-use-path :clip t)
     ,@body))
