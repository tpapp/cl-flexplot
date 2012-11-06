;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)



;;; generic interface

(defgeneric domain (mapping)
  (:documentation "Return the domain of MAPPING."))

;;; coordinate projections

(defgeneric make-coordinate-projection (domain kind)
  (:documentation "Make a coordinate projection from DOMAIN (to the unit
  frame, see FLEX-PROJECT and PROJECT).  KIND determines the details of the
  mapping (eg :LINEAR)"))

;;; linear coordinate projection

(defstruct linear-projection
  "Mapping the DOMAIN (intepreted as relative ecoordinates) to [0,1]."
  (offset nil :type real :read-only t)
  (coefficient nil :type real :read-only t)
  (domain nil :type finite-interval :read-only t))

(defun linear-projection (domain)
  (check-type domain finite-interval)
  (let+ (((&interval left right) domain)
         (coefficient (/ (- right left))))
    (make-linear-projection :offset (- (* coefficient left))
                            :coefficient coefficient
                            :domain domain)))

(defmethod make-coordinate-projection ((domain finite-interval)
                                       (kind (eql :linear)))
  (linear-projection domain))

(defmethod project ((projection linear-projection) coordinate)
  (let+ (((&structure-r/o linear-projection- offset coefficient) projection))
    (flex-transform-relative coordinate
                             (lambda (relative)
                               (+ offset (* coefficient relative))))))

(defmethod domain ((projection linear-projection))
  (linear-projection-domain projection))

;;; drawing area

(defstruct (drawing-area
            (:constructor make-drawing-area
                (frame x-projection y-projection)))
  "A drawing area is a frame equipped with two projections, for the two
coordinates."
  (frame nil :type frame :read-only t)
  (x-projection nil :read-only t)
  (y-projection nil :read-only t))

(defmethod domain ((da drawing-area))
  (let+ (((&structure-r/o drawing-area- x-projection y-projection) da))
    (list (domain x-projection) (domain y-projection))))

(defmethod project ((da drawing-area) point)
  (let+ (((&structure-r/o drawing-area- frame x-projection y-projection) da)
         ((&point x y) point))
    (project frame
             (point (project x-projection x)
                    (project y-projection y)))))

(defmacro with-projection ((mapping project) &body body)
  "Define a local function PROJECT that projects its argument using MAPPING."
  (check-type project symbol)
  (once-only (mapping)
    `(flet ((,project (point)
              (project ,mapping point)))
      ,@body)))

;;; origin drawing area, for implementing marks

(defstruct origin-drawing-area
  "Used for mapping absolute coordinates around some point in another drawing
area.  PROJECT takes a POINT containing real numbers and uses them to
translate around the original coordinates."
  (frame nil :type frame :read-only t)
  (x nil :type coordinate :read-only t)
  (y nil :type coordinate :read-only t))

(defun origin-drawing-area (drawing-area point)
  "Create an ORIGIN-DRAWING-AREA at POINT.  POINT is mapped using
DRAWING-AREA, and PROJECT called with the result translates as if point
contained absolute coordinates."
  (let+ (((&point x y) (project drawing-area point)))
    (make-origin-drawing-area :frame (drawing-area-frame drawing-area)
                              :x x :y y)))

(defmethod project ((da origin-drawing-area) (point point))
  (let+ (((&point p-x p-y) point)
         ((&structure-r/o origin-drawing-area- x y) da))
    (check-types (p-x p-y) real)
    (point (flex-project x x p-x)
           (flex-project y y p-y))))



(defmacro define-expansion ((drawing-area instance-and-class
                             &key use-for-bounding-box)
                            &body body)
  "Define RENDER (and optionally EXTEND-BOUNDING-BOX) expansions for objects
which are composed of primitives that are returned by BODY.

Note: objects can be specified in terms of other objects, in which case it is
not necessary to implement EXTEND-BOUNDING-BOX and RENDER separately, they are
just called with the given objects.  This macro should be used when the
semantics of the object elements as a whole should be preserved, otherwise
object creating functions could just return a sequence of the elements that
make up the object."
  (let+ (((instance &optional (class instance)) (ensure-list instance-and-class)))
    `(progn
       (defmethod render ((,drawing-area drawing-area) (,instance ,class))
         (render ,drawing-area (progn ,@body)))
       ,@(splice-when use-for-bounding-box
           ;; FIXME should memoize?
           (with-unique-names (box)
             `(defmethod extend-bounding-box ((,box bounding-box) (,instance ,class))
                (extend-bounding-box ,box (progn ,@body))))))))
