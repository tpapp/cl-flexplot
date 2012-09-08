;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)



;;; generic interface

(defgeneric domain (mapping)
  (:documentation "Return the domain of MAPPING."))

;; (defgeneric projection (mapping object)
;;   (:documentation "Return function that performs the projection."))

;;; coordinate projections

(defgeneric make-coordinate-projection (domain kind)
  (:documentation "FIXME"))

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

(defmethod make-coordinate-projection ((domain finite-interval) kind)
  (declare (ignore kind))
  (linear-projection domain))

(defmethod project ((projection linear-projection) coordinate)
  (let+ (((&structure-r/o linear-projection- offset coefficient) projection)
         ((&flex rel abs) coordinate))
    (flex (+ offset (* coefficient rel)) abs)))

(defmethod domain ((projection linear-projection))
  (linear-projection-domain projection))

;;; drawing area

(defstruct (drawing-area
            (:constructor make-drawing-area
                (frame x-projection y-projection)))
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
    (point (flex+ x (absolute p-x))
           (flex+ y (absolute p-y)))))



(defmacro define-expansion ((drawing-area instance-and-class
                             &key use-for-bounding-box)
                            &body body)
  "Define RENDER (and optionally EXTEND-BOUNDING-BOX) expansions for objects
which are composed of primitives that are returned by BODY."
  (let+ (((instance &optional (class instance)) (ensure-list instance-and-class)))
    `(progn
       (defmethod render ((,drawing-area drawing-area) (,instance ,class))
         (render ,drawing-area (progn ,@body)))
       ,@(splice-when use-for-bounding-box
           ;; FIXME should memoize?
           (with-unique-names (box)
             `(defmethod extend-bounding-box ((,box bounding-box) (,instance ,class))
                (extend-bounding-box ,box (progn ,@body))))))))
