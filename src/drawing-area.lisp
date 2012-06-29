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
  "Mapping the DOMAIN (intepreted as relativ ecoordinates) to [0,1]."
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
            (:constructor make-drawing-area (frame x-projection y-projection)))
  (frame nil :type frame :read-only t)
  (x-projection nil :read-only t)
  (y-projection nil :read-only t))

(defmacro with-drawing-area ((drawing-area project) &body body)
  (check-type project symbol)
  (once-only (drawing-area)
    (with-unique-names (frame x-projection y-projection)
      `(let+ (((&structure-r/o drawing-area-
                               (,frame frame)
                               (,x-projection x-projection)
                               (,y-projection y-projection)) ,drawing-area)
              ((&flet ,project (point)
                 (let+ (((&point x y) point))
                   (project ,frame
                            (point (project ,x-projection x)
                                   (project ,y-projection y)))))))
         ,@body))))
