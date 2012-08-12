;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; coordinates
;;;
;;; FLEXs have a /relative/ and an /absolute/ component.  REALs are
;;; interpreted as a relative coordinate with a 0 absolute component.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (flex (:constructor flex (relative absolute)))
    "A pair of relative (interpreted on [0,1]) and absolute coordinates."
    (relative nil :type real :read-only t)
    (absolute nil :type real :read-only t)))

(deftype coordinate ()
  "Coordinate type used in the CL-FLEXPLOT library."
  '(or real flex))

(defgeneric rel-part (coordinate)
  (:documentation "Return the absolute part of a coordinate.")
  (:method ((flex flex))
    (flex-relative flex))
  (:method ((real real))
    real))

(defgeneric abs-part (coordinate)
  (:documentation "Return the absolute part of a coordinate.")
  (:method ((flex flex))
    (flex-absolute flex))
  (:method ((real real))
    0))

(defun absolute (abs-part)
  "Convenience function for creating an absolute coordinate."
  (flex 0 abs-part))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-let+-expansion (&flex (rel abs) :value-var value :body-var body)
    "LET+ clause for FLEX coordinates, also accepting reals."
    `(let ((,rel (rel-part ,value))
           (,abs (abs-part ,value)))
       ,@body)))

(define-constant +flex-zero+ (flex 0 0) :test #'equalp)

(defun flex+ (flex &rest other)
  (let+ (((&flex rel abs) flex))
    (mapc (lambda+ ((&flex r a))
            (incf rel r)
            (incf abs a))
          other)
    (flex rel abs)))

(defun flex- (flex &rest other)
  (let+ (((&flex rel abs) flex))
    (mapc (lambda+ ((&flex r a))
            (decf rel r)
            (decf abs a))
          other)
    (flex rel abs)))

(defun flex-max (flex &rest other)
  (let+ (((&flex rel abs) flex))
    (mapc (lambda+ ((&flex r a))
            (maxf rel r)
            (maxf abs a))
          other)
    (flex rel abs)))

(defun flex-project (a b point)
  "Map POINT to between the coordinates A and B."
  (let+ (((&flex a-rel a-abs) a)
         ((&flex b-rel b-abs) b)
         ((&flex p-rel p-abs) point)
         ((&flet combine (a b) (+ (* a (1c p-rel)) (* b p-rel)))))
    (flex (combine a-rel b-rel)
          (+ (combine a-abs b-abs) p-abs))))

(defun flex-apply (function &rest arguments)
  (flex (apply function (mapcar #'rel-part arguments))
        (apply function (mapcar #'abs-part arguments))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (point (:constructor point (x y)))
    (x nil :type coordinate)
    (y nil :type coordinate)))

(define-structure-let+ (point) x y)

(defmethod latex-print ((point point))
  (let+ (((&point-r/o (&flex x-r x-a) (&flex y-r y-a)) point))
    (latex
      (:cc x-r x-a y-r y-a))))

(define-constant +origin+ (point 0 0) :test #'equalp
  :documentation "Origin.")
