;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; coordinates
;;;
;;; FLEXs have a /relative/ and an /absolute/ component.  REALs are
;;; interpreted as a relative coordinate with a 0 absolute component.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (flex (:constructor flex (relative &key (pt 0) (em 0))))
    "A pair of relative (interpreted on [0,1]) and absolute coordinates."
    (relative nil :type real :read-only t)
    (pt nil :type real :read-only t)
    (em nil :type real :read-only t)))

(deftype coordinate ()
  "Coordinate type used in the CL-FLEXPLOT library."
  '(or real flex))

(declaim (inline ensure-flex pt em))

(defun ensure-flex (flex-or-real)
  "Return argument converted into a FLEX coordinate if necessary."
  (aetypecase flex-or-real
    (flex it)
    (real (flex it))))

(defun pt (pt)
  "Shorthand for creating FLEX coordinates with only a PT part."
  (flex 0 :pt pt))

(defun em (em)
  "Shorthand for creating FLEX coordinates with only a EM part."
  (flex 0 :em em))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-let+-expansion (&flex (relative pt em) :value-var value :body-var body)
    "LET+ clause for FLEX coordinates, also accepting reals."
    (with-unique-names (value-var)
      `(let* ((,value-var (ensure-flex ,value))
              (,relative (flex-relative ,value-var))
              (,pt (flex-pt ,value-var))
              (,em (flex-em ,value-var)))
         ,@body))))

(define-constant +flex-zero+ (flex 0) :test #'equalp)
(define-constant +flex-unit+ (flex 1 :pt 1 :em 1) :test #'equalp)

(defmethod make-load-form ((flex flex) &optional environment)
  (declare (ignore environment))
  (let+ (((&flex relative pt em) flex))
    `(flex ,relative :pt ,pt :em ,em)))

;;; operations on FLEX coordinates

(defmacro define-flex-reduction (name operator)
  `(defun ,name (flex &rest other)
     ,(format nil "Apply ~A to arguments for each component of the arguments ~
                   (converted to FLEX if necessary)." operator)
     (let+ (((&flex relative pt em) flex))
       (loop for o in other
             do (let+ (((&flex o-relative o-pt o-em) o))
                  (setf relative (,operator relative o-relative)
                        pt (,operator pt o-pt)
                        em (,operator em o-em))))
       (flex relative :pt pt :em em))))

(define-flex-reduction flex+ +)
(define-flex-reduction flex- -)
(define-flex-reduction flex-max max)

(defun flex-apply (function &rest arguments)
  "Apply FUNCTION to arguments elementwise, then return the resulting FLEX
object."
  (let+ (((&flet apply% (accessor)
            (apply function
                   (mapcar (compose accessor #'ensure-flex) arguments)))))
    (flex (apply% #'flex-relative)
          :pt (apply% #'flex-pt) :em (apply% #'flex-em))))

(defun flex-project (a b point)
  "Map POINT to between the coordinates A and B.  The semantics is defined as
follows: the relative coordinate of POINT is used to determine the convex
combination between A and B, to which the other coordinates are added."
  (let+ (((&flex a-rel a-pt a-em) a)
         ((&flex b-rel b-pt b-em) b)
         ((&flex p-rel p-pt p-em) point)
         ((&flet combine (a b) (+ (* a (1c p-rel)) (* b p-rel)))))
    (flex (combine a-rel b-rel)
          :pt (+ (combine a-pt b-pt) p-pt)
          :em (+ (combine a-em b-em) p-em))))

(defun flex-transform-relative (flex function)
  "Transform the relative coordinate using FUNCTION."
  (let+ (((&flex relative pt em) flex))
    (flex (funcall function relative) :pt pt :em em)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (point (:constructor point (x y)))
    (x nil :type coordinate)
    (y nil :type coordinate)))

(define-structure-let+ (point) x y)

(defmethod latex-print ((point point))
  (let+ (((&point-r/o (&flex x-r x-pt x-em) (&flex y-r y-pt y-em)) point))
    (latex
      (:cc x-r x-pt x-em y-r y-pt y-em))))

(define-constant +origin+ (point 0 0) :test #'equalp
  :documentation "Origin.")
