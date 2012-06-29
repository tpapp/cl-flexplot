;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; FIXME documentation
;;; FIXME weed out unused functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (flex-coordinate
              (:constructor flex-coordinate (relative absolute)))
    (relative nil :type real :read-only t)
    (absolute nil :type real :read-only t)))

(define-constant +flex-coordinate-zero+ (flex-coordinate 0 0)
  :test #'equalp)

(defun flex-coordinate-apply (function &rest arguments)
  (flex-coordinate (apply function (mapcar #'flex-coordinate-relative arguments))
                   (apply function (mapcar #'flex-coordinate-absolute arguments))))

(defstruct (flex-point (:constructor flex-point (x y)))
  (x nil :type flex-coordinate)
  (y nil :type flex-coordinate))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (define-structure-let+ (flex-coordinate) relative absolute)

  (define-let+-expansion (&flex-point ((x-a x-r) (y-a y-r))
                             :value-var value :body-var body)
    (with-unique-names (x y)
      `(let+ (((&structure-r/o flex-point- (,x x) (,y y)) ,value)
              ((&flex-coordinate-r/o ,x-r ,x-a) ,x)
              ((&flex-coordinate-r/o ,y-r ,y-a) ,y))
         ,@body))))

(defmethod emit-value ((flex-point flex-point))
  (let+ (((&flex-point (x-a x-r) (y-a y-r)) flex-point))
    (latex
      (:cc x-r x-a y-r y-a))))

(defgeneric flex-plus (a b)
  (:method ((a flex-coordinate) (b flex-coordinate))
    (flex-coordinate-apply #'+ a b)))

(defun flex+ (object &rest objects)
  "Add flexible coordinate objects."
  (reduce #'flex-plus (cons object objects)))

(defgeneric flex-minus (a b)
  (:method ((a flex-coordinate) (b flex-coordinate))
    (flex-coordinate-apply #'- a b)))

(defun flex- (object &rest objects)
  "Subtract flexible coordinate objects."
  (reduce #'flex-minus (cons object objects)))

(defun flex-convex-combination (a b u)
  "A*U+B*(1-U)."
  (let+ (((&flex-coordinate a-rel a-abs) a)
         ((&flex-coordinate b-rel b-abs) b)
         ((&flet combine (a b) (+ (* a u) (* b (1c u))))))
    (flex-coordinate (combine a-rel b-rel) (combine a-abs b-abs))))
