;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; All commands that emit PGF (LaTeX) code start with PGF-.  The preferred
;;; interface or defining them is DEFINE-PGF-COMMAND.  Some of the definitions
;;; are in this file, but if other functions build on these they should start
;;; with pgf.

(defmacro define-pgf-command (name (&rest arguments) &body body)
  "This function defines a PGF primitive command.

NAME is (lisp-name latex-name), PGF- and pgf are prepended to each,
respectively.  LISP-NAME and (LISP-NAME) generate the function name from
LISP-NAME by stripping dashes and converting to lowercase.

ARGUMENTS are arguments of the lisp function.

If BODY starts with a keyword (after the docstring), it is used to generate
the body of the function.  The following are recognized:

  :ARGUMENTS just emits the arguments.

  :ARGUMENTS* is the same as :ARGUMENTS, "
  (let+ (((function-name
           &optional (command-name
                      (remove #\-
                              (format nil "~(~A~)" function-name))))
          (ensure-list name))
         ((&values remaining-forms declarations doc-string)
          (if (and (stringp (car body)) (not (cdr body)))
              (values nil nil (car body))
              (parse-body body :documentation t)))
         (body (append declarations remaining-forms))
         ((&flet default-body (arguments &optional optional?)
            `((,(if optional?
                    'latex-command*
                    'latex-command)
               ,(format nil "pgf~A" command-name) ,@arguments)))))
    `(defun ,(symbolicate '#:pgf- function-name) ,arguments
       ,@(splice-when doc-string doc-string)
       ,@(ematch body
           ((cons :arguments rest) (default-body rest))
           ((cons :arguments* rest) (default-body rest t))
           ((cons :append-arguments rest) (default-body (append arguments rest)))
           ((list :color) (with-gensyms (red green blue)
                            `((let+ (((&rgb ,red ,green ,blue)
                                      (as-rgb ,(car arguments))))
                                ,@(default-body (list* red green blue
                                                       (cdr arguments)))))))
           (nil (default-body arguments))
           (_ body)))))

(defun pgf-text-align-string (align)
  "Return a string for valid text alignment options."
  ;; Note: PGF's align orientation is the exact opposite of what this library
  ;; uses.
  (ecase align
    (:left "right")
    (:right "left")
    (:base "base")
    (:base-left "base,right")
    (:base-right "base,left")
    (:top "bottom")
    (:top-left "bottom,right")
    (:top-right "bottom,left")
    (:bottom "top")
    (:bottom-left "top,right")
    (:bottom-right "top,left")))

(define-pgf-command text (position text &key align rotate)
  :arguments*
  (latex-cat "at=" position
             (when rotate
               (latex-cat ",rotate=" rotate))
             (when align
               (let ((align-string (pgf-text-align-string align)))
                 (latex-cat "," align-string))))
  text)

(define-pgf-command path-move-to (position))

(define-pgf-command path-line-to (position))

(define-pgf-command (path-rectangle "pathrectanglecorners") (corner1 corner2))

(define-pgf-command path-circle (center radius)
  :arguments center (latex-pt radius))

(define-pgf-command (set-color "sys@color@rgb") (color) :color)

(define-pgf-command (set-fill-color "sys@color@rgb@fill") (color) :color)

(define-pgf-command (set-stroke-color "sys@color@rgb@stroke") (color) :color)

(define-pgf-command set-line-width (width) :arguments (latex-pt width))

(define-pgf-command set-dash (dimensions &optional (phase 0))
  (assert (divides? (length dimensions) 2) ()
          "Dash dimensions need to have an even length.")
  (latex-command "pgfsetdash"
                 (loop for dimension in dimensions
                       do (latex-cat "{" dimension "}"))
                 phase))

(define-pgf-command use-path (&key fill stroke clip)
  :arguments (latex-comma-separated
              (when fill "fill")
              (when stroke "stroke")
              (when clip "clip")))

(define-pgf-command stroke () (pgf-use-path :stroke t))

(define-pgf-command fill () (pgf-use-path :fill t))

(define-pgf-command reset-bounding-box ())

(define-pgf-command use-as-bounding-box ()
  (latex-command "pgfusepath" "use as bounding box"))

(defmacro pgf-scope (&body body)
  `(with-latex-environment "pgfscope" ,@body))

;;; convenience functions

(define-pgf-command lines (points)
  "Stroke consecutive points.  NIL breaks the line."
  (let+ (open?
         ((&flet ensure-closed ()
            (when open?
              (pgf-stroke)
              (setf open? nil)))))
    (map nil (lambda (point)
               (if point
                   (if open?
                       (pgf-path-line-to point)
                       (progn
                         (pgf-path-move-to point)
                         (setf open? t)))
                   (ensure-closed)))
         points)
    (ensure-closed))
  (values))
