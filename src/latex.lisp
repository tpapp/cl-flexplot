;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; Generating LaTeX output
;;;
;;; Low-level DSL for emitting LaTeX code to *LATEX-OUTPUT*.  The main entry
;;; point is the LATEX macro.

;;; FIXME indentation, pretty printer
;;; FIXME simplification & optimization
;;; FIXME work on syntax
;;; FIXME documentation

(defvar *latex-output* *standard-output*)

(defmacro with-latex-output ((filespec &key (if-exists :supersede)) &body body)
  `(with-open-file (*latex-output* ,filespec
                                   :direction :output
                                   :if-exists ,if-exists
                                   :if-does-not-exist :create)
     ,@body))

;;; compiler

(defstruct latex-compiler
  "Internal compiler for the LaTeX macro."
  (operations (make-array 10 :adjustable t :fill-pointer 0) :type vector))

(defun add-operation (latex-compiler operation)
  "Add operation to LATEX-COMPILER."
  (vector-push-extend operation
                      (latex-compiler-operations latex-compiler) 10))

(defgeneric operation->code (operation)
  (:documentation "Return a form that implements OPERATION."))

(defmacro define-op (name slots &body op->code-body)
  (let ((struct-name (symbolicate '#:op- name))
        (slot-names (mapcar (compose #'first #'ensure-list) slots))
        (op-var (gensym "OPERATION"))
        (compiler (gensym "PROCESSOR")))
    `(progn
       (defstruct (,struct-name (:constructor ,struct-name ,slot-names))
         ,@slots)
       (defmethod operation->code ((,op-var ,struct-name))
         (let+ (((&structure-r/o ,(symbolicate struct-name #\-)
                                 ,@(mapcar (compose #'first #'ensure-list)
                                           slots))
                 ,op-var))
           ,@op->code-body))
       (defun ,name (,compiler ,@slot-names)
         (add-operation ,compiler (,struct-name ,@slot-names))))))

(define-op raw-string (string)
  `(princ ,string *latex-output*))

(define-op newline ()
  '(terpri *latex-output*))

(define-op embed-value (value)
  `(latex-print ,value))

(define-op embed-code (code)
  code)

;;; auxiliary formatting functions

(defun latex-command (keyword)
  (check-type keyword keyword)
  (format nil "\\~(~A~)" keyword))

(defgeneric latex-literal (value)
  (:documentation "Return a string representing VALUE in LaTeX.  Useful for
  converting constants at compile time.")
  (:method ((number real))
    ;; note that with the standard libraries, PGF won't use more than 5 digits
    ;; anyway, which is plenty for our purposes as
    (etypecase number
      (integer (format nil "~A" number))
      (real (format nil "~,5F" number))))
  (:method ((string string))
    string))

(defgeneric latex-print (object)
  (:documentation "Print LaTeX representation of OBJECT to *LATEX-OUTPUT*.")
  (:method (object)
    (princ (latex-literal object) *latex-output*)))

;;; recognized forms

(defgeneric special-form-closure (first)
  (:method (first)
    nil))

(defmacro define-latex-special-op (name (processor &rest arguments)
                                   &body body)
  (with-unique-names (first)
    `(defmethod special-form-closure ((,first (eql ',name)))
       (lambda (,processor ,@arguments)
         ,@body))))

(define-latex-special-op :print (processor value)
  (embed-value processor value))

(defun maybe-process-atom (processor form)
  (when (atom form)
    (cond
      ((eq form :/) (newline processor))
      ((keywordp form) (raw-string processor (latex-command form)))
      ((symbolp form) (embed-value processor form))
      ((constantp form)
       (raw-string processor (latex-literal form)))
      (t (error "Don't know how to process atom ~A." form)))
    t))

(defun maybe-process-special (processor form)
  (awhen (and (consp form) (special-form-closure (first form)))
    (apply (special-form-closure (first form)) processor (rest form))
    t))

(defun maybe-process-latex-cons (processor form)
  (when (and (consp form) (keywordp (first form)))
    (raw-string processor (latex-command (first form)))
    (loop with optional? = nil
          for argument in (rest form)
          do (cond
               ((eq argument '&optional)
                (assert (not optional?) ()
                        "Two consecutive &optional forms.")
                (setf optional? t))
               (t
                (raw-string processor (if optional? "[" "{"))
                (process processor argument)
                (raw-string processor (if optional? "]" "}"))
                (when optional?
                  (setf optional? nil))))
          finally (assert (not optional?) ()
                          "No form following last &optional."))
    t))

(defun maybe-process-embedded-code (processor form)
  (when (and (consp form) (not (keywordp (first form))))
    (if (eq (first form) 'latex)        ; recognizing nesting
        (mapc (curry #'process processor) (rest form))
        (embed-code processor form))
    t))

(defun process (processor form)
  (cond
    ((maybe-process-special processor form))
    ((maybe-process-atom processor form))
    ((maybe-process-latex-cons processor form))
    ((maybe-process-embedded-code processor form))
    (t (error "Don't know how to process ~A." form))))

(defun sexp->operations (body)
  (loop with compiler = (make-latex-compiler)
        for form in body do (process compiler form)
        finally (return (latex-compiler-operations compiler))))

(defun generate-code (operations)
  (map 'list #'operation->code operations))

(defmacro latex (&body body)
  (let* ((operations (sexp->operations body)))
    `(progn ,@(generate-code operations))))

(defun math (string)
  "STRING is interpreted as LaTeX code for (inline) math."
  (format nil "$~A$" string))
