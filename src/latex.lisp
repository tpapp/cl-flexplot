;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; LaTeX output

;;; FIXME indentation, pretty printer
;;; FIXME simplification & optimization
;;; FIXME work on syntax
;;; FIXME documentation

(defvar *output* *standard-output*)

(defun latex-atom? (form)
  (and (atom form)
       (if (symbolp form)
           (keywordp form)
           t)))

(defun latex-cons? (form)
  (and (consp form) (keywordp (first form))))

(defun latex-sexp? (form)
  (or (latex-atom? form) (latex-cons? form)))

(defun process (processor form)
  (cond
    ((latex-atom? form) (process-latex-atom processor form))
    ((latex-cons? form) (process-latex-sexp processor form))
    ((consp form) (embed-code processor form))
    (t (embed-value processor form))))

(defun make-op-buffer () (make-array 10 :adjustable t :fill-pointer 0))

(defun push-op (op ops-buffer) (vector-push-extend op ops-buffer))

(defclass latex-compiler ()
  ((ops :accessor ops :initform (make-op-buffer))))

;;; operations

(defgeneric op->code (op)
  (:documentation "FIXME"))

(defmacro define-op (name slots &body op->code-body)
  (let ((struct-name (symbolicate '#:op- name))
        (op-var (gensym)))
    `(progn
       (defstruct (,struct-name (:constructor ,struct-name)) ,@slots)
       (defmethod op->code ((,op-var ,struct-name))
         (let+ (((&structure-r/o ,(symbolicate struct-name #\-)
                                 ,@(mapcar (compose #'first #'ensure-list)
                                           slots))
                 ,op-var))
           ,@op->code-body)))))

(define-op raw-string (string)
  `(princ ,string *output*))

(define-op newline ()
  '(terpri *output*))

(define-op embed-value (value)
  `(emit-value ,value))

(define-op embed-code (code)
  code)

(defmethod raw-string ((compiler latex-compiler) string)
  (push-op (op-raw-string :string string) (ops compiler)))

(defmethod newline ((compiler latex-compiler))
  (push-op (op-newline) (ops compiler)))

(defmethod embed-value ((compiler latex-compiler) form)
  (push-op (op-embed-value :value form) (ops compiler)))

(defmethod embed-code ((compiler latex-compiler) form)
  (push-op (op-embed-code :code form) (ops compiler)))

(defun parse-latex-cons (list)
  (let+ (((first &rest rest) list)
         ((&values optional rest)
          (if (eq (first rest) '&optional)
              (values (second rest) (cddr rest))
              (values nil rest))))
    (values first optional rest)))

(defun keyword-to-latex (keyword)
  (check-type keyword keyword)
  (format nil "\\~(~A~)" keyword))

(defun process-latex-sexp (processor form)
  (let+ (((&values first optional rest) (parse-latex-cons form)))
    (raw-string processor (keyword-to-latex first))
    (when optional
      (raw-string processor "[")
      (process processor optional)
      (raw-string processor "]"))
    (dolist (form rest)
      (raw-string processor "{")
      (process processor form)
      (raw-string processor "}"))))

(defun process-latex-atom (processor form)
  (if (eq form :/)
      (newline processor)
      (raw-string processor
                  (if (keywordp form)
                      (keyword-to-latex form)
                      (princ-to-string form)))))

(defun sexp->ops (body)
  (loop with compiler = (make-instance 'latex-compiler)
        for form in body do (process compiler form)
        finally (return (ops compiler))))

(defgeneric emit-value (value)
  (:documentation "Emit value to *OUTPUT* in a format understood by LaTeX.")
  (:method ((value real))
    (format *output* "~A" (float value 1.0)))
  (:method ((string string))
    (princ string *output*)))

(defun generate-code (ops)
  (map 'list #'op->code ops))

(defmacro latex (&body body)
  (let* ((ops (sexp->ops body)))
    `(progn ,@(generate-code ops))))
