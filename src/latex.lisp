;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

;;; Generating LaTeX output

;;; FIXME indentation, pretty printer
;;; FIXME simplification & optimization
;;; FIXME work on syntax
;;; FIXME documentation

(defvar *latex-output* *standard-output*
  "The stream where LaTeX code is sent.")

(defmacro with-latex-output ((filespec &key (if-exists :supersede))
                             &body body)
  "Emit LaTeX code to FILESPEC."
  `(with-open-file (*latex-output* ,filespec
                                   :direction :output
                                   :if-exists ,if-exists
                                   :if-does-not-exist :create)
     ,@body))


(defun latex-format (control-string &rest arguments)
  (apply #'format *latex-output* control-string arguments))



(defvar *latex-pretty* t
  "When not NIL, whitespace (newlines, indentation, ...) is added to LaTeX
code in order to aid debugging it directly.")

(defvar *latex-nesting* 0
  "Level of nesting within LaTeX commands.  When *LATEX-PRETTY*, the first
  level is separated by newlines.")

(defmacro with-latex-nesting (&body body)
  "Increase the level of nesting."
  `(let ((*latex-nesting* (1+ *latex-nesting*)))
     ,@body))

(defun latex-newline ()
  "Newline for the top level of nesting."
  (when *latex-pretty*
    (unless (plusp *latex-nesting*)
      (terpri *latex-output*))))

(defvar *latex-indent* 0
  "Indentation for commands on the top level of nesting.")

(defmacro with-latex-indent (&body body)
  "Increase indentation."
  `(let ((*latex-indent* (1+ *latex-indent*)))
     ,@body))

(defun latex-indent ()
  "Indent with spaces when applicable (*LATEX-PRETTY* and top level of
nesting."
  (when *latex-pretty*
    (unless (plusp *latex-nesting*)
      (loop repeat (* 2 *latex-indent*)
            do (latex-format " ")))))

;;;

(defgeneric latex-print (object)
  (:documentation "Print LaTeX representation of OBJECT.")
  (:method ((object null)))
  (:method ((number real))
    ;; note that with the standard libraries, PGF won't use more than 5 digits
    ;; anyway, which is plenty for our purposes.
    (etypecase number
      (integer (latex-format "~A" number))
      (real (latex-format "~,5F" number))))
  (:method ((string string))
    (latex-format string)))

;;; macro construction and semantics

(defun latex-sexp-form (form)
  "This function determines what happens to a SEXP in a LATEX-... macro.
Return a form that can be pasted into the expansion body."
  (if (listp form)
      form
      `(latex-print ,form)))

(defun latex-argument-form (argument &optional optional?)
  "Return a form for emitting an argument, wrapped in braces or brackets (when
OPTIONAL?)."
  (let+ (((&values left right) (if optional?
                                   (values "[" "]")
                                   (values "{" "}"))))
    `(progn
       (latex-print ,left)
       ,(latex-sexp-form argument)
       (latex-print ,right))))

;;; LaTeX macros

(defmacro latex-cat (&rest arguments)
  "Emit the concatenated arguments."
  `(progn
     ,@(loop for argument in arguments
             collect (latex-sexp-form argument))))

(defmacro latex-pt (length)
  "Emit LENGTH as a dimension specified in points."
  `(latex-cat ,length "pt"))

(defun latex-command-name (name)
  "Print the LaTeX command name, preceded by a backslash."
  (check-type name string)
  (latex-format "\\~A" name))

(defmacro latex-command-arguments (&rest arguments)
  "Emit arguments for a command."
  `(progn
     ,@(mapcar #'latex-argument-form arguments)))

(defmacro latex-command (command &rest arguments)
  "LaTeX command with arguments."
  `(with-latex-nesting
     (latex-indent)
     (latex-command-name ,command)
     (latex-command-arguments ,@arguments)
     (latex-newline)))

(defmacro latex-command* (command optional-argument &rest arguments)
  "LaTeX command with an optional argument and other arguments."
  `(with-latex-nesting
     (latex-indent)
     (latex-command-name ,command)
     ,(latex-argument-form optional-argument t)
     (latex-command-arguments ,@arguments)
     (latex-newline)))

(defun latex-comma-separated (&rest arguments)
  "Emit arguments formatted with LATEX-PRINT, separated by commas.  Omit
NILs."
  (let ((first t))
    (loop for argument in arguments
          do (when argument
               (if first
                   (setf first nil)
                   (latex-format #\,))
               (latex-print argument)))))

(defmacro latex-newcommand (command (n-arguments &optional (default nil default?))
                            &body body)
  "Emit a \\newcommand, defining command (a string, backslash will be prepended) with the given number of arguments (and a default when given).  BODY is executed for emitting the definition."
  `(with-latex-nesting
     (latex-indent)
     (latex-command "newcommand")
     (latex-command-arguments (latex-command ,command))
     ,(latex-argument-form n-arguments t)
     ,(when default?
        (latex-argument-form default t))
     (latex-command-arguments (progn ,@body))
     (latex-newline)))

(defmacro with-latex-environment (environment &body body)
  "Wrap BODY in the given LaTeX ENVIRONMENT (should be a string)."
  (check-type environment string)
  `(progn
     (latex-command "begin" ,environment)
     (multiple-value-prog1 (with-latex-indent ,@body)
       (latex-command "end" ,environment))))

(defun math (string)
  "STRING is interpreted as LaTeX code for (inline) math."
  (format nil "$~A$" string))
