;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

(defun pgf-bounding-box ()
  "Unit bounding box for plots."
  (pgf-reset-bounding-box)
  (pgf-path-frame +unit-frame+)
  (pgf-use-as-bounding-box))

(defmacro with-flexplot-output ((filespec &key (if-exists :supersede))
                                &body body)
  "Write output to FILESPEC."
  `(with-latex-output (,filespec :if-exists ,if-exists)
     ,@body
     (pgf-bounding-box)))

(defparameter *latex-header*
  "\\documentclass[a4paper,12pt]{article}
\\usepackage{flexplot}
\\usepackage[active,tightpage]{preview}
\\setlength{\\PreviewBorder}{0pt}
"
  "String written to a LaTeX wrapper file at the beginning.  You should not
  need to change this under normal circumstances, see *LATEX-PREAMBLE*.")

(defparameter *latex-preamble*
  ""
  "String written to a LaTeX wrapper file before the body.  Use this to load
  extra packages, code, etc.")

(defparameter *latex-body*
  "\\begin{document}
\\begin{preview}
\\includeflexplot{~A}{~A}{~A}
\\end{preview}
\\end{document}
"
  "Format control string for including a flexplot.  Will take width, height
and filename.")

(defparameter *latex-width* "12cm"
  "Default height for rendering displayed previews.")
(defparameter *latex-height* "8cm"
  "Default width for rendering displayed previews.")

(defun write-latex-wrapper (wrapper-filespec flexplot-namestring
                            &key (header *latex-header*)
                                 (preamble *latex-preamble*)
                                 (body *latex-body*)
                                 (if-exists :supersede)
                                 (width *latex-width*)
                                 (height *latex-height*))
  "Write a LaTeX wrapper file to WRAPPER-FILESPEC that can be used to preview
a flexplot, which is in the file designated by FLEXPLOT-NAMESTRING."
  (with-output-to-file (stream wrapper-filespec :if-exists if-exists)
    (let+ (((&flet w (string)
              (write-sequence string stream))))
      (w header)
      (w preamble)
      (format stream body width height flexplot-namestring))))

(define-condition latex-error (error)
  ((code :initarg :code)
   (log :initarg :log))
  (:documentation "Error during latex compilation."))

(defmethod print-object ((latex-error latex-error) stream)
  (print-unreadable-object (latex-error stream :type t)
    (let+ (((&slots-r/o code log) latex-error))
      (format stream "status code ~A~2%" code)
      (write-sequence log stream))))

(defun compile-latex (pathname)
  "Given a pathname to a TEX file, call PDFLaTeX to compile the file (in the
same directory).  If the compilation was successful, return the pathname of
the PDF file, otherwise signal an error."
  (let+ ((log-pathname (make-pathname :type "log" :defaults pathname))
         ((&values status code)
          (external-program:run "pdflatex"
                                (list "-output-directory"
                                      (directory-namestring pathname)
                                      (namestring pathname)))))
    (assert (eq status :exited))
    (assert (zerop code) ()
            'latex-error
            :code code
            :log (with-open-file (stream log-pathname :direction :input)
                   (let* ((length (file-length stream))
                          (data (make-string length)))
                     (read-sequence data stream)
                     data)))
    (make-pathname :type "pdf" :defaults pathname)))

(defun display-pdf (pathname &key (pdf-server "cl-flexplot") (raise? t))
  "Display PDF by calling a PDF viewer."
  ;; NOTE currently we use XPDF
  (external-program:start "xpdf"
                          (append (list "-remote" pdf-server
                                        (namestring pathname))
                                  (when raise? '("-raise")))))

(defparameter *default-temporary-file* nil
  "Default file used for rendering plots.  If unspecified (NIL), the filename
is generated randomly, otherwise this value is used.  Functions should use the
function DEFAULT-TEMPORARY-FILE to get a temporary filename.")

(defun generate-temporary-file (&key (prefix "cl-flexplot-")
                                     (directory "/tmp/")
                                     type
                                     (characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                                     (length 10)
                                     (maximum-tries 1000))
  "Return a randomly pathname in DIRECTORY, with given PREFIX and TYPE
enclosing LENGTH randomly drawn CHARACTERS, ensuring that the file does not
exist.  If it encounters existing files for MAXIMUM-TRIES, signal an error."
  (let+ ((directory (fad:pathname-as-directory directory))
         (n (length characters))
         (name (make-array length
                           :element-type (array-element-type characters)))
         ((&flet random-name ()
            (concatenate 'string prefix
                         (map-into name (lambda ()
                                          (aref characters (random n)))))))
         ((&flet path ()
            (make-pathname :name (random-name) :type type
                           :defaults directory))))
    (loop repeat maximum-tries
          for path = (path)
          unless (fad:file-exists-p path) return path
            finally (error "Reached maximum number of tries."))))

(defun default-temporary-file (&optional reset)
  "Return the default temporary file.  When RESET, create a new filename."
  (when (or (not *default-temporary-file*) reset)
    (setf *default-temporary-file*
          (generate-temporary-file :type "flexplot")))
  *default-temporary-file*)

(defparameter *default-wrapper* #P"/tmp/cl-flexplot.tex"
  "Default wrapper filespec for displaying.")

(defmacro with-displayed-picture ((&key (path '(default-temporary-file))
                                        (wrapper '*default-wrapper*)
                                        (raise? t))
                                  &body body)
  "Redirect the LaTeX/PGF output of the body to PATH (default: a temporary
file), then compile and display the resulting PDF."
  (once-only (path wrapper)
    (with-unique-names (pdf-path)
      `(progn
         (with-flexplot-output (,path)
           ,@body)
         (write-latex-wrapper ,wrapper ,path)
         (let ((,pdf-path (compile-latex ,wrapper)))
           (display-pdf ,pdf-path :raise? ,raise?))))))

(defmethod render ((pathname pathname) object)
  (with-flexplot-output (pathname)
    (render +unit-frame+ object)))

(defun displaying (object &key (path (default-temporary-file))
                               (wrapper *default-wrapper*)
                               (raise? t))
  "Render OBJECT and display the resulting PDF."
  (with-displayed-picture (:path path :wrapper wrapper :raise? raise?)
    (render +unit-frame+ object)))
