;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

(defun unit-bounding-box ()
  (pgf-reset-bounding-box)
  (pgf-rectangle (flex-point (flex-coordinate 0 0)
                             (flex-coordinate 0 0))
                 (flex-point (flex-coordinate 1 0)
                             (flex-coordinate 1 0)))
  (pgf-use-as-bounding-box))

(defmacro with-output ((filespec &key (if-exists :supersede)) &body body)
  `(with-open-file (*output* ,filespec
                             :direction :output
                             :if-exists ,if-exists
                             :if-does-not-exist :create)
     ,@body))

(defmacro with-flexplot-output ((filespec &key (if-exists :supersede))
                                &body body)
  `(with-output (,filespec :if-exists ,if-exists)
     ,@body
     (unit-bounding-box)))

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
")

(defparameter *latex-width* "12cm")
(defparameter *latex-height* "8cm")

(defun write-latex-wrapper (wrapper-filespec flexplot-namestring
                            &key (header *latex-header*)
                                 (preamble *latex-preamble*)
                                 (body *latex-body*)
                                 (if-exists :supersede)
                                 (width *latex-width*)
                                 (height *latex-height*))
  (with-output (wrapper-filespec :if-exists if-exists)
    (let+ (((&flet w (string)
              (write-sequence string *output*))))
      (w header)
      (w preamble)
      (format *output* body width height flexplot-namestring))))

(defun compile-latex (pathname)
  "Given a pathname to a TEX file, call PDFLaTeX to compile the file (in the
same directory).  If the compilation was successful, return the pathname of
the PDF file, otherwise signal an error."
  (let+ (((&values status code)
          (external-program:run "pdflatex"
                                (list "-output-directory"
                                      (directory-namestring pathname)
                                      (namestring pathname)))))
    (assert (eq status :exited))
    (assert (zerop code) () "Error ~A when compiling file ~A." code
            (namestring pathname))
    (values (make-pathname :type "pdf" :defaults pathname)
            (make-pathname :type "log" :defaults pathname))))

(defun display-pdf (pathname &key (pdf-server "cl-plotpro") (raise? t))
  "Display PDF by calling a PDF viewer."
  ;; (copy-file pathname "/tmp/cl-plotpro.pdf")
  (external-program:start "xpdf"
                          (append (list "-remote" pdf-server
                                        (namestring pathname))
                                  (when raise? '("-raise")))))

(defparameter *default-temporary-file* nil
  "Default file used for rendering plots.  Use DEFAULT-TEMPORARY-FILE to
access it.")

(defun generate-temporary-file (&key (prefix "cl-plotpro-")
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
    (setf *default-temporary-file* (generate-temporary-file)))
  *default-temporary-file*)

(defmacro with-displayed-picture ((&key (path '(default-temporary-file))
                                        (wrapper-filespec
                                         "/tmp/cl-plotpro.tex")
                                        (raise? t))
                                  &body body)
  "Similar to WITH-PICTURE, but also displays the resulting PDF."
  (once-only (path)
    (with-unique-names (pdf-path)
      `(progn
         (with-flexplot-output (,path)
           ,@body)
         (write-latex-wrapper ,wrapper-filespec ,path)
         (let ((,pdf-path (compile-latex ,wrapper-filespec)))
           (display-pdf ,pdf-path :raise? ,raise?))))))

;; (defmacro displaying ((&key (path '(default-temporary-file))
;;                             (raise? t))
;;                       form)
;;   "Render a single form and display the resulting PDF."
;;   `(with-displayed-picture (,width ,height :path ,path :raise? ,raise?)
;;      (render ,form)))
