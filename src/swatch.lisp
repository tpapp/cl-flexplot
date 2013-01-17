;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)

(defgeneric write-swatch (command style)
  (:documentation "Write a command that produces swatch demonstrating that style."))

(defparameter *swatch-length* (em 2))

(defmethod write-swatch ((command string) (style stroke-style))
  (latex-newcommand command (1 "1em")
    (latex-command "flexpicture"
                   (latex-format "#1")
                   (latex-format "2ex")
                   (progn
                     (pgf-set-stroke-style style)
                     (pgf-lines (vector +origin+
                                        (point *swatch-length* 0)))))))

(defun write-swatches (filespec command-style-alist &key (if-exists :supersede))
  "Write an alist of COMMAN-STYLE pairs to FILESPEC."
  (with-latex-output (filespec :if-exists if-exists)
    (loop for (command . style) in command-style-alist
          do (write-swatch command style))))
