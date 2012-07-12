;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-flexplot)



;;; Generating LaTeX output
;;;
;;; Low-level DSL for emitting LaTeX code to *OUTPUT*.  The main entry point
;;; is the LaTeX macro.

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

(defgeneric special-form-closure (first)
  (:method (first)
    nil))

(defun special-form? (form)
  (and (consp form) (special-form-closure (first form))))

(defun process-special-form (processor form)
  (apply (special-form-closure (first form)) processor (rest form)))

(defun process (processor form)
  (cond
    ((special-form? form) (process-special-form processor form))
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

(defstruct (pt (:constructor pt (dimension)))
  "LaTeX point."
  (dimension nil :type real :read-only t))

(defmacro latex (&body body)
  (let* ((ops (sexp->ops body)))
    `(progn ,@(generate-code ops))))

(defun latex-print-number (stream number)
  (etypecase number
    (integer (format stream "~A" number))
    (real (format stream "~,5F" number))))

(defgeneric emit-value (value)
  (:documentation "Emit value to *OUTPUT* in a format understood by LaTeX.")
  (:method ((value real))
    (latex-print-number *output* value))
  (:method ((pt pt))
    (emit-value (pt-dimension pt))
    (princ "pt" *output*))
  (:method ((string string))
    (princ string *output*))
  (:method ((list list))
    (dolist (item list)
      (latex "{" item "}"))))

(defun generate-code (ops)
  (map 'list #'op->code ops))

;;; special forms

(defmacro define-latex-special-op (name (processor &rest arguments)
                                   &body body)
  (with-unique-names (first)
    `(defmethod special-form-closure ((,first (eql ',name)))
       (lambda (,processor ,@arguments)
         ,@body))))

(define-latex-special-op :print (processor value)
  (embed-value processor value))

(defun math (string)
  (format nil "$~A$" string))



;;; FIXME documentation
;;; FIXME weed out unused functions

;;; coordinates
;;;
;;; FLEXs have a /relative/ and an /absolute/ component.  REALs are
;;; interpreted as a relative coordinate with a 0 absolute component.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (flex (:constructor flex (relative absolute)))
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


(defstruct (point (:constructor point (x y)))
  (x nil :type coordinate)
  (y nil :type coordinate))

(define-structure-let+ (point) x y)

(defmethod emit-value ((point point))
  (let+ (((&point-r/o (&flex x-r x-a) (&flex y-r y-a)) point))
    (latex
      (:cc x-r x-a y-r y-a))))



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

(defun pgf-text (position text &key align rotate)
  (latex
    (:pgftext &optional
              (latex "at=" position
                (when rotate
                  (latex ",rotate=" rotate))
                (when align
                  (let ((align-string (pgf-text-align-string align)))
                    (latex "," align-string))))
              text)
    :/))

(defun pgf-path-move-to (position)
  (latex (:pgfpathmoveto position) :/))

(defun pgf-path-line-to (position)
  (latex (:pgfpathlineto position) :/))

(defun pgf-rectangle (corner1 corner2)
  (latex (:pgfpathrectanglecorners corner1 corner2) :/))

(defun pgf-stroke ()
  (latex (:pgfusepath "stroke") :/))

(defun pgf-set-color (color)
  (let+ (((&rgb red green blue) (as-rgb color)))
    (latex (:pgfsys@color@rgb red green blue))))

(defun pgf-set-fill-color (color)
  (let+ (((&rgb red green blue) (as-rgb color)))
    (latex (:pgfsys@color@rgb@fill red green blue))))

(defun pgf-set-stroke-color (color)
  (let+ (((&rgb red green blue) (as-rgb color)))
    (latex (:pgfsys@color@rgb@stroke red green blue))))

(defun pgf-set-line-width (width)
  (latex (:pgfsetlinewidth (:print (pt width)))))

(defun pgf-set-dash (dimensions &optional (phase 0))
  (assert (divides? (length dimensions) 2) ()
          "Dash dimensions need to have an even length.")
  (latex (:pgfsetdash (:print (mapcar #'pt dimensions)) phase) :/))

(defun pgf-fill ()
  (latex (:pgfusepath "fill") :/))

(defun print-comma-separated (stream &rest strings)
  (let ((first t))
    (loop for string in strings
          do (when string
               (if first
                   (setf first nil)
                   (princ #\, stream))
               (princ string stream)))))

(defun pgf-use-path (&key fill stroke clip)
  (latex (:pgfusepath (print-comma-separated *output*
                                             (when fill "fill")
                                             (when stroke "stroke")
                                             (when clip "clip")))))

(defun pgf-reset-bounding-box ()
  (latex (:pgfresetboundingbox) :/))

(defun pgf-use-as-bounding-box ()
  (latex (:pgfusepath "use as bounding box") :/))


;;; convenience functions

(defun pgf-lines (points)
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



;;; FIXME documentation

;;; coordinate interval splitting

(defstruct (flex-spacer
            (:constructor flex-spacer (&optional (coordinate
                                                  (flex 1 1)))))
  (coordinate nil :type coordinate))

(defun split-flex-interval (lower upper divisions)
  (let ((spacer +flex-zero+)
        (non-spacer +flex-zero+))
    (map 'nil
         (lambda (division)
           (atypecase division
             (flex-spacer
              (setf spacer (flex+ spacer (flex-spacer-coordinate it))))
             (t
              (setf non-spacer (flex+ non-spacer it)))))
         divisions)
    (let* ((remaining (flex- upper lower non-spacer))
           (spacer-scale (flex-apply
                          (lambda (spacer remaining)
                            (if (plusp spacer)
                                (/ remaining spacer)
                                (prog1 0 ; will never be used
                                  (assert (zerop remaining)))))
                          spacer remaining)))
      (map 'vector
           (lambda (division)
             (let* ((division
                      (atypecase division
                        (flex-spacer
                         (flex-apply #'* spacer-scale
                                     (flex-spacer-coordinate it)))
                        (t it)))
                    (upper (flex+ lower division)))
               (aprog1 (list lower upper)
                 (setf lower upper))))
           divisions))))

(defun shrink-flex-interval (lower upper lower-padding upper-padding)
  (aref
   (split-flex-interval lower upper
                        (list lower-padding
                              (flex-spacer)
                              upper-padding))
   1))

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (frame
              (:constructor frame (left right bottom top)))
    (left nil :type coordinate)
    (right nil :type coordinate)
    (bottom nil :type coordinate)
    (top nil :type coordinate)))

(define-constant +unit-frame+ (frame 0 1 0 1)
  :test #'equalp)

(define-structure-let+ (frame) left right bottom top)

(defun pgf-frame-rectangle (frame)
  (let+ (((&frame-r/o left right bottom top) frame))
    (pgf-rectangle (point left bottom) (point right top))))

(defun split (frame h-divisions v-divisions)
    "Split FRAME into a grid defined by the division sequences (see
SPLIT-FLEX-INTERVAL for semantics), returning a matrix."
  (let+ (((&frame-r/o left right bottom top) frame))
    (outer* (split-flex-interval left right h-divisions)
            (split-flex-interval bottom top v-divisions)
            (lambda+ ((left right) (bottom top))
              (frame left right bottom top)))))

(defun split5 (frame left bottom &optional (right left) (top bottom))
  "Split FRAME into a center and the four adjacent regions.  Useful for plots
with scales.  Return (list center left bottom right top)."
  (let+ ((#2A((&ign left &ign)
              (bottom center top)
              (&ign right &ign)) (split frame
                                       (list left (flex-spacer) right)
                                       (list  bottom (flex-spacer) top))))
    (list center left bottom right top)))

(defun split-h (frame divisions)
  "Split FRAME horizontally, returning a vector.  See SPLIT."
  (let+ (((&frame-r/o left right bottom top) frame))
    (map 'vector (lambda+ ((left right))
                   (frame left right bottom top))
         (split-flex-interval left right divisions))))

(defun split-v (frame divisions)
  "Split FRAME vertically, returning a vector.  See SPLIT."
  (let+ (((&frame-r/o left right bottom top) frame))
    (map 'vector (lambda+ ((bottom top))
                   (frame left right bottom top))
         (split-flex-interval bottom top divisions))))

(defun shrink (frame left &optional (bottom left) (right left) (top bottom))
  "Shrink FRAME.  Arguments can be relative terms."
  (let+ (((&frame-r/o left% right% bottom% top%) frame)
         ((left right) (shrink-flex-interval left% right% left right))
         ((bottom top) (shrink-flex-interval bottom% top% bottom top)))
    (frame left right bottom top)))

(defun split2 (frame orientation division)
  "Split FRAME in two, according to ORIENTATION.  The first value returned
corresponds to the frame with the given orientation, the second value is the
other frame."
  (flet ((split% (horizontal? first?)
           (let+ ((division (if first?
                           (list division (flex-spacer))
                           (list (flex-spacer) division)))
                  (#(first second) (if horizontal?
                                       (split-h frame division)
                                       (split-v frame division))))
             (if first?
                 (values first second)
                 (values second first)))))
    (ecase orientation
      (:top (split% nil nil))
      (:bottom (split% nil t))
      (:left (split% t t))
      (:right (split% t nil)))))

(defun replace-h (frame replacement-frame)
  "Return a new FRAME, taking horizontal coordinates from REPLACEMENT-FRAME
and the rest from FRAME."
  (if replacement-frame
      (let+ (((&frame-r/o &ign &ign bottom top) frame)
             ((&frame-r/o left right &ign &ign) replacement-frame))

        (frame left right bottom top))
      frame))

(defun replace-v (frame replacement-frame)
  "Return a new FRAME, taking vertical coordinates from REPLACEMENT-FRAME and
the rest from FRAME."
  (if replacement-frame
      (let+ (((&frame-r/o left right &ign &ign) frame)
             ((&frame-r/o &ign &ign bottom top) replacement-frame))
        (frame left right bottom top))
      frame))

(defun replace-with-orientation (orientation frame replacement-frame)
  (if (o-horizontal? orientation)
      (replace-h frame replacement-frame)
      (replace-v frame replacement-frame)))



(defgeneric project (mapping point)
  (:documentation ""))

(defun center (mapping)
  (project mapping (point 0.5 0.5)))

(defmethod project ((frame frame) (point point))
  (let+ (((&frame-r/o left right bottom top) frame)
         ((&point x y) point))
    (point (flex-project left right x) (flex-project bottom top y))))





(defgeneric render (target object)
  (:documentation "Render TARGET in FRAME, usually by emitting the appropriate
PGF commands using the PGF- functions.")
  (:method (target (sequence sequence))
    (map nil (curry #'render target) sequence)))

(defmacro with-clip-to-frame (frame &body body)
  `(progn
    ,@body)
#+nil  `(pdf:with-saved-state
     (%rectangle ,frame)
     (pdf:clip-path)
     (pdf:end-path-no-op)
     ,@body))




(defun unit-bounding-box ()
  (pgf-reset-bounding-box)
  (pgf-frame-rectangle +unit-frame+)
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
     (unit-bounding-box)
     ))

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

(define-condition latex-error (error)
  ((code :initarg :code)
   (log :initarg :log)))

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
  (external-program:start "xpdf"
                          (append (list "-remote" pdf-server
                                        (namestring pathname))
                                  (when raise? '("-raise")))))

(defparameter *default-temporary-file* nil
  "Default file used for rendering plots.  Use DEFAULT-TEMPORARY-FILE to
access it.")

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
    (setf *default-temporary-file* (generate-temporary-file)))
  *default-temporary-file*)

(defmacro with-displayed-picture ((&key (path '(default-temporary-file))
                                        (wrapper-filespec
                                         "/tmp/cl-flexplot.tex")
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
