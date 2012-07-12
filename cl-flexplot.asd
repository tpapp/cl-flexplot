;;;; cl-flexplot.asd

(asdf:defsystem #:cl-flexplot
  :depends-on (#:alexandria
               #:anaphora
               #:cl-colors
               #:cl-fad
               #:cl-num-utils
               #:external-program
               #:fare-mop
               #:iterate
               #:let-plus)
  :serial t
  :pathname #P"src/"
  :components
  ((:file "package")
   (:file "low-level")
   (:file "orientation")
   (:file "margin")
   (:file "drawing-area")
   (:file "bounding-box")
   (:file "axis")
   (:file "plot")
   (:file "styles")
   (:file "lines")
   (:file "bands")
   (:file "functions")
   (:file "debug")))
