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
  :components
  ((:module "low-level"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "package")
     (:file "latex")
     (:file "flex")
     (:file "pgf")
     (:file "frame")
     (:file "external")))
   (:module "plot"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "orientation")
     (:file "margin")
     (:file "drawing-area")
     (:file "bounding-box")
     (:file "axis")
     (:file "plot")))
   (:module "objects"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "simple-objects")))
   (:module "misc"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "debug")))))
