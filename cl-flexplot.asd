;;;; cl-flexplot.asd

(asdf:defsystem #:cl-flexplot
  :depends-on (#:alexandria
               #:anaphora
               #:cl-colors
               #:cl-fad
               #:cl-num-utils
               #:external-program
               #:let-plus)
  :serial t
  :components
  ((:module "base"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "package")
     (:file "latex")
     (:file "flex")
     (:file "pgf")
     (:file "orientation")
     (:file "frame")
     (:file "external")))
   (:module "plot"
    :serial t
    :components
    ((:file "margin")
     (:file "drawing-area")))
   (:module "misc"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "debug")))))
