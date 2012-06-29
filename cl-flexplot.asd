;;;; cl-flexplot.asd

(asdf:defsystem #:cl-flexplot
  :depends-on (#:alexandria
               #:anaphora
               #:cl-colors
               #:cl-fad
               #:cl-num-utils
               #:external-program
               #:let-plus)
  :pathname #P"src/"
  :serial t
  :components
  ((:module "base"
    :serial t
    :components
    ((:file "package")
     (:file "latex")
     (:file "flex")
     (:file "pgf")
     (:file "orientation")
     (:file "frame")))
   (:module "plot"
    :serial t
    :components
    ((:file "axis")))
   (:module "misc"
    :serial t
    :components
    ((:file "debug")))))
