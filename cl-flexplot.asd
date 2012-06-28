;;;; cl-flexplot.asd

(asdf:defsystem #:cl-flexplot
  :serial t
  :depends-on (#:alexandria
               #:anaphora
               #:cl-colors
               #:cl-fad
               #:cl-num-utils
               #:external-program
               #:let-plus)
  :pathname #P"src/"
  :components ((:file "package")
               (:file "latex")
               (:file "flex")
               (:file "pgf")
               (:file "orientation")
               (:file "frame")
               (:file "debug")))
