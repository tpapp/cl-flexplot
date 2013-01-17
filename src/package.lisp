;;;; package.lisp

(defpackage #:cl-flexplot
  (:nicknames #:fxpl)
  (:use #:cl #:anaphora #:alexandria #:cl-colors #:cl-num-utils #:fare-mop
        #:iterate #:let-plus #:optima)
  (:shadowing-import-from #:cl-num-utils #:mean #:variance #:median ; also in ALEXANDRIA
                          #:sum)        ; also in ITERATE
  (:export
   #:displaying
   #:plot
   #:lines
   #:fx
   #:mark
   #:circle
   #:point
   #:categories
   #:orientation
   #:o-horizontal?
   #:o-other?
   #:o-opposite
   #:o-orthogonal-pair
   #:orientation-dependent
   #:flex
   #:coordinate
   #:*latex-preamble*
   #:*latex-width*
   #:*latex-height*
   #:*guide-style*
   #:categories-guides
   #:stroke-style
   #:fill-style
   #:include
   #:marks
   #:marks-xy
   #:axis
   #:*axis-margin*
   #:mark-xy
   #:label
   #:*label-color*
   #:lines-xy
   #:segment
   #:segment-xy
   #:q5-y
   #:*default-mark*
   #:guide
   #:object-points
   #:extend-bounding-box
   #:bounding-box
   #:strut
   #:+dash-solid+
   #:+dash-dot+
   #:+dash-line+
   #:pt
   #:em
   #:*fill-style*
   #:diagonal-guide
   #:q5-x)
  (:export
   :strut
   :make-bounding-box
   :title
   #:square-bounding-box
   #:+origin+
   #:*stroke-style*
   #:*fill-style*
   #:*guide-style*
   #:vertical-guide
   #:diagonal-guide
   #:horizontal-guide
   #:categories-guides
   #:strut-y
   #:strut-x
   #:qy
   #:*band-style*
   #:vertical-band
   #:write-swatches))
