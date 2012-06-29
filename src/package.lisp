;;;; package.lisp

(defpackage #:cl-flexplot
  (:use #:cl #:anaphora #:alexandria #:cl-colors #:cl-num-utils #:fare-mop
        #:iterate #:let-plus)
  (:shadowing-import-from #:cl-num-utils #:mean #:variance #:median) ; also in ALEXANDRIA
  )
