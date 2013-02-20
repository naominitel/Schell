#lang racket

(provide expand)
(require (prefix-in schell: "variables.rkt"))
(require (prefix-in schell: "conditionals.rkt"))
(require (prefix-in schell: "let.rkt"))

; expand: any -> SchellExpr
; expand exprs with special syntax into a subset syntax understood by $eval
(define (expand expr)
  (if (or (schell:quote? expr) (not (pair? expr))) expr
    (let ((expr (map expand expr)))
      (cond
        ((schell:or-expr? expr) (expand (schell:or-expand expr)))
        ((schell:and-expr? expr) (expand (schell:and-expand expr)))
        ((schell:let-expr? expr) (expand (schell:let-expand expr)))
        ((schell:let*-expr? expr) (expand (schell:let*-expand expr)))
        ((schell:letrec-expr? expr) (expand (schell:letrec-expand expr)))
        (else expr)))))
