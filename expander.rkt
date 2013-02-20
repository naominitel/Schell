#lang racket

(provide expand)
(require (prefix-in schell: "conditionals.rkt"))
(require (prefix-in schell: "let.rkt"))

(define (expand expr)
  (cond
    ((schell:or-expr? expr) (expand (schell:or-expand expr)))
    ((schell:and-expr? expr) (expand (schell:and-expand expr)))
    ((schell:let-expr? expr) (expand (schell:let-expand expr)))
    ((schell:let*-expr? expr) (expand (schell:let*-expand expr)))
    ((schell:letrec-expr? expr) (expand (schell:letrec-expand expr)))
    (else expr)))
