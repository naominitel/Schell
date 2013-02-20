#lang racket

(provide begin?)

(define (begin? expr)
  (and (pair? expr) (eq? 'begin (car expr))))
