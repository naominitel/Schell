#lang racket

(provide begin?)

(define (begin? expr)
  (and (list? expr) (eq? 'begin (car expr))))
