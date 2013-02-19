#lang racket

(provide if-expr? or-expr? or-expand and-expr? and-expand)

(define (if-expr? expr)
  (and (list? expr) (eq? 'if (car expr))))

(define (or-expr? expr)
  (and (list? expr) (eq? 'or (car expr))))

(define (or-expand expr)
  (letrec 
    ((expand 
       (lambda (exprs)
         (if (null? exprs) '#f
           (list 'if 
                 (car exprs)
                 (expand (cdr exprs)))))))
    (expand (cdr expr))))

(define (and-expr? expr)
  (and (list? expr) (eq? 'and (car expr))))

(define (and-expand expr)
  (letrec
    ((expand
       (lambda (exprs)
         (if (null? exprs) '#t
           (list 'if
                 (expand (cdr exprs))
                 (car exprs))))))
    (expand (cdr expr))))


