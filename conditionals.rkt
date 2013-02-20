#lang racket

(provide true false if-expr? or-expr? or-expand and-expr? and-expand)

(define true "0")
(define false "1")

(define (if-expr? expr)
  (and (pair? expr) (eq? 'if (car expr))))

(define (or-expr? expr)
  (and (pair? expr) (eq? 'or (car expr))))

(define (or-expand expr)
  (letrec 
    ((or-expand-args
       (lambda (args)
         (cond
           ((null? args) false)
           ((null? (cdr args)) (car args))
           (else
             (list 'let
                   (list (list 'expr (car args)))
                   (list 'if expr expr (or-expand-args (cdr args)))))))))
    (or-expand-args (cdr expr))))

(define (and-expr? expr)
  (and (pair? expr) (eq? 'and (car expr))))

(define (and-expand expr)
  (letrec
    ((and-expand-args
       (lambda (args)
         (cond
           ((null? args) true)
           ((null? (cdr args)) (car args))
           (else
             (list 'if (car args) (and-expand-args (cdr args)) false))))))
    (and-expand-args (cdr expr))))
