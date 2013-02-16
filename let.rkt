#lang racket

(provide let? let-expr let-vars let-vals let*? let*-expand)

(define (let? expr)
  (and (list? expr) (eq? 'let (car expr))))

(define (let-expr expr)
  (caddr expr))

; let-vars : let? -> list?
; returns the list of the names of the variables declared inside the let expr
; example, for (let ((e1 v1) (e2 v2)) expr), this returns (e1 e2)
(define (let-vars expr)
  (letrec 
    ((extract-vars
       (lambda (args)
         (if (null? args) null
           (cons (caar args) (extract-vars (cdr args)))))))
    (extract-vars (cadr expr))))

; let-vals : let? -> list?
; same as above but returns here the list of the values given to the variables
; for the example above, this would return (v1 v2)
(define (let-vals expr)
  (letrec
    ((extract-vals
       (lambda (args)
         (if (null? args) null
           (cons (cadar args) (extract-vals (cdr args)))))))
    (extract-vals (cadr expr))))

(define (let*? expr)
  (and (list? expr) (eq? 'let* (car expr))))

(define (let*-expand expr)
  (if (null? (cadr expr)) (caddr expr)
    (list
      'let
      (list (caadr expr))
      (let*-expand (list 'let* (cdadr expr) (caddr expr))))))
