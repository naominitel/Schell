#lang racket

(provide let-expr? let-expand let*-expr? let*-expand letrec-expr? letrec-expand)

; let? : any -> boolean?
; returns true if a the given expr is a let-expr
(define (let-expr? expr)
  (and (list? expr) (eq? 'let (car expr))))

; let-expr : let? -> any
; returns the expr of a let expression. e.g. (let (...) expr) -> expr
(define (let-expr expr)
  (caddr expr))

; let-bindings : let? -> list?
; returns all the bindings assigned by the let expr. example :
; (let ((x1 e1) (x2 e2)) expr) -> ((x1 e1) (x2 e2))
(define (let-bindings expr)
  (cadr expr))

; let-vars : let? -> list?
; returns the list of the names of the variables declared inside the let expr
; example, for (let ((e1 v1) (e2 v2)) expr), this returns (e1 e2)
(define (let-vars expr)
    (map (lambda (binding) (car binding)) (let-bindings expr)))

; let-vals : let? -> list?
; same as above but returns here the list of the values given to the variables
; for the example above, this would return (v1 v2)
(define (let-vals expr)
    (map (lambda (binding) (cadr binding)) (let-bindings expr)))

; let-expand : let? -> any
; expand a let expr into ((lambda (e1 ...) expr) e1 e2)
(define (let-expand expr)
  (cons (list 'lambda (let-vars expr) (let-expr expr)) (let-vals expr)))

; let*? : any -> boolean?
; returns true if the given expr is a let* expr
(define (let*-expr? expr)
  (and (list? expr) (eq? 'let* (car expr))))

; let*-expand : let*? -> let?
; expand (let* ((x1 e1) ...) expr) into (let ((x1 e1)) (let (...) expr))
(define (let*-expand expr)
  (if (null? (cadr expr)) (caddr expr)
    (list
      'let
      (list (caadr expr))
      (let*-expand (list 'let* (cdadr expr) (caddr expr))))))

; letrec? : any -> boolean?
; returns true if the given expr is a letrec expr
(define (letrec-expr? expr)
  (and (list? expr) (eq? 'letrec (car expr))))

; letrec-expand : letrec? -> let?
; expand (letrec ((x1 e1) ...) e) into 
; (let ((x1 <undef>) ...) (set! x1 e1) ... e)
(define (letrec-expand expr)
  (let* ((vars (let-bindings expr))
         (new-vars (map (lambda (var) (list (car var) '<undef>)) vars))
         (sets (map (lambda (var) (cons 'set! var)) vars)))
    (list 'let new-vars (cons 'begin (append sets (cddr expr))))))
