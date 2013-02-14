#lang racket

(provide function? make-function function-expr function-env lambda? lambda-expr)

; function? : datum -> boolean
; returns true if the expression is an function that can be called
; functions are lists of ('FUNCTION env expr)
(define (function? expr)
  (and (list? expr) (eq? 'function (car expr))))

(define (make-function env expr)
  (list 'function env expr))

; function-expr : function? -> datum
; returns the expr of a function
(define (function-expr func)
  (caddr func))

; function-env : function? -> env
; returns the environnement in which the function was created
(define (function-env func)
  (cadr func))

; lambda? : datum -> boolean
; returns true if the expr is a lambda-expr that should be evaluated
; to a function. (lambda (args) expr)
(define (lambda? expr)
  (and (list? expr) (eq? 'lambda (car expr))))

(define (lambda-expr expr)
  (caddr expr))
