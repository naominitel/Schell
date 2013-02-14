#lang racket

(provide function? make-function function-args function-expr function-env lambda? lambda-args lambda-expr)

; function? : datum -> boolean
; returns true if the expression is an function that can be called
; functions are lists of ('FUNCTION env args expr)
(define (function? expr)
  (and (list? expr) (eq? 'function (car expr))))

(define (make-function env args expr)
  (list 'function env args expr))

; function-args : function? -> list
; returns the list of args taken as parameters
(define (function-args func)
  (caddr func))

; function-expr : function? -> datum
; returns the expr of a function
(define (function-expr func)
  (cadddr func))

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

(define (lambda-args expr)
  (cadr expr))
