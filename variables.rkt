#lang racket

(provide empty text? value? variable? variable-name quote?)

; An empty string
(define empty "")

; text? : datum -> bool
; returns true if the given expr represents program text.
(define (text? expr)
  (symbol? expr))

; value? : datum -> bool
; returns true if the given expr can be evaluated to a value
; such expressions are of the form \$.*
(define (value? expr)
  (and (text? expr)
       (eq? (string-ref (symbol->string expr) 0) #\$)))

; variable? : datum -> bool
; returns true if the given expr represents a variable.
; a variable is a value of the form \$+*
(define (variable? expr)
  (and (value? expr)
       (not (string=? (variable-name expr) empty))))

; variable-name : variable -> string
(define (variable-name var)
  (if (value? var)
      (substring (symbol->string var) 1)
      empty))

; quote? : datum -> boolean
; returns true if the expression is a quoted expression, and should
; not be evaluated
(define (quote? expr)
  (and (pair? expr) (eq? 'quote (car expr))))
