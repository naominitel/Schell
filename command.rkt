#lang racket

(provide command? command arguments)

; command? : datum -> bool
; returns true if expr corresponds to a command call
; of form (cmd [args ...])
(define (command? expr)
  (and (list? expr) (not (null? expr))))

; command : datum -> datum
; returns the command part of a command call
; e.g. for (ls /home), returns "ls"
(define (command expr)
  (car expr))

; arguments : datum -> list
; returns the arguments of a command call
(define (arguments expr)
  (cdr expr))
