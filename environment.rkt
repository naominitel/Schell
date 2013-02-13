#lang racket

(require (prefix-in schell: "variables.rkt"))
(provide envsearch variable-value)

; an environnement is a list of pairs<string, value>

; envsearch : list variable -> any
; search the value of var in the given environnement
; and returns it. Returns #f if not found.
(define (envsearch env var)
  (let ((search (filter (lambda (e) (string=? (mcar e) var)) env)))
    (if (null? search) #f
      (mcdr (car search)))))

; variable-value : variable -> string
; evaluates a variable. Its value is first searched in
; the builtin variables, then in the UNIX environnement
; and finally, in the given local environnement, which
; is a list of environnements. If still not found, 
; returns an empty value.
(define (variable-value name env builtins)
  (let ((builtin (envsearch builtins name)))
    (if (false? builtin) 
      (let ((environ (getenv name)))
        (if (false? environ)
          (letrec ((local-envsearch
                     (lambda (env)
                     (if (null? env) schell:empty
                       (let ((localenv (envsearch (car env) name)))
                         (if (false? localenv)
                           (local-envsearch (cdr env))
                           localenv))))))
            (local-envsearch env))
          environ))
      builtin)))
