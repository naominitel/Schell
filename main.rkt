#lang racket

(require (prefix-in schell: "eval.rkt"))

; repl -> void
(letrec 
  ((repl
     (let ((env null))
       (lambda ()
         (printf "> ")
         (let-values (((ret nenv) (schell:eval (syntax->datum (read-syntax)) env)))
           (set! env nenv)
           (printf "expr returned : ~a\n" ret)
           (printf "env at the end : ~a\n" nenv))
         (repl)))))
  (repl))
