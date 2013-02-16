#lang racket

(require (prefix-in schell: "eval.rkt"))
(require (prefix-in schell: "environment.rkt"))

; repl -> void
(letrec 
  ((repl
     (let ((env (schell:make-envstack)))
       (lambda ()
         (printf "> ")
         (let ((ret (schell:$eval (syntax->datum (read-syntax)) env)))
           (printf "expr returned : ~a\n" ret)
           (printf "env at the end : ~a\n" env))
         (repl)))))
  (repl))
