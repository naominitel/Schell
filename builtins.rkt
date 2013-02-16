#lang racket

(require mzlib/os)
(require (prefix-in schell: "environment.rkt"))
(provide builtin-variables builtin-commands)

(define builtin-variables
  (list
   (mcons "$" (number->string (getpid)))
   (mcons "PS1" (string-append (getenv "USER") "@localhost:" "~"))
   (mcons "?" "0")
   (mcons "PWD" (current-directory))))

; envset! : list string any -> void
; search for the variable in the given env and sets its.
; if not found, returns #f
(define (envset! env var value)
  (printf "LOCALSET: setting (~a . ~a) in ~a\n" var value env)
  (let ((search (filter (lambda (e) (string=? (mcar e) var)) env)))
    (printf "LOCALSET: searching for ~a returned ~a\n" var search)
    (if (null? search) #f
      (set-mcdr! (car search) value))))

; variable-set : string? any mpair? -> void?
; sets the variable to the given value. First checks if it exists as an
; envvar and sets it, otherwise sets it in the local environment, creating it
; if it doesn't exists at the top of the environment stack.
; setting the variable is done in place and this function returns nothing
; this assumes the env-stack is never null and contains at least an element
; (that can be null), but ensures (eval) is now tail-recursive.
(define (variable-set var value env-stack)
  (if (false? (getenv var))
    (letrec ((local-envset!
              (lambda (env)
                (if (null? env)
                  (set-mcar!
                    env-stack
                    (cons (mcons var value) (mcar env-stack)))
                  (let ((test (envset! (mcar env) var value)))
                    (if (false? test)
                      (local-envset! (mcdr env))
                      (void)))))))
      (local-envset! env-stack))
    (putenv var value)))

; set : string? any mpair? -> void?
(define schell-set
  (lambda (env args)
    (variable-set (car args) (cadr args) env)))

; cd : mpair? string? -> void?
; change directory to the given path
(define cd
  (case-lambda
    ((env) (cd (getenv "HOME")))
    ((env args) (current-directory (car args)))))

; export : mpair? string? any -> void?
; puts the given (variable, value) in the UNIX environnement variables. If
; no value is providen, use actual value of the variable in the local env stack
(define export
  (case-lambda
    ((env args)
     (if (null? (cdr args))
       (export env (list (car args) (schell:variable-value (car args) env null)))
       (putenv (car args) (cadr args))))))

; echo : env? list? -> void?
; prints all elements of the given list on the standard output
(define echo
  (case-lambda
    ((env args)
     (if (null? args)
       (begin (printf "\n") 0)
       (begin
         (printf "~a " (car args))
         (echo env (cdr args)))))
     ((env) (echo env null))))

(define builtin-commands
  (list
    (mcons "getwd" (lambda (args) (printf "~a\n" (current-directory))))
    (mcons "cd" cd)
    (mcons "export" export)
    (mcons "echo" echo)
    (mcons "set!" schell-set)))
