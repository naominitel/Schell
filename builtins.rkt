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

(define-struct exn:bad-parameters (command expected given))

; set : string? any mpair? -> void?
(define schell-set
  (case-lambda
    ((env var value) (variable-set var value env))
    (args (raise (make-exn:bad-parameters "set!" 2 (- (length args) 1))))))

; cd : mpair? string? -> void?
; change directory to the given path
(define cd
  (case-lambda
    ((env) (cd env (getenv "HOME")))
    ((env path) (current-directory path))
    (args (raise (make-exn:bad-parameters "cd" 1 (- (length args) 1))))))

; export : mpair? string? any -> void?
; puts the given (variable, value) in the UNIX environnement variables. If
; no value is providen, use actual value of the variable in the local env stack
(define export
  (case-lambda
    ((env var value) (putenv var value))
    ((env var) (export env var (schell:variable-value var env null)))
    (args (raise (make-exn:bad-parameters "export" 1 (- (length args) 1))))))

; echo : env? list? -> void?
; prints all elements of the given list on the standard output
(define echo
  (case-lambda
    (() (raise (make-exn:bad-parameters "echo" 1 0)))
    ((env) (begin (printf "\n") 0))
    (args
      (let (args (cdr args))
        (if (null? args) (echo env)
          (begin (printf "~a " (car args))
                 (echo env (cdr args))))))))

(define builtin-commands
  (list
    (mcons "getwd" (lambda (args) (printf "~a\n" (current-directory))))
    (mcons "cd" cd)
    (mcons "export" export)
    (mcons "echo" echo)
    (mcons "set!" schell-set)))
