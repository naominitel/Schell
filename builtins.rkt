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
    (printf "LOCALSET: searching for ~a returned ~a" var search)
    (if (null? search) #f
      (set-mcdr! (car search) value))))

; variable-set! : string any list -> list
; sets the variable var to the given value. First check if exists
; as an envvar and sets it, otherwise sets it in the local environnement,
; creating it if it doesn't exists.
; returns the environment (modified or not)
(define (variable-set var value localenv)
  (if (false? (getenv var))
    (letrec ((local-envset
               (lambda (env)
                 (printf "LOCALENVSET: setting (~a . ~a) in ~a\n" var value env)
                 (if (null? env)
                   (if (null? localenv)
                     (cons (list (mcons var value)) localenv)
                     (cons (cons (mcons var value) (car localenv)) (cdr localenv)))
                   (let ((localset (envset! (car env) var value)))
                     (if (false? localset)
                       (local-envset (cdr env))
                       localenv))))))
      (local-envset localenv))
    (begin 
      (putenv var value)
      localenv)))

; set : var value env -> void env
(define schell-set
  (case-lambda
    ((env args) (values (void) (variable-set (car args) (cadr args) env))))) 

; cd : env string -> void env
; change directory to the given path
(define cd
  (case-lambda
    ((env) (cd (getenv "HOME")))
    ((env args) (values (current-directory (car args)) env))))

(define export
  (case-lambda
    ((env args)
     (if (null? (cdr args))
       (export env (list (car args) (schell:variable-value (car args) env null)))
       (values (putenv (car args) (cadr args)) env)))))

(define echo
  (case-lambda
    ((env args)
     (if (null? args)
       (begin
         (printf "\n")
         (values 0 env))
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
