#lang racket

(require racket/string)
(require (prefix-in schell: "variables.rkt"))
(require (prefix-in schell: "environment.rkt"))
(require (prefix-in schell: "command.rkt"))
(require (prefix-in schell: "builtins.rkt"))
(provide eval)

(define-struct exn:command-not-found (command))

; whereis : string -> string
; return the full file path of the given command. If cmd is not a 
; relative nor absolute file path, search it in the PATH
(define (whereis command)
  (if (or (eq? (string-ref command 0) #\.)
          (eq? (string-ref command 0) #\/))
      ; relative or absolute path
      command   
      ; search in PATH
      (let ((PATH (string-split (getenv "PATH") ":")))
        (letrec ((locate-path
                   (lambda (command path)
                     (if (null? path)
                       (raise (make-exn:command-not-found command))
                       (let ((test (build-path (car path) command)))
                         (if (file-exists? test)
                           test
                           (locate-path command (cdr path))))))))
          (path->string (locate-path command PATH))))))

; eval-args : list -> string
; evaluates a list of arguments and put them in a list
(define (eval-args args env)
  (if (null? args) null
    (let-values (((carargs nenv) (eval (car args) env))) 
      (cons carargs (eval-args (cdr args) env)))))

; run-command : datum env -> string env
; executes an external or internal command and return its exit code
; first search through builtin commands, then external programs
; some builtin functions can modify the environnement so return the
; modified environnement
(define (run-command cmd env)
  (let-values (((command nenv) (eval (schell:command cmd) env)))
    (let ((args (eval-args (schell:arguments cmd) env))
          (builtin (schell:envsearch schell:builtin-commands command)))
      (if (false? builtin)
        (let-values (((proc out in err) 
                      (let ((stdin (current-input-port))
                            (stdout (current-output-port))
                            (stderr (current-error-port))
                            (command (whereis command)))
                        (parameterize ((current-directory (current-directory)))
                          (if (null? args)
                              (subprocess stdout stdin stderr command)
                              (subprocess stdout stdin stderr command #|args|#))))))
          (subprocess-wait proc)
          (values (number->string (subprocess-status proc)) env))
        (if (null? args)
          (builtin env)
          (builtin env args))))))

; eval datum env -> string env
; evaluates a Schell expression in the given environment
; returns a string containing the result of the expression and the modified
; environment
(define (eval expr env)
  (printf "eval ~a in ~a\n" expr env)
  (cond
    ((schell:quote? expr)
     (values (cadr expr) env))

    ((schell:command? expr)
     (with-handlers ((exn:command-not-found?
                       (lambda (e)
                         (printf "schell: ~a: command not found\n"
                                 (exn:command-not-found-command e))
                         (values 1 env))))
        (run-command expr env)))

    ((schell:variable? expr)
     (values (schell:variable-value
               (schell:variable-name expr)
               env schell:builtin-variables)
             env))

    ((number? expr)
     (values (number->string expr) env))

    ((schell:text? expr)
     (values (symbol->string expr) env))

    ((string? expr)
     (values expr env))))

