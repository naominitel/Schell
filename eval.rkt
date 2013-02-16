#lang racket

(require racket/string)
(require (prefix-in schell: "variables.rkt"))
(require (prefix-in schell: "environment.rkt"))
(require (prefix-in schell: "command.rkt"))
(require (prefix-in schell: "builtins.rkt"))
(require (prefix-in schell: "functions.rkt"))
(provide eval)

(define-struct exn:command-not-found (command))

; whereis : string? -> string?
; return the full file path of the given command. If cmd is not a 
; relative nor absolute file path, search it in the PATH
(define (whereis command)
  (if (or (eq? (string-ref command 0) #\.) (eq? (string-ref command 0) #\/))
      ; relative or absolute path
      command   
      ; search in PATH
      (let ((PATH (string-split (getenv "PATH") ":")))
        (letrec ((locate-path
                   (lambda (command path)
                     (if (null? path)
                       (raise (make-exn:command-not-found command))
                       (let ((test (build-path (car path) command)))
                         (if (file-exists? test) test
                           (locate-path command (cdr path))))))))
          (path->string (locate-path command PATH))))))

; eval-args : list? -> list?
; evaluates a list of arguments and put them in a list
(define (eval-args args env)
  (if (null? args) null
    (let ((carargs (eval (car args) env)))
      (cons carargs (eval-args (cdr args) env)))))

; bind-env : list? list? -> list?
; bind the values contained in the second list to the names contained in
; the first list and return them in an environment (a list of pairs of string
; and value. Return #f if the size of lists doesn't match
(define (bind-env names vars)
  (cond
    ((null? names) 
     (if (null? vars) null #f))
    ((null? vars) 
     (if (null? names) null #f))
    (else
      (cons (mcons (symbol->string (car names)) (car vars))
            (bind-env (cdr names) (cdr vars))))))

; function-apply : function? list -> any
; arguments are ignored for now
(define (function-apply func args)
  (let ((result (eval (schell:function-expr func)
                      (mcons (bind-env (schell:function-args func) args)
                             (schell:function-env func)))))
    result))

; run-command : command? list? -> number? list?
; executes an external or internal command and return its exit code
; first search through builtin commands, then external programs
; some builtin functions can modify the environnement so return the
; modified environnement
(define (run-command cmd env)
  (let ((command (eval (schell:command cmd) env)))
    (let ((args (eval-args (schell:arguments cmd) env)))
      (if (schell:function? command)
        (function-apply command args)
        (let ((builtin (schell:envsearch schell:builtin-commands command)))
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
              (number->string (subprocess-status proc)))
            (if (null? args)
              (builtin env)
              (builtin env args))))))))

; eval : any list? -> any
; evaluates a Schell expression in the given environment
; returns a string containing the result of the expression and the modified
; environment
(define (eval expr env)
  (printf "eval ~a in ~a\n" expr env)
  (cond
    ((schell:quote? expr) (cadr expr))

    ((schell:lambda? expr) (schell:make-function
                             env
                             (schell:lambda-args expr)
                             (schell:lambda-expr expr)))

    ((schell:function? expr) expr)

    ((schell:command? expr)
     (with-handlers ((exn:command-not-found?
                       (lambda (e)
                         (printf "schell: ~a: command not found\n"
                                 (exn:command-not-found-command e))
                         1)))
        (run-command expr env)))

    ((schell:variable? expr)
     (schell:variable-value
       (schell:variable-name expr) env schell:builtin-variables))

    ((number? expr)
     (number->string expr))

    ((schell:text? expr)
     (symbol->string expr))

    ((string? expr) expr)))
