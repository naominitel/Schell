## Schell : The Scheme shell

_Other possible names : Lish, or lsh (the Lisp shell), Sheme ou Schell, lambdash, etc._

Schell is an attempt to provide a full-featured UNIX shell using the Scheme syntax and programming mechanisms. It's not aimed at replacing zsh or whatever, just a fun project.

### Syntax

Keep in mind that the first fundamental difference between Schell and classic shells is that this one works in a functional style. Instead of _executing_ an instruction, it evaluates expressions. Almost everything has a value and can be evaluated. In interactive mode, the shell works like a Lisp REPL.

#### Command call

```
(cmd arg1 arg2 ...)
```

Runs `cmd` with the given arguments. It may be either a builtin command, an user-defined function, or an external program designed by its absolute or relative path, or just by its name when it's located in a directory of the `PATH` environnement variable.

Arguments are evaluated in sequence and their value are passaed as parameter. To the ccommand.

#### Environnement variables

As in any shell, program text such as `text` is evaluated to a string. The main reason of this is to avoid having to type things such as "ls" "foo" with quotes each time the user wants to run a command, which may be boring.

Therefore, accessing symbols and variables is done using the `$symbol` syntax. `$var` is evaluated to the value of the variable name `var`.

##### Environnements

Schell follows the same rules as any shell for searching for variable values. It first searches the builtin variables, which are internal to the shell and set by it (see below), then the UNIX environnement variables (getenv). Finally, if the variable is still not found, it searches through the local environnements stack.

If the variable is still not found in the local environnement, an empty value is returned.

##### Mutation

Altough you usually don't use mutation in functional langages, it is provided via the `set!` function. When a variable is set, it is searched in the same order than when reading a variable (except that builtin variables can't be set), and when it is found, its value is changed. If no variable with this name is found, it is then added with the given value at the top of the local environnements stack.

The `export` function is used to declare a new environnement variable. If no value is providen, its value will be the value of the var in the current local environnement.

```
(set! var 3)    ; This sets var only inside the current shell
(export! var)   ; var is now an UNIX env var
(set! var 5)    ; Now, this modifies the env var
```

As a recall, an environnement variable is passed to, and accessible by any child process created from this shell process.

##### Builtin variables

All common shell variables are implemented

* `$#` gives the number of parameters passed to the script.
* `$N` where N is a number is the Nth parameter
* `$@` is a string containing all parameters
* `$?` is the return value of the last command
* `$$` is the PID of the current process
* `$PWD` is the current working directory of the shell process

For the moment, only `$PWD` and `$$` are implemented.

The following variables are added :

* `$#t` represents a value that is true.
* `$#f` represents a value that is false.
* `$empty` represents an empty value (`""`)
* `$args` gives all the arguments as a list

#### Chaining programs (Not implemented yet)

Getting the output of a program is done by putting a `$` symbol before the call of the function :

```
$(ls)
```

The above expression is evaluated to the output text of the ls command.

Another way of chaining programs is using a pipe, which is done the following way :

```
(pipe (cmd1 ...) (cmd2 ...))
```

Runs in the same time `cmd1` and `cmd2`, and connects the standard output of `cmd1` to the standard input `cmd2`. When prefixed by a `$`, it returns the output of `cmd2`.

#### Value types

Almost every value is text, as it is the most common and practical way of manipulating values that must be transferred between programs.

A value of `"0"` (zero) is considered as true. Every other value is considered false. The special variables `$#t` and `$#f` respectively return a true and a false value. __(Not implemented)__

Traditional Lisp lists and pairs are also supported. Lists can be constructed using the `'()` syntax, or the `cons` constructor. __(Not implemented).__ 

Note that passing a list as parameter to a command will not work as expected. To transform it into a list of strings that a program will understand, you must "extract" the values of the list by using the @syntax. :

```
(setenv! list '(1 2 3 4))

(ls $list) ; equivalent to ls (1 2 3 4) which is obviously wrong
(ls @list) ; equivalent to ls 1 2 3 4 which is correct
```

__(This is not implemented yet)__

An empty value is equivalent to `""`.

As in regular Scheme, placing a quote (`'`) in front of an expression will cause it to be treated as such without being evaluated. This means that `'(wrong)` will not cause the `wrong` command to be run, but instead return a list containing `'wrong`. In the same way, `'$PATH` does not returns the value of the variable, but the text `$PATH`.

#### Conditionals

Conditions are exprimed the same as in tradititonal Scheme :

```
(if (expr) expr_true expr_false)
```

If the `expr` is evaluated to true, then the expression returns `expr_true`, otherwise it returns `expr_false`. 

Any expression is considered as false, except the string `"0"`, which is true. This may looks weird but keep in mind that conditionals in shell often consists in checking command exit codes, and `"0"` is by convention the output of a command that ran properly.

Standard Scheme booleans such as `eq?`, `<`, `>`, etc. are all supported.
The standard shell booleans are also provided :

* `(file? foo)` tells if foo is a file (Bash `-f`)
* `(dir? foo)` tells if foo is a directory (Bash `-d`)
* `(exists? foo)` tells if foo exists (Bash `-e`)

#### Exit codes

A command execution such as `(cmd args...)` is evaluated to the exit code of the command.

The exit code of the last run command is also stored in the environnement variable `?` which can be accessed like any other variable using `$?`. 

It is possible to run a command only-if the precedent command succeeded by using the `and` function. __(Not yet implemented)__.

```
(and expr1 expr2 ...)
```

`and` evaluates in order every expression passed as parameter and returns the value of the first expression evaluated to `$#f`. If all exceptions are evaluated to `$#t`, it returns `$#t`. That means that the following code :

```
(and (cmd1) (cmd2))
```

will never run `cmd2` if `cmd1` fails because it will return just after `cmd1` finished executing.

Similarly, the `or` function that returns the first `true` value it founds, and false otherwise can be used to run a command only if the previous command failed : __(Not yet implemented)__

```
(or (cmd1) (cmd2))
```

If `cmd1` succeed, or will return `true` and `(cmd2)` will never be evaluated. 

#### Comments

Both traditional Lisp comments using a semicolon `;` and Shell-like comments using a sharp `#` are supported, in order to keep compatibility with other shells : 

```
#!/bin/schell

; This script can be run from Bash !
(echo Hello, world)
```

__(This is not implemented. :] )__

#### Functions

Functions are defined the same way as in Scheme, using the define keyword :

```
(define (sum a b)
    (+ a b))
```

_(Currently, this keyword is not implemented and functions must be created using the `lambda` syntax, then stored in a variable, but hey work anyway.)_

Passing functions as parameters or return values is done with the `$` syntax, just as any other variable.

#### Loops (Not implemented)

It is generally preferred to implement iterative algorithms using (tail-)recursion. But two other forms are provided. First, the functional inlines map and apply. 

Second, the do form :

```
(do action while expr)
(while expr do action)
```

The first repeats `action` tills `expr` returns `$#f`. The second first check that `expr` is `$#t` then executes `action`. The whole expression returns the value of the last executed `action`.

#### Let

Allows to add variables to the environnement when running a child command without modifying the current environnement variables : 

```
(let ((PATH $PATH:$HOME/bin) (foo 42))
  (some-script.sch))
```

Possibility : rename this form (for example `with`) not to confuse it with the standard Scheme let that does not affect envvars.
For the moment, variables declared inside let forms are not passed to subprocesses.
Schell also provides the `let*` form.

#### Builtins

Schell supports the following standard builtin commands : 

* `cd`
* `export`
* `echo`
* `set!`

They are used as any other command.
A more functional form of `cd` is provided through the `in` function :

```
(in /home
  (cmd args...))
```

Runs `cmd` as it was run from `/home`. __(Just an idea, not implemented).__

