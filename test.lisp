; sbcl --load test.lisp
; ../single-threaded-ccl/single-threaded-ccl --load test.lisp
; clisp -i test.lisp

(in-package :cl-user)
#+sbcl (require :asdf)
#+clozure (load "../asdf/asdf.lisp")
#+clisp (require :asdf)

(setf *load-verbose* t
      *load-print* t
      *compile-verbose* t
      *compile-print* t)

(push "/home/fare/.local/share/common-lisp/systems/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :poiu :verbose t)
(asdf:oos 'asdf:load-op :cl-launch :verbose t)

(setf *load-verbose* t
      *load-print* t
      *compile-verbose* t
      *compile-print* t)

(asdf:oos 'asdf:parallel-load-op :exscribe :verbose t)
(exscribe::process-command-line
 '("-I" "/home/fare/fare/www" "-o" "-" "-H" "/home/fare/fare/www/index.scr"))

(cl-launch:quit 0)
