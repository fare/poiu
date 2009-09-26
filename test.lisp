;; sbcl --load test.lisp

(require :asdf)
(push "/home/fare/.local/share/common-lisp/systems/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :poiu)
(asdf:oos 'asdf:parallel-load-op :exscribe)
(exscribe::process-command-line
 '("-I" "/home/fare/fare/www" "-o" "-" "-H" "/home/fare/fare/www/index.scr"))
