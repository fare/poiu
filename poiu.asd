(defsystem "poiu"
  :author ("Daniel Barlow" "Andreas Fuchs" "Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "Parallel Operator on Independent Units"
  :long-description "POIU is a extension to ASDF that may operate on your systems in parallel.
POIU will notably compile each Lisp file in its own forked process,
in parallel with other operations (compilation or loading).
However, it will load FASLs serially as they become available."
  :depends-on ((:version "asdf" "3.0.2")
               (:feature (:and :sbcl :os-unix) "sb-posix"))
  :version "1.31.1"
  :components
  ((:file "queue")
   (:file "fork")
   (:file "action-graph" :depends-on ("queue"))
   (:file "background-process" :depends-on ("queue" "fork"))
   (:file "poiu" :depends-on ("action-graph" "background-process"))))
