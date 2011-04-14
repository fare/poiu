;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software, same MIT-style license as ASDF. See poiu.lisp.    ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2001-2010 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem :poiu
    :author ("Daniel Barlow" "Andreas Fuchs" "Francois-Rene Rideau")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "POIU"
    :long-description "Parallel Operator on Independent Units
POIU is a variant of ASDF that may operate on your systems in parallel.
POIU will notably compile each Lisp file in its own forked process,
in parallel with other operations (compilation or loading).
However, it will load FASLs serially as they become available."
    ;; It is not currently safe to upgrade asdf itself as part of an asdf operation;
    ;; that would require special magic handling by asdf. So we don't try.
    ;; :depends-on ((:version :asdf "2.010"))
    :components ((:file "poiu")))
