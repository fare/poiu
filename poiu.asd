;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software, same MIT-style license as ASDF. See poiu.lisp.    ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2001 Daniel Barlow                                 ;;;
;;; Copyright (c) 2008 ITA Software                                  ;;;
;;; Copyright (c) 2011 Google, Inc.                                  ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :asdf)

(defsystem :poiu
  :author ("Daniel Barlow" "Andreas Fuchs" "Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "POIU"
  :long-description "Parallel Operator on Independent Units
POIU is a variant of ASDF that may operate on your systems in parallel.
POIU will notably compile each Lisp file in its own forked process,
in parallel with other operations (compilation or loading).
However, it will load FASLs serially as they become available."
  :defsystem-depends-on (:asdf) ; let's enhance our chances that ASDF 3 is loaded.
  :version (:read-file-form "poiu.lisp" :at (1 2 2))
  :components ((:file "poiu")))
