;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software, same MIT-style license as ASDF. See poiu.lisp.    ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2001-2011 ITA Software, Inc.  All rights reserved. ;;;
;;; Copyright (c) 2011-2012 Google, Inc.  All rights reserved.       ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :asdf)
#-asdf2 (error "XCVB requires ASDF 2")

(let ((old-ver (asdf-version)))
  (load-system :asdf)
  (let ((min "2.26.15")
	(ver (asdf-version)))
    (unless (or (version-satisfies old-ver "2.014.8") ; first version to do magic upgrade
		(equal ver old-ver))
      (error "You must upgrade ASDF to your latest *before* you load POIU~%~
		If you're trying to load POIU at a REPL, try again, it should work."))
    (unless (and ver (version-satisfies ver min))
      (error "POIU requires ASDF ~D or later, you only have ~D" min ver))))

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
    :depends-on ((:version :asdf "2.26.15")) ; for new compute-action-stamp
    :components ((:file "poiu")))
