POIU: Parallel Operator on Independent Units
============================================

POIU is an ASDF extension that will parallelize your Common Lisp builds,
for some build speedup, both through parallelization and reduced GC.


WARNING
-------

POIU 1.34 is broken: It requires more changes to actually work with a recent
ASDF (3.3.2 or later), yet is modified enough to not work with older ASDF.

For a working combination, try POIU 1.31.1 and ASDF 3.1.7 -- which if your
implementation comes with a more recent ASDF
(as checked by `(require :asdf) (asdf:asdf-version)`) may require
overriding your implementation's ASDF e.g. using the `tools/install-asdf.lisp`
script from the ASDF source repository at
< http://gitlab.common-lisp.net/asdf/asdf >.


Introduction
------------

POIU is a modification of ASDF that may operate on your systems in parallel.
This version of POIU was designed to work with ASDF no earlier than specified.

POIU will notably compile each Lisp file in its own forked process,
in parallel with other operations (compilation or loading).
However, it will load FASLs serially as they become available.

POIU will only make a difference with respect to ASDF if the dependencies
are not serial (i.e. no difference for systems using `:serial t` everywhere).
You can however use Andreas Fuchs's `ASDF-DEPENDENCY-GROVEL` to autodetect
minimal dependencies from an ASDF system (or a set of multiple such).

POIU may speed up compilation by utilizing all CPUs of an SMP machine.
POIU may also reduce the memory pressure on the main (loading) process
by off-loading the compilation onto forked subprocesses.
POIU will enforce separation between compile- and load- time environments,
helping you detect when `:LOAD-TOPLEVEL` is missing in `EVAL-WHEN`'s,
as needed for incremental compilation even with vanilla ASDF.
POIU will also catch *some* missing dependencies as exist between the
files that it will happen to compile in parallel (but may not catch all
dependencies that may otherwise be missing from your system).

When a compilation fails in a parallel process, POIU will retry compiling
in the main (loading) process so you get the usual ASDF error behavior,
with a chance to debug the issue and restart the operation.

POIU was currently only made to work with SBCL, CCL and CLISP.
[NB: the CLISP port is somewhat less stable.]
Porting to another Lisp implementation that supports ASDF
should not be difficult.
When unable to fork because the implementation is unsupported,
or because multiple threads are currently in use,
POIU will fall back to compiling everything in the main process.

Warning to CCL users: you need to save a CCL image that doesn't start threads
at startup in order to use POIU (or anything that uses fork).
Watch [QITAB](https://common-lisp.net/project/qitab/)
for a package that does just that: `SINGLE-THREADED-CCL`.

To use POIU, (1) make sure `asdf.lisp` is loaded.
We require a recent enough ASDF; see specific requirement in [poiu.asd](poiu.asd).
Usually, you can just:
```
(require "asdf")
```

(2) configure ASDF's `SOURCE-REGISTRY` or its `*CENTRAL-REGISTRY*`,
then load POIU:
```
(asdf:load-system :poiu)
```

(3) POIU is active by default. You can just
```
(asdf:load-system :your-system)
```

and POIU will be used to compile it.
Once again, you may want to first use `asdf-dependency-grovel`
to minimize the dependencies in your system.

POIU was initially written by Andreas Fuchs in 2007
as part of an experiment funded by ITA Software, Inc.
It was subsequently modified by Francois-Rene Rideau at ITA Software,
who adapted POIU for use with XCVB in 2009,
wrote the CCL and CLISP ports, moved code from POIU to ASDF, and
eventually rewrote both of them together in a simpler way.
The original copyright and (MIT-style) licence of ASDF (below) applies to POIU.


Usage
-----

POIU overrides your ASDF 3's `asdf::*plan-class*`,
and thereafter all compilation goes through POIU by default.
Bind this variable back to `'asdf::sequential-plan` to restore the default,
and explicitly to `'asdf::parallel-plan` to go parallel again.
You can also explicitly pass a `:plan-class` parameter to `asdf:operate` & co,
or you can call the parallel-operate functions defined by POIU.

You can control how many processes POIU may fork at a time
by binding `asdf::*max-forks*`.
The default is the number of cpus on which the machine POIU was loaded,
which if resuming from a dumped image might not be the same as
the machine on which it is now running, so you may want to reset that variable
in e.g. uiop's image-restore hook.
You can recompute the number of processors on the current machine with:
`(poiu/fork:ncpus)`.
In case this function fails to find an answer, it returns NIL,
in which case POIU defaults the `*max-forks*` to 16.


Installation
------------

POIU 1.34 depends on the new plan-making internals of ASDF 3.3.0,
but for bug fix purposes, we recommend ASDF 3.3.2.2 or later.

To use POIU, just make sure you use a recent enough ASDF,
and in your build scripts, after you `(require "asdf")`
but before you build the rest of your software, include the line:

    (asdf:load-system "poiu")

It automatically will hook into `asdf::*plan-class*`,
though you can reset it.




Support
-------

The official web pages for POIU are:
    <http://common-lisp.net/project/qitab/>
    <http://cliki.net/poiu>

The proper mailing-lists on which to ask questions are
`asdf-devel` and `qitab-devel`, both on `common-lisp.net`.


Testing
-------

Before to test, you must download the test files, e.g. with
    mkdir -p ~/src/fare
    git checkout https://github.com/fare/bastiat.org ~/src/fare/bastiat.org
    sbcl --load ~/quicklisp/setup --eval '(ql:quickload :exscribe)' --quit

To run the test, use
    sh test.lisp


Determinism
-----------

POIU uses convergent parallelism by default to preserve some determinism, but
currently it's configured to be deterministic *given the incremental state*,
instead of deterministic *given the source only*,
which would be safer though slower.
TODO: make the latter the default.
