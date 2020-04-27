POIU: Parallel Operator on Independent Units
============================================

POIU is an ASDF extension that will parallelize your Common Lisp builds,
for some build speedup, both through parallelization and reduced GC.

POIU builds a precise and complete dependency graph,
based on which it schedules performing of actions by worker subprocesses.
This dependency graph can also be extracted and used for other purposes.


Version Compatibility
---------------------

POIU 1.34.1 reportedly passed its test of building and using Exscribe
with ASDF 3.3.4.1 on SBCL 2.0.3.62 on Linux x86-64.
Still, support for ASDF 3.3 is considered incomplete, with many spurious warnings,
as POIU is confused by ASDF's support for multiple phases of invocation in one session.

See TODO section at the end.
Please report issues you experience, and particularly so
if you can isolate minimal circumstances that trigger those issues.

For an older known-working combination, try POIU 1.31.1 and ASDF 3.1.7
-- which if your implementation comes with a more recent ASDF
(as checked by `(require :asdf) (asdf:asdf-version)`) may require
overriding your implementation's ASDF e.g. using the `tools/install-asdf.lisp`
script from the ASDF source repository at
< http://gitlab.common-lisp.net/asdf/asdf >.


Introduction
------------

POIU is a modification of ASDF that may `operate` on your systems in parallel.
Each version of POIU is designed to work with a matching version of ASDF;
it will not work on older versions, and
may or may not work on more recent versions.

POIU performs file-creating actions such as compilation of Lisp files
each in its own forked process, in parallel with other such operations.
On the other hand, in-image actions such as loading of FASLs happens serially
as the dependencies for these actions are completed.

POIU will only make a difference with respect to ASDF
if the dependencies are not serial. Thus,
there will be no behavioral difference within
systems that use `:serial t` everywhere.

You can however use Andreas Fuchs's
[`ASDF-DEPENDENCY-GROVEL`](https://gitlab.common-lisp.net/xcvb/asdf-dependency-grovel)
to autodetect minimal dependencies from an ASDF system (or a set of multiple such).

POIU may speed up compilation by utilizing all CPUs of an SMP machine.
POIU may also reduce the memory pressure on the main (loading) process
by off-loading the compilation onto forked subprocesses;
this could help reduce the performance hit of Garbage Collection (GC).
POIU will enforce separation between compile- and load- time environments,
helping you detect
[when `:LOAD-TOPLEVEL` is missing in `EVAL-WHEN`'s](https://fare.livejournal.com/146698.html),
as needed for incremental compilation even with vanilla ASDF.
POIU will also catch *some* missing dependencies as exist between the
files that it will happen to compile in parallel. But POIU will not catch all
dependencies that may otherwise be missing from some systems.

When a compilation fails in a parallel process, POIU will retry compiling
in the main (loading) process so you get the usual ASDF error behavior,
with a chance to debug the issue and restart the operation at your regular REPL.

POIU was currently only made to work with Allegro, CCL, CLISP and SBCL.
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
It was subsequently maintained by Francois-Rene Rideau at ITA Software,
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

POIU 1.34.1 depends on the new plan-making internals of ASDF 3.3.0,
but for bug fix purposes, we recommend ASDF 3.3.3 or later.

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

Before to test, you must download the test files, e.g. with:
```
    mkdir -p ~/src/fare
    git checkout https://github.com/fare/bastiat.org ~/src/fare/bastiat.org
    sbcl --load ~/quicklisp/setup --eval '(ql:quickload :exscribe)' --quit
```

To run the test, use:
```
    sh test.lisp
```

Determinism
-----------

POIU uses convergent parallelism by default to preserve some determinism, but
currently it's configured to be deterministic *given the incremental state*,
instead of deterministic *given the source only*,
which would be safer though slower.

TODO: make the latter the default?


TODO: Support Build Phases
--------------------------

ASDF 3.3 introduced a builtin notion of multiple build phases in a same session.
These phases properly model system-definition time dependencies,
typically using using `defsystem-depends-on`, though for backward-compatibility,
ASDF also recognizes manual calls to `load-system` or `operate` within a `.asd` file.
POIU needs to be updated to support this notion.

POIU uses mutable hash-tables to represent "the" dependency graph, and
currently copies that graph once at "the" start of the build.
To properly handle multiple build phases,
POIU may have to maintain two explicit distinct graphs:

  1. the graph of "all the dependencies",
     which only grows as more are discovered, and

  2. the graph for "all pending dependencies",
     which grows with discovery and shrinks when performing actions.

The latter graph is used to schedule actions, while the former can be used
to report progress, verify consistency, display dependencies, etc.
Because of the multiple build phases, you can't actually precompute the former
then copy it into the latter before you start `perform`'ing the plan;
instead, you must start both from an empty state, and compute them concurrently
as you both discover dependencies and perform those from earlier phases.

Note that due to how `defsystem-depends-on` dependencies work,
to even compute the graph, you need to perform all actions in all phases
except possibly the very last one.
You could imaginably cache the results of this graph off-image,
but there still needs be some image that builds all those systems
with matching source code versions before you may prime the cache
and later use it.

To display the graph, you could output `dot` using `CL-DOT`,
or some JSON data for use with JavaScript D3.
Ideally, you'd probably want to compute the maximum build phase depth,
then pick an according color scheme wherein nodes and dependency arrows
get a different color based on how deep a phase they are built at,
with extra width for a defsystem-depends-on dependency.
