POIU: Parallel Operator on Independent Units
============================================

POIU is an ASDF extension that will parallelize your Common Lisp builds,
for some build speedup, both through parallelization and reduced GC.

Usage
-----

POIU overrides your ASDF 3's `asdf::*default-plan-class*`,
and thereafter all compilation goes through POIU by default.
Bind this variable back to `'asdf::sequential-plan` to restore the default,
and explicitly to `'asdf::parallel-plan` to go parallel again.
You can also explicitly pass a `:plan-class` parameter to `asdf:operate` & co,
or you can call the parallel-operate functions defined by POIU.

You can control how many processes POIU may fork at a time my binding
	`asdf::*max-forks*`
The default is the number of cpus on which the machine POIU was loaded,
which if resuming from a dumped image might not be the same as
the machine on which it is now running.
You can recompute the number of processors on the current machine with:
	`(asdf::ncpus)`
In case this function fails to find an answer, it returns NIL,
in which case POIU defaults the `*max-forks*` to 16.


Installation
------------

Just make sure you use ASDF 3.0.2 or later, and include
      `(asdf:load-system :poiu)`
in your build scripts before you build the rest of your software.
It automatically will hook into `asdf::*default-plan-class*`,
though you can reset it.

POIU depends on the new plan-making internals of ASDF 3
as well as on ASDF 3's new UIOP library.


Support
-------

The official web pages for POIU are:
    <http://common-lisp.net/project/qitab/>
    <http://cliki.net/poiu>

The proper mailing-lists on which to ask questions are
`asdf-devel` and `qitab-devel`, both on `common-lisp.net`.


Determinism
-----------

POIU uses convergent parallelism by default to preserve some determinism, but currently
it's configured to be deterministic *given the incremental state*, instead of deterministic
*given the source only*, which would be safer though slower. TODO: make it the latter by default.
