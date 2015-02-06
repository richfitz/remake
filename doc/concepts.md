# Concepts

## Targets

"Targets" are the main things that `remake` interacts with.  They represent things that are made (they're also the vertices of the [dependency graph](http://en.wikipedia.org/wiki/Dependency_graph)).  If you want to make a plot called `plot.pdf`, then that's a target.  If you depend on a dataset called `data.csv`, that's a target (even if it already exists).

There are several types of target:

* **files**: The name of a file target is the same as its path.  Something is actually stored in them, and it's possible for these to be modified outside of `remake`.  These are the main types of target that real `make` deals with.  With `remake` though, these should probably only be the beginning or end points of an analysis.  Within files, there are two sub-types:
  - *implicit*: these are file targets that are depended on somewhere in your process, but for which no rule exists.  You can't build these of course.  However, `remake` will build a target for them so we can monitor changes.
  - *plotting*: There are special plotting targets that help automate creating plots.
* **objects**: These are R objects that represent intermediate objects in an analysis.  However, these objects are transparently stored to disk so that they persist across R sesssions.  Unlike actual R objects though they won't appear in your workspace and a little extra work is required to get at them.
* **fake**: Fake targets are simply pointers to other targets (in `make` these are "phoney" targets).  Following longstanding conventions in `make`, it can be useful to define a fake target `all` that depends on all the "end points" of your analysis, so that running `make("all")` will build all of your targets, or check that they are up to date.
* **cleanup**: These are special sorts of fake targets used for cleaning.  These are `tidy`, `clean` and `purge`.

## Being up to date

`remake` tries to do as much work as possible, so that you can be as confident that things are up to date.  To do this it checks:

* That the dependencies all exist
* That the fingerprint of all dependencies are the same as last time the target was generated (using [SHA hash](http://en.wikipedia.org/wiki/SHA-1) for object targets, [MD5 hash](http://en.wikipedia.org/wiki/MD5) for file targets)
* That the code used in the rule is unchanged from last time the target was generated.  This checks the rule function itself, plus all functions that it calls from within the files listed in `sources`, plus the versions of all packages for functions that are not defined in your own code.  The code checking uses R's `deparse` so whitespace changes and commenting will not trigger rebuilds.
