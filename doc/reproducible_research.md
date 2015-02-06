# The challenge of reproducible research

## Why not make?

I have used `make` on nontrivial projects (e.g., [how much of the world is woody](https://github.com/richfitz/woody) and [adequacy of models in comparative biology](https://github.com/richfitz/modeladequacy)).  All my R packages generally have a makefile for controlling the build and running tests (including `remake`!). While `make` can be used to create reproducible research workflows (e.g. [here](http://www.bioinformaticszen.com/post/decomplected-workflows-makefiles/) and [here](http://kbroman.org/minimal_make/)), it is a challenge.

### Platform independence

Getting `make` to work well on different platforms (especially Windows vs unix-like platforms) is a a major challenge.  Different platforms have different utilities installed (e.g., `curl` vs `wget`, GNU versions of tools vs POSIX versions, etc) and managing this dependency means either writing makefiles that are complicated and include conditionals, requiring manual editing of makefiles to specifiy programs/paths, etc, or a makefile *generator*, such as [cmake](http://www.cmake.org) or [autoconf](www.gnu.org/software/autoconf).

R provides a relatively homogenous interface across platforms, so much of this hassle should not be necessary.  Similarly, *not* using `make` makes it easier for people on Windows, or without a full toolchain, to take advantage of the approaches that `make` can offer.

### Simplicity

Makefiles are "[Turing complete](http://en.wikipedia.org/wiki/Turing_complete)" (see [here](http://stackoverflow.com/questions/3480950/are-makefiles-turing-complete)), which means they can become extremely difficult to understand without a full knowledge of them.  Because they contain an program you have to imagine the flow of the program to work out what might happen.  That's not how most makefiles are written in practice, but there's often a lot of complexity because there *can* be.

Here, we're taking the same approach as [mustache](http://mustache.github.io/) - as little logic as possible.  Describe what is going on and delegate the logic to the actual programming language.

### In research, the rules are themselves dependencies

In a research setting, the rules are usually dependencies.  Rather than stringing together known tools that work (which is how `make` is designed to be used), we often are writing analysis scripts that the targets should depend on.  This leads to contortions in the makefile syntax that are not hugely clear.  It's also easy to get the dependencies wrong.  `Make` is not language-aware, so in general the best you can do is depend on a script.  But if that script calls functions in another script you miss situations to where rebuilding a target is necessary (similarly, if functions in that file change that are unrelated to your target, or are comments/whitespace there is no need to rebuild).

`remake` solves this problem by automatically depending on the functions used as rules.  It inspects the functions and also depends on all the functions they call.  If functions are being called from a package, it depends on the package version -- if that changes the target should be rebuilt.  If functions are part of code you source into the project then any change that is not comments or whitespace to the rule function (or to functions it calls) triggers a rebuild.

There are limits to this of course: if your function calls `system()` or compiled code changes can be missed.

### Driving R from the command line

`make` works best when each rule generates a single output, by being run through a command.  This is not the natural way to work with R, and it's not particularly easy (or enjoyable) to write scripts in R that are designed to be called as standalone programs.
