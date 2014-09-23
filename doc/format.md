# The makerfile format

**Warning: There are no validation tools yet apart from loading the file.  There also is not a formal spec.**

There are currently three sections to the makerfile, though this is subject to change.

```
packages: <list of packages to include>
sources: <list of files to include>
targets: <see below>
```

## Packages

The section `packages` takes a list of packages to load -- these are the packages that your project relies on.  They will be loaded when `maker` starts up.  To load the `MASS` and `lme4` packages you could write:

```
packages:
  - MASS
  - lme4
```

(You could also write `packages: [MASS, lme4]`, and if there is a single package you can write `packages: MASS`).

## Sources

The section `sources` contains code that needs to be loaded into `R` (i.e., via `source()`).  This needs is specified as a set of paths relative to the *current* directory.  If a directory is included in this list, then all files with extension `.R` or `.r` in the directory will be loaded.  So writing

```
sources:
  - mydirectory
  - mycode.R
```

Will load all the R code in the directory `mydirectory` and the code in `mycode.R`.  All objects in these files will be available to `maker`, but ideally they'll just contain functions and not data.

## Targets

The `targets` section will take up most of the file.  Each entry is a "target" - a stepping stone in the analysis (see [[docs_concepts]] on this).  The simplest form of targets looks like:

```
target_name:
  depends: <list of target names>
  rule: function_name
```

The order of keys (`depends`, `rule` here) is not important.

The name `target_name` needs to be unique within a file.  If it contains a slash or ends in an extension that looks like a file it will be treated as a file.  Otherwise it will be treated as the name of an R object.  Files are relative to R's working directory, or absolute.  Care should be taken with absolute paths though, as they will almost aways make a project specific to a particular computer.

The section `depends` lists targets that are required for this target.  These are therefore either files on the file system (relative to the current directory) or are R objects known to `maker`.  You cannot depend on R objects in the global environment (Why not?  Because they're not reproducible).

The section `rule` is an R function that will be run to build your target.  It will be passed in the names of all file targets and the contents of all object targets when it is run.  For example, this rule defines a target `myobject` (which is an *object* target), which depends on the file `data.csv` that will be found in the current directory, and which will be built using the function `myfunction`:

```
myobject:
  depends: data.csv
  rule: myfunction
```

When run, this will be roughly equivalent to running

```r
myobject <- myfunction("data.csv")
```

Another example:

```
another_object:
  depends:
    - a_file.txt
    - myobject
  rule: another_function
```

In this case there are two dependencies: a file called `a_file.txt` and the object that would be created by the previous rule.  This will be roughly equivalent to running:

```r
another_object <- another_function("a_file.txt", myobject)
```

Note that the *name* of the file is to the function, while the *contents* of the R object are passed along.
