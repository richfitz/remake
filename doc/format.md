# The makerfile format

**Warning: There are no validation tools yet apart from loading the file.  There also is not a formal specification.**

`maker` uses [yaml](http://yaml.org) for configuration.  This format is easy to read and write, though you do need to take care with indentation.

The format is logic-free: there are no conditionals, no loops, etc.  Instead the focus is on just declaring how pieces of a system link together.

Most makerfiles will include three sections:

```
packages: <list of packages to load>
sources: <list of files to include>
targets: <list of targets -- see below>
```

There are additional optional sections listed at the bottom of this document.

## Packages

The section `packages` takes a list of packages to load -- these are the packages that your project relies on.  They will be loaded when `maker` starts up.  To load the `MASS` and `lme4` packages you could write:

```
packages:
  - MASS
  - lme4
```

(You could also write `packages: [MASS, lme4]`, and if there is a single package you can write `packages: MASS`).  This section is also used to [automatically install dependencies](project_dependencies.md).  If your project does not depend on any external packges, this section can be ommited.

## Sources

The section `sources` contains code that needs to be loaded into `R` (i.e., via `source()`).  This needs is specified as a set of paths relative to the *current* directory.  If a directory is included in this list, then all files with extension `.R` or `.r` in the directory will be loaded.  So writing

```
sources:
  - mydirectory
  - mycode.R
```

Will load all the R code in the directory `mydirectory` and the code in `mycode.R`.  All objects in these files will be available to `maker`, but ideally they'll just contain functions and not data.

If a directory is given, do not rely on the sort order as this will likely be locale specific (if order is important then include the files one at a time).

## Targets

The `targets` section will take up most of the file.  Each entry is a "target": a stepping stone in the analysis (see [[concepts]] for more on this).  The simplest form of targets looks like:

```
target_name:
  command: function_name(arg1, arg2)
```

The name `target_name` needs to be unique within a file.  If it contains a slash ("/") or ends in an extension that looks like a file it will be treated as a file.  Otherwise it will be treated as the name of an R object.

Files are relative to R's working directory, or absolute.  Care should be taken with absolute paths though, as they will almost aways make a project specific to a particular computer.  Don't use windows-style backslashes: use unix style forward slashes (this may relax at some point to make these equivalent).

The `command` section is the recipe to make the target.  It's an R function (defined in a package you loaded or in the files you sourced) that takes arguments that are themselves also targets.  So here `arg` and `arg2` are targets that would be defined elsewhere in the makerfile.

For example, you might read in a csv file:

```yaml
data:
  command: read.csv("mydata.csv")
```

This rule would run the built-in function `read.csv`, reading in a file `"mydata.csv"` that will be found in the current directory.  The result of this will be stored within maker's database and will be available to other rules.  So elsewhere you might have:


```yaml
processed:
  command: clean_data(data)
```

which would run the function `clean_data` (defined in your code files) over the object `data` defined by the previous rule.  Note that the filenames are quoted but the object names are not - just like in R.

You may depend on files that do not have target, but this will generate a warning if they do not exist when the makerfile is loaded as this is usually a spelling mistake (the exception here is if a rule generates multiple files).  You may *not* depend on an object that does not have a target because there is no way of making this object.  In particular you may not depend on objects that are found in the global environment (why not?  Because they're not reproducible).

### Additional per-target keys:

There are optional keys that may also occur at the same level as `command:`.

`cleanup_level:` indicates how agressively maker will try to clean the target up, and may be one of `tidy`, `clean`, `purge` or `never` (the default is `tidy`).  See [here](cleanup) for more information.

One use of this option is if you are downloading a dataset that will not change or that is very large so you don't want to delete and redownload it often, you could write:

```yaml
data:
  command: download_data()
  cleanup_level: never
```

so that this object would never be deleted by maker.

`check:` indicates what should be checked to see if a target is up-to-date or not.  By default we check if any of the dependencies have changed or if the code has changed.  To check just the dependencies (and ignore changes to code) `check: depends`.  To check just the code and ignore changes in the dependencies, use `check: code`.  To check only that the target exists use `check: exists` (the default is `check: all`, but this never needs specifying directly).

`packages:` lists additional packages that need to be loaded for this target only.  This is useful if a target requires packages that are extremely slow to load or that are hard to compile.  `maker` will load the package immediately before running the command, and then unload the package after the target has built (along with any additional packages that these packages loaded).

`quiet:` is a logical (`TRUE` / `FALSE`) flag that indicates if printed output from targets should be suppressed.  If `TRUE` it suppresses the output of `cat`, `print` and `message`, but *not* `warning` (warnings will still be printed to the screen: to quieten these you'll need to use `suppressWarnings` in your code, or prevent the warning from happening in the first place).

### File targets

There is one extra wrinkle for file targets (i.e., targets that produce files).  Sometimes it will be useful to pass the target name into the command that is building you function.  Suppose you are downloading some data.  You might have a rule:

```yaml
  dest.html:
    command: download_data()
```

and associated code:

```r
download_data <- function() {
  download.file("http://google.com/index.html", "dest.html")
}
```

this downloads a file from the internet and saves it as `dest.html`.  However, this means that the filename occurs in two places.  If you want to move the file you need to update it in both of them.  This violates the principle of Dont Repeat Yourself, and will eventually cause problems.  To avoid this, there is a special variable that can be used within `command:`, called `target_name`.  We can rewrite the above as:

```yaml
  dest.html:
    command: download_data(target_name)
```

and associated code:

```r
download_data <- function(dest) {
  download.file("http://google.com/index.html", dest)
}
```

This will pass the name of the target into the function `download_data`.  If you rename `dest.html`, then the code does not need changing.

If this seems to weird, you can also write:

```yaml
  dest.html:
    command: download_data("dest.html")
```

which is equivalent (note the quotes).

### Plot targets

`plot:`: This is valid *only* for file targets.  It is also only valid if the extension of the file target is one of `pdf` or `png`.  It can take the value `true`, or it can be a set of key/value pairs corresponding to the device that is being opened (i.e., options to R's `pdf()` or `png()`).  In either case what happens when the targets is built:

1. The appropriate device is openned
2. The command is run, which creates a figure as a side effect (but which does not itself open a device)
3. The device is closed

If an error is thrown while creating the figure, the device will be closed automatically.

For example, you might have this code within a file listed in `sources:`

```r
myplot <- function(data) {
  plot(y ~ x, data, main="This is a plot")
}
```

The target definition:

```
  plot.pdf:
    command: myplot(data)
    plot: true
```

will be equivalent to first checking that `data` is up to date, and then running

```
pdf("plot.pdf")
myplot(data)
dev.off()
```

Alternatively, the target might be defined as

```
  plot.pdf:
    command: myplot(data)
    plot:
      width: 8
      height: 4
      family: Times
```

which would be equivalent to

```
pdf("plot.pdf", width=8, height=4, family="Times")
myplot(data)
dev.off()
```

If you repeatedly use the same block of options, you can define them as a set to reuse easily: see `plot_options:` below.

Other plotting devices are easy enough to support, I just have not done that yet.  I especially want to support devices from other packages, such as the `Cairo` package, which allow easy use of system fonts.

### Knitr targets

The option `knitr: true` indicates that a target is going to be generated by `knitr`.

Knitr targets are different to other target types in that they do not have a `command:` key (the command is going to be `knitr::knit()`.  Instead, a list of objects that will be exported to knitr is given, along with any paths of files that must also exist.  The target itself must have the extension `.md`, and there must be a file with the same name but extension `.Rmd`.  For example:

```
  myreport.md:
    knitr: true
    depends:
      - object1
      - object2
      - path/to/file.csv
```

This is a knitr target that will generate a report `myreport.md` from a file `myreport.Rmd` which must exist or be generated by some other target (this latter option is useful with [`sowsear`](https://github.com/richfitz/sowsear) or `knitr::spin`.  The objects `object1` and `object2` will be exported to `knitr` (so the script can use these objects as if they are in the global environment, from within the knitr document).  Similarly, because the file `path/to/file.csv` is depended on, it will exist by the time the knitr document is compiled.

### Fake targets

It's possible to create "fake" targets, that simply group other targets.  Suppose you have a number of figures `figure1.pdf`, `figure2.pdf`, etc.  You can make a target like this:

```
  figures:
    depends:
      - figure1.pdf
      - figure2.pdf
```

You can then "build" the `figures` target, but it does nothing other than make sure that all the figures are created.

## Other keys at the top level

### Default target

The `target_default` key is the "default target", if maker is run without a target name (similar to the way that `make` runs).  This key is optional, but if present the target *must* be included in the targets below.  If it is missing, but there is a target `all` within the `targets` section, then that is the default target (but `target_default` overrides this).  If neither is present then there is no default target, and a target must be provided for `maker` to run.

### Plot options

You can define repeatedly used plotting options in a section `plot_options:`.  For example:


```yaml
plot_options:
  mystyle:
    width: 8
    height: 4
```

would define a style `mystyle` that sets the width and height of a device.  You can then use that within a target by saying:

```
targets:
  plot.pdf:
    command: myplot(input_data)
    plot: mystyle
```

### Include other maker files

You can include additional files using the `include` key at the top level.  For example:

```yaml
include:
  - maker_part1.yml
  - maker_part2.yml
```

These files can include sections `sources:`, `packages:` and `targets:`, and even `include:` other files.  Any `target_default:` keys will be ignored though.
