# maker

[![Build Status](https://travis-ci.org/richfitz/maker.png?branch=master)](https://travis-ci.org/richfitz/maker)

Make-like build management for R

# Installation

Install using [devtools](https://github.com/hadley/devtools):

```r
devtools::install_github("richfitz/maker")
```

If you don't have devtools installed you will see an error "there is no package called 'devtools'"; if that happens install devtools with `install.packages("devtools")`.

If you work at the terminal (not Rstudio or Rgui) and would like coloured output, you might like to install [rainbowrite](https://github.com/richfitz/rainbowrite) with

```r
devtools::install_github("richfitz/rainbowrite")
```

maker depends on several R packages, all of which can be installed from CRAN.  The required packages are:

* [`R6`](http://cran.r-project.org/web/packages/R6) for holding things together
* [`yaml`](http://cran.r-project.org/web/packages/yaml) for reading the configuration
* [`digest`](http://cran.r-project.org/web/packages/digest) for efficiently hashing objects

```r
install.packages(c("R6", "yaml", "digest"))
```

In addition, there are several optional packages:

* [`optparse`](http://cran.r-project.org/web/packages/optparse) for a command line version (run from outside of an R session)
* [`igraph`](http://cran.r-project.org/web/packages/igraph) for creating a plots of the dependency graph

```r
install.packages(c("optparse", "igraph"))
```

# The idea

"[make](http://en.wikipedia.org/wiki/Make_(software))",
when it works, is wonderful.  Being able to change part of a complicated system and the re-make, updating only the parts of the system that have changed is great.  While it gets some use It's very heavily tailored towards building software though.  While make can be used to create reproducible research workflows (e.g. [here](http://www.bioinformaticszen.com/post/decomplected-workflows-makefiles/) and [here](http://kbroman.org/minimal_make/)), it is a challenge.

The idea here is to re-imagine a set of ideas from make but built for R.  Rather than having a series of calls to different instances of R (as happens if you run make on R scripts), the idea is to define pieces of a pipeline within an R session.

# A slightly more concrete example

Here's a very simple analysis pipeline that illustrates the basic problem:

1. Download some data from the web into a local file
2. Process that file to prepare it for analysis
3. Create a plot from that file

If you do this with `make` (but using R for all the actual actions) you might have a makefile that looks like this:

```make
all: plot.pdf
 
data.csv:
  Rscript download_data.R
 
processed.rds: data.csv
  Rscript process_data.R
 
plot.pdf: processed.rds
  Rscript plot_data.R
```

The R scripts need to know the names of the files that they will read and write.  Because it's traditionally been a pain to pass in command line arguments, I've assumed that the names are hard coded.  That's fine, but it leaves us with logic split between the makefile and the script files.

The great thing about this is if that the processed data changes (or if we depend on the processing script and *that* changes), we would rebuild the processed data and the plot, but we would not update the plot.  The workflow also becomes self documenting; we describe what needs to to happen but not the order, and we can be vague about how checking that things are up to date is done.

A more unixy way of doing this would be to write general *programs* in R so that we could write:

```
processed.rds: data.csv
  ./process_data.R --input data.csv --output processed.rds
```

or better still (and equivalently):

```
processed.rds: data.csv
  ./process_data.R --input $< --output $@
```

but this requires that the R scripts are written to take command line arguments.  There are some nice packages to that (e.g,, [optparse](http://cran.r-project.org/web/packages/optparse) and [docopt](http://cran.r-project.org/web/packages/docopt)) but this is not really the natural way to drive R.

## A more R-ish solution

A possible solution comes from replacing the **scripts** in the makefile with R **functions**.  The aim is to be able to write something like this (in [yaml](http://yaml.org), but I'm open to other formats):

```yaml
data.csv:
  rule: download_data
 
processed:
  depends:
    - data.csv
  rule: process_data
  
plot.pdf:
  depends:
    - processed
  rule: do_plot
```

In this example `download_data`, `process_data` and `do_plot` would be R functions.  There are two sorts of objects involved: `data.csv` and `plot.pdf` would be files (easy enough to determine by comparing against a set of known types, or by the presence of a forward slash), and `processed` would be an R object.

The functions could take arguments positionally or by name.  If dependencies are unnamed (like above) then we could just pass them in the order given.  So we'd run `process_data("data.csv")` and `do_plot(processed)` respectively (note that the first passes in a filename and the second passes in an object).  Alternatively, rather than relying on positional arguments we could pass keyword arguments.  To do that, we could write:

```
processed:
  depends:
    - filename: data.csv
  rule: process_data
```

with the idea that we'd end up evaluating `process_data(filename="data.csv")`.

# Examples

Real world examples coming soon.  In the meantime [this example](tests/testthat/maker_command.yml) gives a flavour of what maker tries to enable.  Here's a similar [configuration]((tests/testthat/knitr.yml) using [knitr](http://yihui.name/knitr).
