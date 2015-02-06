# Managing project dependencies

*NOTE: This is subject to change.*

Dealing with project dependencies is a pain.  Telling other people how to deal with your project's dependencies is even more of a pain.  Projects like [packrat](rstudio.github.io/packrat) provide an extremely thorough solution to this problem and manage package *versions*, from diverse locations, keeping projects isolated from one another.  Remake will probably eventually use this.  In the mean time, it provides a lightweight automated dependency installation system.

Your remakefile already lists required packages in the `packages:` section.  From this, remake can arrange to install all **missing** packages by running

```
remake::make(install_packages")
```

This skips over packages that are already installed and does *not* enforce any version information.  This works without modification if all packages are available on CRAN.

If you depend on packages from GitHub or BitBucket, you can provide a little more metadata so that this works.  In another file called `remake_packages.yml` you can include sections like:


```
sowsear:
  source: github
  repo: richfitz/sowsear
```

The top level key is the package name.  The key `source:` can be `github`, `bitbucket`, `git` or `url`, corresponding to the [devtools](https://github.com/hadley/devtools) commands `install_github()`, etc.  The next key is the *first argument* of the corresponding function: `repo` for `github` and `bitbucket`, `url` for `url` and `git_url` for `git`.

If packages are listed in `remake_packages.yml`, then remake will install all non-installed CRAN packages *not* listed here, then install these extra packages from their special locations.  This assumes the extra packagesdon't have complicated dependencies (in particular one github package does not depend on a bitbucket package).  Packages are installed sequentially, but yaml ordering is not dependable so this is not very controllable.  It's not wonderful, but it should cover the most common use case.

## The future

The reason why `remake_packages.yml` is in its own package is that remake should be able to *generate* these, from devtools metadata.  This information is basically in packrat already, but not exposed.
