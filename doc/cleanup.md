# Cleanup

There are several levels of cleanup that apply both through using the `cleanup` method and for targets.  They are:

* `tidy`: remove unimportant intermediate generated files
* `clean`: remove all generated files
* `purge`: **really** remove all generated files
* `never`: this target will never be removed by `maker`.

`clean` includes everything from `tidy` and `purge` includes everything from `clean`.

By default, all targets are by default `tidy`, so they'll be removed by any cleanup operation.  Targets with no rule (including implicit file targets), and fake targets all get the cleanup level `never`.

Targets can protected by adding the key:

```
  cleanup_level: <level>
```

within their target entry in the makerfile (at the same level as `rule` and `depends`:

```
target_name:
  depends: <list of target names>
  rule: function_name
  cleanup: <cleanup level>
```

The cleanup levels `tidy`, `clean` and `purge` are simply fake targets: if `m` is a `maker` object, they can be run as (for example):

```
m$make("clean")
```

# Hooks

Makerfile can also contain targets `tidy`, `clean` and `purge`.  However, these are treated specially:

* If dependencies are given, then dependencies are first satisfied (so pre-cleanup hooks can be run by depending on them)
* The cleanup is run at the requested level, as usual
* If a rule is given, then this is run as a post-cleanup hook

Hopefully this will not often be needed.
