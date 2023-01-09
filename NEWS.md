
# rui 0.2.0

* Added a "Get started" vignette.
* New `console()` API as alternative to the verb-based API.
* Added a custom {cli} theme that still behaves well in literate computing.
* Added a `NEWS.md` file to track changes to the package.
* Changed internals to catch up with {cli} developments.

# rui 0.1.0

* New `python()` function to enable calling {rui} functions from Python, through
  {reticulate}, by using dot notation access to dictionary attributes.
* Deprecated original function names `title()`, `update()`, `end()`, `copy()`,
  `do()`, `stop()`, `extract()`, and `show()`, in favour of `entitle()`,
  `proceed()`, `clear()`, `give()`, `suggest()`, `error()`, `expose()`, and
  `display()` for Python compatibility and avoiding conflicts.