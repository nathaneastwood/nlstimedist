# nlstimedist 2.0.0

This is a major update which overhauls the underlying code. A large focus has been placed on the reduction of dependencies. `{dplyr}` has been replaced with `{poorman}` which also removes the need for `{lazyeval}`; the `{broom}` package has been removed and so `{nlstimedist}` simply implements its own variant of the `glance()` and `augment()` functions directly; `{testthat}` has been replaced with `{tinytest}`.

The coding standards were also modernised and `{roxygen2}` markdown is now in place throughout the package.

# nlstimedist 1.1.4

* Update authors
