# Changelog

## mrgsim.sa 0.3.0

- Add `palette` argument to
  [`sens_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_plot.md)
  for both `sens_each` and `sens_grid` output, allowing users to pass a
  discrete color scale (e.g., from ggplot2 or ggsci). The default
  palette uses lattice/trellis colors for small numbers of levels and
  `hcl.colors(palette = "Dark 2")` for larger sets
  ([\#13](https://github.com/kylebaron/mrgsim.sa/issues/13)).

- Add `group` and `facet` arguments to
  [`sens_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_plot.md)
  for
  [`sens_grid()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md)
  output to control which sensitivity variable is used for within-panel
  color grouping and which variable(s) appear in the facet strips.

- Add `xlab` and `ylab` arguments to
  [`sens_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_plot.md)
  for
  [`sens_grid()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md)
  output ([\#15](https://github.com/kylebaron/mrgsim.sa/issues/15)).

- `ylab` can now be a vector; the length must match the length of
  `dv_name` when multiple dependent variables are requested
  ([\#15](https://github.com/kylebaron/mrgsim.sa/issues/15)).

- Add `palette` argument to
  [`lsa_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/lsa.md)
  allowing users to pass a discrete color scale; the `pal` argument is
  deprecated; use `palette` instead.

### Bugs Fixed

- Fixed bug where sensitivity parameters were carried into the simulated
  output when running
  [`sens_grid()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md)
  and causing a naming conflict
  ([\#14](https://github.com/kylebaron/mrgsim.sa/issues/14)).

## mrgsim.sa 0.2.0

CRAN release: 2023-12-08

- [`sens_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_plot.md)
  for `sens_grid` output will now plot multiple dependent variables with
  a single call
  ([\#12](https://github.com/kylebaron/mrgsim.sa/issues/12)).

- [`sens_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_plot.md)
  for `sens_each` output will now plot the all items in `dv_name` by
  default ([\#7](https://github.com/kylebaron/mrgsim.sa/issues/7)).

- Add `layout` argument to
  [`sens_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_plot.md)
  to control how plots appear when multiple dependent variables are
  getting plotted
  ([\#7](https://github.com/kylebaron/mrgsim.sa/issues/7)).

- `NEWS.md` is now retained in the built package
  ([\#7](https://github.com/kylebaron/mrgsim.sa/issues/7)).

### Bugs Fixed

- Fixed bug that allowed non-integer values of `.n` getting passed into
  `parseq_` friends
  ([\#8](https://github.com/kylebaron/mrgsim.sa/issues/8)).

- Fixed warnings that were issued because extra arguments were getting
  passed through to
  [`mrgsim()`](https://mrgsolve.org/docs/reference/mrgsim.html)
  ([\#7](https://github.com/kylebaron/mrgsim.sa/issues/7)).

### Internal

- Now using
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html)
  globally ([\#8](https://github.com/kylebaron/mrgsim.sa/issues/8)).

## mrgsim.sa 0.1.0

CRAN release: 2020-11-30

- Initial version
