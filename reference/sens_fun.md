# Run an ad-hoc sensitivity analysis

Use `sens_each()` to examine sequences of parameters, one at a time. Use
`sens_grid()` to examine all combinations of sequences of parameters.
The `sens_each_data()` and `sens_grid_data()` variants allow you to pass
in a data set to simulate from.

## Usage

``` r
sens_each(mod, idata = NULL, ...)

sens_each_data(mod, data, idata = NULL, ...)

sens_grid(mod, idata = NULL, ...)

sens_grid_data(mod, data, idata = NULL, ...)
```

## Arguments

- mod:

  an mrgsolve model object (usually read in with
  [`mrgsolve::mread()`](https://mrgsolve.org/docs/reference/mread.html)).

- idata:

  included only to prevent users from passing through; the function will
  create an `idata_set` if appropriate.

- ...:

  passed to
  [`mrgsolve::mrgsim()`](https://mrgsolve.org/docs/reference/mrgsim.html).

- data:

  a simulation input data set (see
  [`mrgsolve::data_set()`](https://mrgsolve.org/docs/reference/data_set.html)).

## Value

A tibble-like object with class `sens_each` or `sens_grid`, depending on
the vary method that was used. These objects will look just like a
tibble, but they can be plotted with
[`sens_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_plot.md).

## See also

[`sens_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_plot.md)

## Examples

``` r
mod <- mrgsolve::house()

mod <- mrgsolve::ev(mod, amt = 100)

out_each <- parseq_cv(mod, CL, VC, .n = 3) %>% sens_each()

sens_plot(out_each, dv_name = "CP,RESP", layout = "facet_grid")


out_grid <- parseq_cv(mod, CL, VC) %>% sens_grid()

sens_plot(out_grid, dv_name = "CP")

```
