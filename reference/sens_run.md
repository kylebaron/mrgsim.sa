# Run ad-hoc parameter sensitivity analyses with mrgsolve

Run ad-hoc parameter sensitivity analyses with mrgsolve

## Usage

``` r
sens_run(
  mod,
  par = NULL,
  var = NULL,
  method = c("factor", "cv", "range", "manual"),
  vary = c("each", "grid"),
  ...,
  sargs = list()
)
```

## Arguments

- mod:

  a mrgsolve model object.

- par:

  parameter names for sensitivity analysis; this can be a character
  vector or a comma-separated string (see examples).

- var:

  names of model output variables to include in simulated output; this
  could be the name of a compartment or another output derived inside of
  the model (e.g. `DV` or `CP` or `logV`, but is specific to what is
  coded into `mod`).

- method:

  parameter sequence generation method.

- vary:

  use `each` to vary one parameter at a time or `grid` to vary all
  combinations of parameters.

- ...:

  passed to `method` function.

- sargs:

  a named list of arguments passed to
  [`sens_each()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md)
  or
  [`sens_grid()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md)
  and eventually to
  [`mrgsolve::mrgsim()`](https://mrgsolve.org/docs/reference/mrgsim.html).

## Value

A tibble-like object with class `sens_each` or `sens_grid`, depending on
the value of `vary`.

## Examples

``` r
mod <- mrgsolve::house()

dose <- mrgsolve::ev(amt = 100)

sens_run(
  mod, 
  par = "CL,VC", 
  method = "cv", 
  vary = "each", 
  sargs = list(events = dose)
)
#> # A tibble: 24,200 × 7
#>     case  time p_name p_value dv_name dv_value ref_value
#>  * <int> <dbl> <chr>    <dbl> <chr>      <dbl>     <dbl>
#>  1     1     0 CL       0.549 GUT            0         0
#>  2     1     0 CL       0.549 GUT            0       100
#>  3     1     0 CL       0.549 CENT           0         0
#>  4     1     0 CL       0.549 CENT           0         0
#>  5     1     0 CL       0.549 RESP          50        50
#>  6     1     0 CL       0.549 RESP          50        50
#>  7     1     0 CL       0.549 DV             0         0
#>  8     1     0 CL       0.549 DV             0         0
#>  9     1     0 CL       0.549 CP             0         0
#> 10     1     0 CL       0.549 CP             0         0
#> # ℹ 24,190 more rows
```
