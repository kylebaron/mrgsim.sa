# Simulation helper to generate a sequence of parameters from a range

Simulation helper to generate a sequence of parameters from a range

## Usage

``` r
parseq_range(mod, ..., .n = 5, .geo = TRUE, .digits = NULL)
```

## Arguments

- mod:

  mrgsolve model object.

- ...:

  named parameter range vectors (minimum and maximum) for model
  parameters; each vector must have length 2 and names must correspond
  to model parameters.

- .n:

  number of values to simulate for each parameter sequence; passed to
  [`seq_geo()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_geo.md)
  as `n`.

- .geo:

  if `TRUE` generate a geometric sequence; otherwise, generate a
  sequence evenly spaced on Cartesian scale; see
  [`seq_geo()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_geo.md).

- .digits:

  if `numeric`, the number of significant digits in the parameter
  sensitivity values are set using
  [`base::signif()`](https://rdrr.io/r/base/Round.html).

## Details

Parameter range vectors passed via `...` will be sorted prior to
simulation.

## See also

[`parseq_cv()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_cv.md),
[`parseq_fct()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_fct.md),
[`parseq_manual()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_manual.md)

## Examples

``` r
mod <- mrgsolve::house()

mod %>%
  parseq_range(CL = c(0.5,1),VC = c(10,40)) %>% 
  sens_each()
#> # A tibble: 24,050 × 7
#>     case  time p_name p_value dv_name dv_value ref_value
#>  * <int> <dbl> <chr>    <dbl> <chr>      <dbl>     <dbl>
#>  1     1  0    CL         0.5 GUT            0         0
#>  2     1  0    CL         0.5 CENT           0         0
#>  3     1  0    CL         0.5 RESP          50        50
#>  4     1  0    CL         0.5 DV             0         0
#>  5     1  0    CL         0.5 CP             0         0
#>  6     1  0.25 CL         0.5 GUT            0         0
#>  7     1  0.25 CL         0.5 CENT           0         0
#>  8     1  0.25 CL         0.5 RESP          50        50
#>  9     1  0.25 CL         0.5 DV             0         0
#> 10     1  0.25 CL         0.5 CP             0         0
#> # ℹ 24,040 more rows
```
