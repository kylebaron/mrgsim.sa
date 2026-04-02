# Simulation helper to manually specify parameter sequences

Simulation helper to manually specify parameter sequences

## Usage

``` r
parseq_manual(mod, ...)
```

## Arguments

- mod:

  mrgsolve model object.

- ...:

  named numeric vectors of parameter values to simulate; names must
  correspond to parameters in the model object.

## Details

Parameter value vectors passed via `...` will be sorted prior to
simulation.

## See also

[`parseq_cv()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_cv.md),
[`parseq_range()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_range.md),
[`parseq_fct()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_fct.md)

## Examples

``` r
mod <- mrgsolve::house()

mod %>%
  parseq_manual(CL = c(0.5, 1, 1.5)) %>% 
  sens_each()
#> # A tibble: 7,215 × 7
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
#> # ℹ 7,205 more rows
```
