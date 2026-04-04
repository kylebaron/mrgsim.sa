# Generate a sequence of parameters

Generate a sequence of parameters

## Usage

``` r
parseq_fct(mod, ..., .n = 5, .factor = 2, .geo = TRUE, .digits = NULL)

parseq_factor(mod, ..., .n = 5, .factor = 2, .geo = TRUE, .digits = NULL)
```

## Arguments

- mod:

  mrgsolve model object.

- ...:

  unquoted parameter names.

- .n:

  number of parameters to simulate between the minimum and maximum
  parameter values.

- .factor:

  a numeric vector used to divide and multiply the parameter value thus
  generating the minimum and maximum parameter values, respectively, for
  the sequence; if `.factor` is length 1 it will be recycled to length
  2; the first value is used to divide the nominal value generating the
  minimum value; the second value is used to multiply the nominal value
  generating the maximum value.

- .geo:

  if `TRUE` a geometric sequence is generated (evenly spaced from min to
  max on log scale); otherwise, the sequence is evenly spaced on
  Cartesian scale.

- .digits:

  if `numeric`, the number of significant digits in the parameter
  sensitivity values are set using
  [`base::signif()`](https://rdrr.io/r/base/Round.html).

## Details

- `.n` is passed to
  [`seq_fct()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_fct.md)
  as `n`

- `.factor` is passed to
  [`seq_fct()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_fct.md)
  as `factor`

## See also

[`parseq_cv()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_cv.md),
[`parseq_range()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_range.md),
[`parseq_manual()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_manual.md)

## Examples

``` r
mod <- mrgsolve::house()

mod %>%
  parseq_fct(CL,VC) %>% 
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
