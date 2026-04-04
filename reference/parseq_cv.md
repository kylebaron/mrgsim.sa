# Generate a sequence of parameters based on CV

Generate a sequence of parameters based on CV

## Usage

``` r
parseq_cv(mod, ..., .cv = 30, .n = 5, .nsd = 2, .digits = NULL)
```

## Arguments

- mod:

  mrgsolve model object.

- ...:

  unquoted parameter names.

- .cv:

  a coefficient of variation used to determine range of test parameters.

- .n:

  number of parameters to simulate in the sequence.

- .nsd:

  number of standard deviations used to determine the range.

- .digits:

  if `numeric`, the number of significant digits in the parameter
  sensitivity values are set using
  [`base::signif()`](https://rdrr.io/r/base/Round.html).

## Details

- `.cv` is passed to
  [`seq_cv()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_cv.md)
  as `cv`

- `.n` is passed to
  [`seq_cv()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_cv.md)
  as `n`

- `.nsd` is passed to
  [`seq_cv()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_cv.md)
  as `nsd`

## See also

[`parseq_fct()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_fct.md),
[`parseq_range()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_range.md),
[`parseq_manual()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_manual.md)

## Examples

``` r
mod <- mrgsolve::house()

mod %>%
  parseq_cv(CL,VC) %>% 
  sens_each()
#> # A tibble: 24,050 × 7
#>     case  time p_name p_value dv_name dv_value ref_value
#>  * <int> <dbl> <chr>    <dbl> <chr>      <dbl>     <dbl>
#>  1     1  0    CL       0.549 GUT            0         0
#>  2     1  0    CL       0.549 CENT           0         0
#>  3     1  0    CL       0.549 RESP          50        50
#>  4     1  0    CL       0.549 DV             0         0
#>  5     1  0    CL       0.549 CP             0         0
#>  6     1  0.25 CL       0.549 GUT            0         0
#>  7     1  0.25 CL       0.549 CENT           0         0
#>  8     1  0.25 CL       0.549 RESP          50        50
#>  9     1  0.25 CL       0.549 DV             0         0
#> 10     1  0.25 CL       0.549 CP             0         0
#> # ℹ 24,040 more rows
```
