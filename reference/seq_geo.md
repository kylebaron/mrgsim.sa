# Generate a geometric sequence of parameter values

Generate a geometric sequence of parameter values

## Usage

``` r
seq_geo(from, to, n = 5, digits = NULL)
```

## Arguments

- from:

  passed to [`base::seq()`](https://rdrr.io/r/base/seq.html); must be
  numeric and positive.

- to:

  passed to [`base::seq()`](https://rdrr.io/r/base/seq.html); must be
  numeric and positive.

- n:

  passed to [`base::seq()`](https://rdrr.io/r/base/seq.html) as
  `length.out`.

- digits:

  number of significant digits in the answer; if `NULL` (the default)
  all digits are retained.

## Examples

``` r
seq_geo(from = 1, to = 10, n = 10)
#>  [1]  1.000000  1.291550  1.668101  2.154435  2.782559  3.593814  4.641589
#>  [8]  5.994843  7.742637 10.000000
```
