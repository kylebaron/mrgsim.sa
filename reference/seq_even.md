# Generate evenly spaced sequence

Generate evenly spaced sequence

## Usage

``` r
seq_even(from, to, n = 5, digits = NULL)
```

## Arguments

- from:

  passed to [`base::seq()`](https://rdrr.io/r/base/seq.html).

- to:

  passed to [`base::seq()`](https://rdrr.io/r/base/seq.html).

- n:

  passed to [`base::seq()`](https://rdrr.io/r/base/seq.html) as
  `length.out`.

- digits:

  number of significant digits in the answer; if `NULL` (the default)
  all digits are retained.

## Examples

``` r
seq_even(1, 10, 4)
#> [1]  1  4  7 10
```
