# Generate a sequence by fold increase and decrease from a point

Generate a sequence by fold increase and decrease from a point

## Usage

``` r
seq_fct(point, n = 5, factor = c(3, 3), geo = TRUE, digits = NULL)
```

## Arguments

- point:

  a numeric vector of length 1.

- n:

  number of elements in the sequence.

- factor:

  an integer vector of length 1 or 2; if length 1, values will be
  recycled to length 2; the first number used to divide `point` to
  generate the minimum value in the sequence; the second number is used
  to multiply `point` to generate the maximum value in the sequence.

- geo:

  if `TRUE`,
  [`seq_geo()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_geo.md)
  is used to generate the sequence; otherwise,
  [`seq_even()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_even.md)
  is used to generate the sequence.

- digits:

  number of significant digits in the answer; if `NULL` (the default)
  all digits are retained.

## Examples

``` r
seq_fct(10)
#> [1]  3.333333  5.773503 10.000000 17.320508 30.000000

seq_fct(10, n = 4, factor = 2)
#> [1]  5.000000  7.937005 12.599210 20.000000

seq_fct(10, n = 4, factor = 2, geo = TRUE)
#> [1]  5.000000  7.937005 12.599210 20.000000
```
