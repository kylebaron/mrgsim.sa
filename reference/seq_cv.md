# Generate a sequence based on coefficient of variation

Generate a sequence based on coefficient of variation

## Usage

``` r
seq_cv(point, cv = 30, n = 5, nsd = 2, digits = NULL)
```

## Arguments

- point:

  reference parameter value.

- cv:

  coefficient of variation.

- n:

  number of values to simulate in the sequence.

- nsd:

  number of standard deviations defining the range of simulated
  parameter values.

- digits:

  number of significant digits in the answer; if `NULL` (the default)
  all digits are retained.

## Examples

``` r
seq_cv(10)
#> [1]  5.488116  7.408182 10.000000 13.498588 18.221188

seq_cv(5, n = 10)
#>  [1] 2.744058 3.135445 3.582657 4.093654 4.677535 5.344696 6.107014 6.978062
#>  [9] 7.973349 9.110594
```
