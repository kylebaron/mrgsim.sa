# Select sensitivity runs from a sens_each object

Select sensitivity runs from a sens_each object

## Usage

``` r
select_sens(x, dv_name = NULL, p_name = NULL)
```

## Arguments

- x:

  a `sens_each` object.

- dv_name:

  character names of dependent variables to select; can be a
  comma-separated string.

- p_name:

  character names of parameters to select; can be a comma-separated
  string.

## Value

The updated `sens_each` object is returned.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

mod <- mrgsolve::house()

out1 <- mod %>% parseq_factor(CL,VC) %>% sens_each()

out2 <- select_sens(out1, dv_name = "CP,RESP", p_name = "CL")

```
