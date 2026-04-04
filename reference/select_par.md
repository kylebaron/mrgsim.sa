# Identify parameters in a model for sensitivity analysis

Identify parameters in a model for sensitivity analysis

## Usage

``` r
select_par(mod, ...)
```

## Arguments

- mod:

  an mrgsolve model object.

- ...:

  unquoted parameter names.

## Value

The model object with the selected parameters stored for use by
[`parseq_fct()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_fct.md),
[`parseq_cv()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_cv.md),
[`parseq_range()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_range.md),
or
[`parseq_manual()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_manual.md).

## Examples

``` r
mod <- mrgsolve::house()
select_par(mod, CL, VC)
#> 
#> 
#> --------------  source: housemodel.cpp  --------------
#> 
#>   project: /home/runner/wor...solve/project
#>   shared object: mrgsolve 
#> 
#>   time:          start: 0 end: 120 delta: 0.25
#>                  add: <none>
#>   compartments:  GUT CENT RESP [3]
#>   parameters:    CL VC KA F1 D1 WTCL WTVC SEXCL SEXVC
#>                  KIN KOUT IC50 WT SEX [14]
#>   captures:      DV CP [2]
#>   omega:         4x4 
#>   sigma:         1x1 
#> 
#>   solver:        rtol: 1e-08 atol: 1e-08 itol: 1 (scalar)
#> ------------------------------------------------------
```
