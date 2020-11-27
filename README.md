mrgsim.sa: sensitivity analysis with mrgsolve
================

A simple, clean workflow for sensitivity analysis with mrgsolve.

<hr>

<BR>

``` r
library(mrgsim.sa)
```

``` r
mod <- mread("pk1", modlib(), end = 48, delta = 0.1)
```

    . Building pk1 ... done.

``` r
param(mod)
```

    . 
    .  Model parameters (N=3):
    .  name value . name value
    .  CL   1     | V    20   
    .  KA   1     | .    .

## PK model sensitivity analysis by factor

The nominal (in model) parameter value is divided and multiplied by a
factor, generating minimum and maximum bounds for simulating a sequence
of parameter values

``` r
out <- 
  mod %>% 
  ev(amt = 100) %>% 
  select_par(CL, V) %>% 
  parseq_factor(.n=8) %>% 
  sens_each() 

sens_plot(out, "CP")
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

The simulated data is returned in a long format

``` r
out
```

    . # A tibble: 23,232 x 7
    .    case  time p_name p_value dv_name dv_value ref_value
    . * <int> <dbl> <chr>    <dbl> <chr>      <dbl>     <dbl>
    . 1     1     0 CL         0.5 EV             0         0
    . 2     1     0 CL         0.5 EV             0       100
    . 3     1     0 CL         0.5 CENT           0         0
    . 4     1     0 CL         0.5 CENT           0         0
    . 5     1     0 CL         0.5 CP             0         0
    . # … with 23,227 more rows

And you can plot with more informative color scale and legend

``` r
sens_plot(out, "CP", grid = TRUE)
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

## HIV viral dynamic model

We look at latent infected cell pool development over ten years at
different “burst” size, or the number of HIV particles released when one
cell lyses.

``` r
mod <- mread("hiv", "inst/example")
```

    . Building hiv ... done.

``` r
mod %>% 
  update(end = 365*10) %>%
  parseq_range(N = c(900,1500), .n = 10) %>%
  sens_each(tscale = 1/365) %>% 
  sens_plot("L", grid = TRUE)
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

## Sensitivity analysis on custom sequences

The model is rifampicin PBPK.

``` r
mod <- mread("inst/example/rifampicin.cpp") %>% update(delta = 0.1)
```

    . Building rifampicin_cpp ... done.

``` r
mod %>% 
  ev(amt = 600) %>% 
  parseq_manual(
    SFKp = seq_fct(.$SFKp, n = 20), 
    Kp_muscle = seq_even(0.001, 0.1, n = 6)
  ) %>% 
  sens_each() %>% 
  sens_plot("Ccentral")
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

# Simulate a grid

To this point, we have always used `sens_each` so that each value for
each parameter is simulated one at a time. Now, simulate the grid or all
combinations.

We use `parseq_cv` here, which generates lower and upper bounds for the
range using 50% coefficient of variation.

``` r
mod %>% 
  ev(amt = 600) %>% 
  parseq_cv(fBCLint_all_kg, .n = 7) %>% 
  parseq_cv(SFKp, Kp_muscle, .n = 3) %>% 
  sens_grid() %>% 
  sens_plot("Ccentral")
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

# Local sensitivity analysis

``` r
mod <- modlib("pk2", delta = 0.1, end = 72)
```

    . Building pk2 ... done.

``` r
doses <- ev(amt = 100)

out <- lsa(mod, var = "CP", par = "CL,V2,Q", events = doses)

lsa_plot(out, pal = NULL)
```

![](man/figures/README-unnamed-chunk-10-1.png)<!-- -->
