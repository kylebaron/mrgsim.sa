parseq
================

``` r
library(parseq)
```

``` r
mod <- mread("pk1", modlib(), end = 48, delta=0.1)

param(mod)
```

    . 
    .  Model parameters (N=3):
    .  name value . name value
    .  CL   1     | V    20   
    .  KA   1     | .    .

``` r
mod %>% 
  ev(amt = 100) %>% 
  parseq_factor(CL,V,.n=3) %>% 
  sens_each() %>% 
  plot_sens(CP)
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)
