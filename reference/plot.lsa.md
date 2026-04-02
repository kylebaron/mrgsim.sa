# Plot a lsa object

Plot a lsa object

## Usage

``` r
# S3 method for class 'lsa'
plot(x, y = NULL, palette = NULL, pal = NULL, ...)
```

## Arguments

- x:

  output from
  [`lsa()`](https://kylebaron.github.io/mrgsim.sa/reference/lsa.md).

- y:

  not used.

- palette:

  a discrete color scale object, such as one returned by
  [`ggplot2::scale_color_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.html)
  or
  [`ggplot2::scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html).
  When `NULL` (default), a palette is chosen automatically based on the
  number of parameters.

- pal:

  **\[deprecated\]**; please use `palette` instead.

- ...:

  not used.

## Value

A ggplot.
