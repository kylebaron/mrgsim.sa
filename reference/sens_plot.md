# Plot sensitivity analysis results

Plot sensitivity analysis results

## Usage

``` r
sens_plot(data, ...)

# S3 method for class 'sens_each'
sens_plot(
  data,
  dv_name = NULL,
  p_name = NULL,
  logy = FALSE,
  ncol = NULL,
  lwd = 0.8,
  digits = 3,
  plot_ref = TRUE,
  xlab = "time",
  ylab = NULL,
  layout = c("default", "facet_grid", "facet_wrap", "list"),
  grid = FALSE,
  palette = NULL,
  ...
)

# S3 method for class 'sens_grid'
sens_plot(
  data,
  dv_name = NULL,
  logy = FALSE,
  ncol = NULL,
  lwd = 0.8,
  digits = 2,
  plot_ref = TRUE,
  xlab = "time",
  ylab = dv_name,
  group = NULL,
  facet = NULL,
  palette = NULL,
  ...
)
```

## Arguments

- data:

  output from
  [`sens_each()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md)
  or
  [`sens_grid()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md).

- ...:

  arguments passed on to methods.

- dv_name:

  dependent variable names to plot; can be a comma-separated string; if
  `NULL`, then the unique values of `dv_name` in `data` are used.

- p_name:

  parameter names to plot; can be a comma-separates string.

- logy:

  if `TRUE`, y-axis is transformed to log scale

- ncol:

  passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- lwd:

  passed to
  [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html).

- digits:

  used to format numbers on the strips.

- plot_ref:

  if `TRUE`, then the reference case will be plotted in a black dashed
  line.

- xlab:

  x-axis title.

- ylab:

  y-axis title; not used for `facet_grid` or `facet_wrap` layouts.

- layout:

  specifies how plots should be returned when `dv_name` requests
  multiple dependent variables; see `Details`.

- grid:

  if `TRUE`, plots from the `sens_each` method will be arranged on a
  page with
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html);
  see the `ncol` argument.

- palette:

  a discrete color scale; like what you get from calling
  [`ggplot2::scale_color_discrete()`](https://ggplot2.tidyverse.org/reference/scale_colour_discrete.html).
  For `sens_each`, this is only applied when `grid = TRUE`; it is
  ignored for all other layouts, which use a continuous viridis color
  scale.

- group:

  sensitivity variable for within-panel grouping; defaults to the first
  sensitivity variable.

- facet:

  sensitivity variable for faceting when 3 sensitivity variables are
  being plotted; the `facet` variable will run left to right and the
  other variable will run up and down; this argument is ignored / not
  needed when there are fewer than 3 sensitivity variables.

## Value

A `ggplot` object when one `dv_name` is specified or a list of `ggplot`
objects when multiple `dv_name`s are specified.

## Details

The `layout` argument is only used for the `sens_each` method. It lets
you get the plots back in different formats when multiple dependent
variables are requested via `dv_name`.

- Use `default` to get the plots back in a list if multiple dependent
  variables are requested otherwise a single plot is returned.

- Use `facet_grid` to get a single plot, with parameters in columns and
  dependent variables in rows.

- Use `facet_wrap` to get a plot with faceted using
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html),
  with both the parameter name and the dependent variable name in the
  strip.

- Use `list` to force output to be a list of plots; this output can be
  further arranged using
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  if desired.

When `grid` is `TRUE`, a list of plots will be returned when multiple
dependent variables are requested.

## Examples

``` r
mod <- mrgsolve::house()

dose <- mrgsolve::ev(amt = 100)

out <- sens_run(
  mod, 
  sargs = list(events = dose),  
  par = "CL,VC"
) 

sens_plot(out, "CP")


out <- sens_run(
  mod, 
  sargs = list(events = dose), 
  par = "CL,VC", 
  vary  = "grid", 
  .n = 3
)

sens_plot(out, "CP")


sens_plot(out, "CP", group = "VC")


if(requireNamespace("ggsci")) {

  color <- ggsci::scale_color_atlassian()
  
  sens_plot(out, "CP", palette = color)

}
#> Loading required namespace: ggsci

```
