# Sensitivity Analysis with 'mrgsolve'

Perform local sensitivity analysis on ordinary differential equation
based models, including ad-hoc graphical analyses based on sequences of
parameters as well as local sensitivity analysis. Functions are provided
for creating inputs, simulating scenarios and plotting outputs.

## Details

- Local sensitivity analysis:
  [`lsa()`](https://kylebaron.github.io/mrgsim.sa/reference/lsa.md),
  [`lsa_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/lsa.md)

- Run ad-hoc sensitivity analyses:
  [`sens_each()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md),
  [`sens_grid()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md),
  [`sens_run()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_run.md)

  - Use
    [`sens_each_data()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md)
    and
    [`sens_grid_data()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md)
    to pass in data sets

- Parameter sequence generation:

  - In a pipeline:
    [`parseq_cv()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_cv.md),
    [`parseq_fct()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_fct.md),
    [`parseq_range()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_range.md),
    [`parseq_manual()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_manual.md)

  - Stand alone:
    [`seq_cv()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_cv.md),
    [`seq_fct()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_fct.md),
    [`seq_geo()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_geo.md),
    [`seq_even()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_even.md)

- Plot ad-hoc sensitivity analysis results

  - Use
    [`sens_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_plot.md)

- Select parameters or results

  - Use
    [`select_par()`](https://kylebaron.github.io/mrgsim.sa/reference/select_par.md)
    to choose which parameters to vary

  - Use
    [`select_sens()`](https://kylebaron.github.io/mrgsim.sa/reference/select_sens.md)
    to subset sensitivity analysis results

## See also

[`vignette("mrgsim.sa", package = "mrgsim.sa")`](https://kylebaron.github.io/mrgsim.sa/articles/mrgsim.sa.md)
for a complete tutorial.
