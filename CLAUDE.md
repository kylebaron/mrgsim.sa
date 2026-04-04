# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Commands

``` bash
# Run tests
make test
# or: Rscript -e "testthat::test_dir('tests/testthat')"

# Run a single test file
Rscript -e "testthat::test_file('tests/testthat/test-sens_each.R')"

# Generate documentation (roxygen2)
make doc
# or: Rscript -e 'devtools::document(".")'

# Build package (no vignettes)
make build

# Install from built tarball
make install

# Full check (no vignettes)
make check

# Code coverage
make covr

# Render README
make readme
```

## Architecture

This is an R package that adds sensitivity analysis workflows on top of
[mrgsolve](https://mrgsolve.org/) ODE models. The core idea: attach
parameter sequences to a model object, then simulate while varying those
parameters.

### Source file load order (from DESCRIPTION Collate)

`utils.R` → `parseq.R` → `sens.R` → `AAA.R` → `lsa.R` → `sens_each.R` →
`sens_grid.R` → `sens_plot.R` → `sens_run.R` → `seq.R`

### Key concepts

**Parameter sequences** (`parseq.R`, `seq.R`): Functions like
[`parseq_fct()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_fct.md),
[`parseq_cv()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_cv.md),
[`parseq_range()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_range.md),
[`parseq_manual()`](https://kylebaron.github.io/mrgsim.sa/reference/parseq_manual.md)
attach a `sens_values` attribute to the mrgsolve model object.
[`seq_geo()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_geo.md),
[`seq_fct()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_fct.md),
[`seq_even()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_even.md),
[`seq_cv()`](https://kylebaron.github.io/mrgsim.sa/reference/seq_cv.md)
generate the underlying numeric sequences.

**Sensitivity simulation** (`sens_each.R`, `sens_grid.R`):
[`sens_each()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md)
varies one parameter at a time (OAT);
[`sens_grid()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_fun.md)
does full factorial combinations via
[`expand.grid()`](https://rdrr.io/r/base/expand.grid.html). Both convert
parameter lists to `idata` (individual-level data) and call mrgsolve’s
simulation engine. Output is a nested tibble with columns `p_name`,
`.value`, and `data`.

**Visualization** (`sens_plot.R`):
[`sens_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_plot.md)
dispatches on `sens_each` or `sens_grid` class. Supports layouts:
default (patchwork grid), `facet_grid`, `facet_wrap`, `list`. Reference
case (nominal parameter values) plotted as black dashed line.

**Local sensitivity analysis** (`lsa.R`):
[`lsa()`](https://kylebaron.github.io/mrgsim.sa/reference/lsa.md)
perturbs each parameter by `eps` (default 1e-7) and computes
finite-difference sensitivities. Output has its own
[`plot.lsa()`](https://kylebaron.github.io/mrgsim.sa/reference/plot.lsa.md)
S3 method.

**Convenience wrapper** (`sens_run.R`):
[`sens_run()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_run.md)
accepts `method=` (“factor”, “cv”, “range”, “manual”) and `vary=`
(“each”, “grid”) to combine parseq + sens in one call.

### Typical workflow

``` r
mod %>%
  select_par(CL, VC) %>%     # tidyselect on model parameters
  parseq_fct(.n = 8) %>%     # attach sequences (factor method)
  sens_each() %>%             # OAT simulation → nested tibble
  sens_plot("CP")             # visualize dependent variable
```

### S3 classes

- `sens_each` / `sens_grid` / `sens_data`: output of simulation
  functions; have
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
  `as_tibble()`, [`print()`](https://rdrr.io/r/base/print.html), and
  [`sens_plot()`](https://kylebaron.github.io/mrgsim.sa/reference/sens_plot.md)
  methods
- `lsa`: output of
  [`lsa()`](https://kylebaron.github.io/mrgsim.sa/reference/lsa.md); has
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method

### Example models

`inst/example/` contains three mrgsolve `.cpp` model files (`hiv.cpp`,
`gcsf.cpp`, `rifampicin.cpp`) used in tests and documentation.
