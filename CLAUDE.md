# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
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

This is an R package that adds sensitivity analysis workflows on top of [mrgsolve](https://mrgsolve.org/) ODE models. The core idea: attach parameter sequences to a model object, then simulate while varying those parameters.

### Source file load order (from DESCRIPTION Collate)

`utils.R` → `parseq.R` → `sens.R` → `AAA.R` → `lsa.R` → `sens_each.R` → `sens_grid.R` → `sens_plot.R` → `sens_run.R` → `seq.R`

### Key concepts

**Parameter sequences** (`parseq.R`, `seq.R`): Functions like `parseq_fct()`, `parseq_cv()`, `parseq_range()`, `parseq_manual()` attach a `sens_values` attribute to the mrgsolve model object. `seq_geo()`, `seq_fct()`, `seq_even()`, `seq_cv()` generate the underlying numeric sequences.

**Sensitivity simulation** (`sens_each.R`, `sens_grid.R`): `sens_each()` varies one parameter at a time (OAT); `sens_grid()` does full factorial combinations via `expand.grid()`. Both convert parameter lists to `idata` (individual-level data) and call mrgsolve's simulation engine. Output is a nested tibble with columns `p_name`, `.value`, and `data`.

**Visualization** (`sens_plot.R`): `sens_plot()` dispatches on `sens_each` or `sens_grid` class. Supports layouts: default (patchwork grid), `facet_grid`, `facet_wrap`, `list`. Reference case (nominal parameter values) plotted as black dashed line.

**Local sensitivity analysis** (`lsa.R`): `lsa()` perturbs each parameter by `eps` (default 1e-7) and computes finite-difference sensitivities. Output has its own `plot.lsa()` S3 method.

**Convenience wrapper** (`sens_run.R`): `sens_run()` accepts `method=` ("factor", "cv", "range", "manual") and `vary=` ("each", "grid") to combine parseq + sens in one call.

### Typical workflow

```r
mod %>%
  select_par(CL, VC) %>%     # tidyselect on model parameters
  parseq_fct(.n = 8) %>%     # attach sequences (factor method)
  sens_each() %>%             # OAT simulation → nested tibble
  sens_plot("CP")             # visualize dependent variable
```

### S3 classes

- `sens_each` / `sens_grid` / `sens_data`: output of simulation functions; have `as.data.frame()`, `as_tibble()`, `print()`, and `sens_plot()` methods
- `lsa`: output of `lsa()`; has `plot()` method

### Example models

`inst/example/` contains three mrgsolve `.cpp` model files (`hiv.cpp`, `gcsf.cpp`, `rifampicin.cpp`) used in tests and documentation.
