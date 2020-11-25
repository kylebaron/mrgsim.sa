library(testthat)
library(dplyr)
library(mrgsim.sa)

context("test-sens_grid")

mod <- mrgsolve::house()

test_that("sensitivity analysis on grid of values", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  out <- 
    mod %>% 
    parseq_cv(CL,VC) %>%
    sens_grid() 
  expect_is(out, "sens_grid")  
  expect_error(sens_grid(mod), msg = "parameter values must be selected")
  expect_error(
    mod %>% 
      parseq_cv(CL) %>%
      idata_set(tibble(a = 1)) %>% 
      sens_grid(), 
    msg = "idata_set use is not allowed"
  )
  expect_error(
    mod %>% 
      parseq_cv(CL) %>%
      idata_set(tibble(a = 1)) %>% 
      sens_grid(idata = tibble(a = 1)), 
    msg = "idata_set use is not allowed"
  )
})

test_that("sens_grid coerce output", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  outx <- mrgsim(mod, end = -1)
  out <- 
    mod %>% 
    parseq_cv(CL,VC) %>%
    sens_grid() 
  tb <- dplyr::as_tibble(out)
  expect_is(tb, "tbl_df")  
  expect_equal(
    names(tb), 
    c("case", "CL", "VC", "time", "dv_name", "dv_value", "ref_value")
  )
  df <- as.data.frame(out)
  expect_is(df, "data.frame")
})

test_that("sens grid data", {
  data <- mrgsolve:::expand.ev(amt = c(100,300))
  out <- 
    house() %>% 
    parseq_cv(CL, VC) %>%
    data_set(data) %>%
    sens_grid()
  expect_is(out, "sens_data")
  
})

test_that("sens_data coerce output", {
  data <- mrgsolve:::expand.ev(amt = c(100,300))
  outx <- mrgsim(mod, end = -1)
  out <- 
    mod %>% 
    parseq_cv(CL,VC) %>%
    sens_grid_data(data) 
  tb <- dplyr::as_tibble(out)
  expect_is(tb, "tbl_df")  
  expect_equal(
    names(tb), 
    c("case", "CL", "VC","ID", "time", "dv_name", "dv_value", "ref_value")
  )
  df <- as.data.frame(out)
  expect_is(df, "data.frame")
})

test_that("plot a sens_grid object", {
  out <- sens_run(house(), par = "CL,VC", vary = "grid") 
  expect_is(out, "sens_grid")
  expect_is(sens_plot(out, dv_name = "CP"), "gg")
  expect_error(sens_plot(out))
  out2 <- sens_run(house(), par = "CL,VC,KA,IC50", vary = "grid")
  expect_error(sens_plot(out2, "CP"), regexp = "found more than 3 parameters")
})
