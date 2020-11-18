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
  expect_equal(names(out), c("CL", "VC", "case", "data"))
  expect_is(out, "sens_grid")  
  expect_error(sens_grid(mod), msg = "parameter values must be selected")
  expect_error(
    mod %>% 
      parseq_cv(CL) %>%
      idata_set(tibble(a = 1)) %>% 
      sens_grid(), 
    msg = "idata_set use is not allowed"
  )
  
})

test_that("sens_grid coerce output", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  out <- 
    mod %>% 
    parseq_cv(CL,VC) %>%
    sens_grid() 
  tb <- dplyr::as_tibble(out)
  expect_is(tb, "tbl_df")  
  df <- as.data.frame(out)
  expect_is(df, "data.frame")
})


