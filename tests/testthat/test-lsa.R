library(testthat)
library(dplyr)
library(mrgsim.sa)

context("test-lsa")

mod <- mrgsolve::house()


test_that("lsa", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  out <- lsa(mod, par = "CL,VC", var = "CP")
  expect_is(out, "lsa")
  expect_is(out, "tbl_df")
  expect_equal(names(out), c("time", "var", "value", "par", "sens"))
  expect_equal(unique(out$var), "CP")
  expect_equal(unique(out$par), c("CL", "VC"))
})
