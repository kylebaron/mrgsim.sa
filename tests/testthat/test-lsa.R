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
  expect_equal(names(out), c("time", "dv_name", "dv_value", "p_name", "sens"))
  expect_equal(unique(out$dv_name), "CP")
  expect_equal(unique(out$p_name), c("CL", "VC"))
})

test_that("lsa input and output errors", {
  expect_error(lsa(mod, par = "a,b,c"), regex = "Invalid parameter") 
  expect_error(
    lsa(mod, par = "CL", var = "a,b,c"), 
    regex = "Invalid output name"
  ) 
  fun <- function(p, ...) {
    out <- mrgsim_df(mod)
    out$time <- NULL
    out
  }
  expect_error(
    lsa(mod, par = "CL", var = "CP", fun = fun), 
    regex = "Output from `fun` must contain"
  )
})

test_that("lsa plot", {
  out <- lsa(mod, par = "CL", var = "CP")
  ans <- lsa_plot(out)
  expect_is(ans, "gg")
})

test_that("lsa plot - pal is deprecated", {
  out <- lsa(mod, par = "CL", var = "CP")
  expect_warning(
    lsa_plot(out, pal = ggplot2::scale_color_brewer()),
    regex = "`pal` argument of `lsa_plot\\(\\)` is deprecated"
  )
})

test_that("lsa plot - custom palette", {
  out <- lsa(mod, par = "CL,VC", var = "CP")
  pal <- ggplot2::scale_color_brewer(palette = "Set1")
  ans <- lsa_plot(out, palette = pal)
  expect_is(ans, "gg")
})
