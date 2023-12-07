library(testthat)
library(mrgsim.sa)

context("test-parseq")
mod <- mrgsolve::house()

test_that("parseq_factor", {
  expect_error(parseq_fct(mod, CL, .n = 0.6), "must be an integer")
})

test_that("parseq_cv", {
  expect_error(parseq_cv(mod, CL, .n = 0.6), "must be an integer")
  expect_error(parseq_cv(mod, CL, .n = 2, .cv = TRUE), "must be numeric")
  expect_error(parseq_cv(mod, CL, .n = 2, .nsd = TRUE), "must be numeric")
})

test_that("parseq_range", {
  expect_error(parseq_range(mod, CL, .n = 0.6), "must be an integer")
})

