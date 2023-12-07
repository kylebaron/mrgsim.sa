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
  expect_error(parseq_range(mod, CL=c(1,2), .n = 0.6), "must be an integer")
  expect_error(
    parseq_range(mod, c(1,2)),
    "All parameter range vectors in ... must be named" 
  )
  expect_error(
    parseq_range(mod, CL = c(1)),
    "must have length 2" 
  )
  expect_error(
    parseq_range(mod, CLx = c(1,2)),
    "Some parameter names were not found in the model" 
  )
  expect_error(
    parseq_range(mod),
    "At least one" 
  )
})

test_that("parseq_manual", {
  expect_error(
    parseq_manual(mod, c(1,2)),
    "All parameter value vectors in ... must be named" 
  )
  expect_error(
    parseq_manual(mod, CLx = c(1,2)),
    "Some parameter names were not found in the model" 
  )
  expect_error(
    parseq_manual(mod),
    "At least one" 
  )
})
