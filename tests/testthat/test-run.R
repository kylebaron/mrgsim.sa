library(testthat)
library(dplyr)
library(mrgsim.sa)

context("test-run")

mod <- mrgsolve::house()

test_each <- function(object) {
  a <- identical(names(object),c("p_name", ".value", "data"))
  cl <- purrr::map_chr(object,class)
  b <- identical(cl,c(.name="character", .value="numeric", data="list"))
  c <- is.tbl(object)
  all(a,b,c)
}

expect_each <- function(object, n) {
  act <- quasi_label(rlang::enquo(object))
  expect(
    test_each(object),
    sprintf("%s is not valid test_each output", act$lab)
  )
  invisible(act$val)
}

test_that("parseq factor", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  out <- 
    mod %>% 
    parseq_factor(CL,VC) %>%
    sens_each() 
  expect_is(out, "sens_each")
})

test_that("parseq cv", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  out <- 
    mod %>% 
    parseq_cv(CL,VC) %>%
    sens_each() 
  expect_is(out, "sens_each")
})

test_that("parseq manual", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  out <- 
    mod %>% 
    parseq_manual(
      CL = c(0.5, 1, 1.5),
      VC = c(10,20,30)
    ) %>%
    sens_each() 
  expect_is(out, "sens_each")
})

test_that("parseq range", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  out <- 
    mod %>% 
    parseq_range(
      CL = c(0.5, 1.5),
      VC = c(10, 30)
    ) %>%
    sens_each() 
  expect_is(out, "sens_each")
})

test_that("plot parseq output", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  out <- 
    mod %>% 
    update(end = 24) %>%
    parseq_range(
      CL = c(0.5, 1.5)
    ) %>% sens_each() %>% sens_plot("CP")
  expect_is(out, "gg")
})


