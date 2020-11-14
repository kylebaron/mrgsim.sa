library(testthat)
library(dplyr)
library(mrgsim.sa)

context("test-mrgsens")

mod <- mrgsolve::house()

test_that("mrgsens - parseq factor", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  out1 <- mod %>% parseq_factor(CL,VC) %>% sens_each() 
  out2 <- mod %>%  mrgsens(par = "CL,VC",method = "factor")
  expect_equal(out1,out2)
})

test_that("mrgsens - parseq cv", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  out1 <- mod %>% parseq_cv(CL,VC) %>% sens_each() 
  out2 <- mod %>%  mrgsens(par = "CL,VC",method = "cv")
  expect_equal(out1,out2)
})

test_that("mrgsens - parseq range", {
  mod <- mrgsolve::ev(mod, mrgsolve::ev(amt = 100))
  out1 <- mod %>% parseq_range(CL = c(1,2)) %>% sens_each() 
  out2 <- mod %>%  mrgsens(par = "CL", CL = c(1,2), method = "range")
  expect_equal(out1,out2)
})
