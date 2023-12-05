library(testthat)
library(mrgsim.sa)

context("test-sens-plot")

s1 <- 
  mrgsolve::house() %>% 
  ev(amt = 100) %>% 
  parseq_cv(CL, VC, KIN, KOUT) %>%
  sens_each(end = 12) 

test_that("select nothing", {
  a <- select_sens(s1)
  expect_identical(a, s1)
})

test_that("select dv", {
  a <- select_sens(s1, dv_name = "GUT,RESP")
  expect_equal(unique(a$dv_name), c("GUT", "RESP"))
  expect_equal(unique(a$p_name), c("CL", "VC", "KIN", "KOUT"))
  
  b <- mrgsim.sa:::sens_names_to_factor(a)
  expect_is(b$dv_name, "factor")
  expect_is(b$p_name, "factor")
})

test_that("select parameter name", {
  a <- select_sens(s1, p_name = "CL,KOUT")
  expect_equal(unique(a$p_name), c("CL", "KOUT"))
  expect_equal(unique(a$dv_name), unique(s1$dv_name))
  
  b <- mrgsim.sa:::sens_names_to_factor(a)
  expect_is(b$dv_name, "factor")
  expect_is(b$p_name, "factor")
})
