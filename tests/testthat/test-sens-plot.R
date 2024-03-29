library(testthat)
library(mrgsim.sa)

context("test-sens-plot")

s1 <- 
  mrgsolve::house() %>% 
  ev(amt = 100) %>% 
  parseq_cv(CL, VC, KIN, KOUT) %>%
  sens_each(end = 12) 

test_that("by default, plot everything", {
  p <- sens_plot(s1)  
  expect_is(p, "list")
  expect_length(p, length(unique(s1$dv_name)))
})

test_that("single plot - default", {
  p <- sens_plot(s1, "CP")
  expect_is(p, "gg")  
})

test_that("single plot - list layout", {
  p <- sens_plot(s1, "CP", layout = "list")
  expect_is(p, "list")
  expect_length(p, 1)
  expect_is(p[[1]], "gg")
})

test_that("single plot - facet_grid layout", {
  p <- sens_plot(s1, "CP", layout = "facet_grid")
  expect_is(p, "gg")
})

test_that("single plot - facet_wrap layout", {
  p <- sens_plot(s1, "CP", layout = "facet_wrap")
  expect_is(p, "gg")
})

test_that("single plot - grid", {
  p <- sens_plot(s1, "CP", grid = TRUE)
  expect_is(p, "gg")
})

test_that("multiple plots - default", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP") 
  expect_is(p, "list")
  expect_length(p, 3)
  expect_is(p[[2]], "gg")
})

test_that("multiple plots - list", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP", layout = "list") 
  expect_is(p, "list")
  expect_length(p, 3)
  expect_is(p[[2]], "gg")
})

test_that("multiple plots - facet_wrap", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP", layout = "facet_wrap") 
  expect_is(p, "gg")
})

test_that("multiple plots - facet_grid", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP", layout = "facet_grid") 
  expect_is(p, "gg")
})

test_that("multiple plots - facet_grid", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP", layout = "facet_grid") 
  expect_is(p, "gg")
})

test_that("multiple plots - grid", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP", layout = "facet_grid", grid = TRUE) 
  expect_is(p, "list")
  expect_length(p, 3)
  expect_is(p[[2]], "gg")
})
