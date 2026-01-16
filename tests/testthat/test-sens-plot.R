library(testthat)
library(mrgsim.sa)

context("test-sens-plot")

s1 <- 
  mrgsolve::house() %>% 
  ev(amt = 100) %>% 
  parseq_cv(CL, VC, KIN, KOUT) %>%
  sens_each(end = 12) 

s2 <- mrgsolve::house() %>% 
  ev(amt = 100) %>% 
  parseq_cv(CL, VC, KOUT) %>%
  sens_grid(end = 12) 

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

test_that("plot a sens_grid object", {
  out <- sens_run(house(), par = "CL,VC", vary = "grid") 
  expect_is(out, "sens_grid")
  expect_is(sens_plot(out, dv_name = "CP"), "gg")
  out2 <- sens_run(house(), par = "CL,VC,KA,IC50", vary = "grid")
  expect_error(sens_plot(out2, "CP"), regexp = "Too many ")
})

test_that("sens_grid - single plot", {
  p <- sens_plot(s2, "CP")
  expect_is(p, "gg")
  expect_error(sens_plot(s2, TRUE), "dv_name is not a character")
})

test_that("sens_grid - multiple plots", {
  p <- sens_plot(s2, "CP,RESP")
  expect_is(p, "list")
  expect_length(p, 2)
  expect_is(p[[1]], "gg")
})

test_that("sens_grid - multiple plots everything", {
  p <- sens_plot(s2)
  expect_is(p, "list")
  outv <- unique(s2$dv_name)
  expect_length(p, length(outv))
  expect_is(p[[1]], "gg")
})
