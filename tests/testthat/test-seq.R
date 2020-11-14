library(testthat)
library(mrgsim.sa)

context("test-seq")

test_that("even_seq", {
  a <- even_seq(1,10,4)
  b <- seq(1,10, length.out=4)
  expect_identical(a,b)
})

test_that("cv_seq", {
  a <- cv_seq(10,30,n = 10)
  
  sd <- sqrt((30/100)^2)
  from <- log(10)-2*sd
  to <- log(10)+2*sd
  b <- exp(seq(from,to, length.out=10))
  expect_length(b,10)
  expect_identical(a,b)
})

test_that("cv_seq", {
  a <- geo_seq(10,30,n = 10)
  b <- exp(seq(log(10), log(30),length.out = 10))
  expect_length(b, 10)
  expect_identical(a,b)
})
