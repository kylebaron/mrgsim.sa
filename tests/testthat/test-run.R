library(testthat)

context("test-utils")
library(dplyr)
mod <- mrgsolve:::house()

test_each <- function(object) {
  a <- identical(names(object),c("name", "value", "data"))
  cl <- purrr::map_chr(object,class)
  b <- identical(cl,c(name="character", value="numeric", data="list"))
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
  
  out <- 
    mod %>% 
    ev(amt = 100) %>%
    parseq_factor(CL,VC) %>%
    sens_each() 
  
  expect_each(out)
  
})

