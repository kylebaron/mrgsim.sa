Sys.setenv("R_TESTS" = "")
library(testthat)
library(parset)

test_check("parset", reporter="summary")

