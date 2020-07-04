Sys.setenv("R_TESTS" = "")
library(testthat)
library(parseq)

test_check("parseq", reporter="summary")

