Sys.setenv("R_TESTS" = "")
library(testthat)
library(mrgsim.sa)

test_check("mrgsim.sa", reporter="summary")

