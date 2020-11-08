#' Run a sensitivity analysis
#' 
#' Use `sens_each` to examine sequences of parameters one 
#' at a time.  Use `sens_grid` to examine all combinations of 
#' sequences of parameters. 
#' 
#' @param mod a model object 
#' @param idata included only to prevent users from passing through; the 
#' function will create an idata set if appropriate
#' @param ... passed to [mrgsolve::mrgsim_d()]
#' @param data a simulation input data set (see [mrgsolve::data_set()])
#' 
#' @rdname sens_fun
#' @name sens_fun
NULL
