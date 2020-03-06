#' Run a sensitivity analysis
#' 
#' Use \code{sens_each} to examine sequences of parameters one 
#' at a time.  Use \code{sens_grid} to examine all combinations of 
#' sequences of parameters. 
#' 
#' @param mod a model object 
#' @param idata included only to prevent users from passing through; the 
#' function will create an idata set if appropriate
#' @param ... passed to \code{\link[mrgsolve]{mrgsim_df}}
#' @param data a simulation input data set (see \code{\link[mrgsolve]{data_set}})
#' 
#' @rdname sens_fun
#' @name sens_fun
NULL