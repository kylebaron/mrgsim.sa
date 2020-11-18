#' Run an ad-hoc sensitivity analysis
#' 
#' Use `sens_each()` to examine sequences of parameters one 
#' at a time.  Use `sens_grid()` to examine all combinations of 
#' sequences of parameters. The `sens_each_data()` and `sens_grid_data()` 
#' variants allow you to pass in a data set to simulate from.
#' 
#' @param mod an mrgsolve model object (usually read in with 
#' [mrgsolve::mread()])
#' @param idata included only to prevent users from passing through; the 
#' function will create an idata set if appropriate
#' @param ... passed to [mrgsolve::mrgsim_d()]
#' @param data a simulation input data set (see [mrgsolve::data_set()])
#' 
#' @examples
#' mod <- mrgsolve::house()
#' 
#' dose <- mrgsolve::ev(amt = 100)
#' 
#' out_each <- parseq_cv(mod, CL, VC) %>% sens_each()
#' 
#' out_grid <- parseq_cv(mod, CL, VC) %>% sens_grid()
#' 
#' @return
#' A tibble-like object with class `sens_each` or `sens_grid`, depending on the 
#' vary method that was used.  These objects will look just like a tibble, but 
#' they can be plotted with [sens_plot()].
#' 
#' @seealso [sens_plot()]
#' 
#' @rdname sens_fun
#' @name sens_fun
NULL
