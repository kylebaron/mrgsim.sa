#' Run ad-hoc parameter sensitivity analyses with mrgsolve
#' 
#' @param mod a mrgsolve model object
#' @param par parameter names for sensitivity analysis; this can be 
#' a character vector or a comma-separated string (see examples)
#' @param var names of model output variables to include in simulated output; 
#' this could be the name of a compartment or another output derived inside
#' of the model (e.g. `DV` or `CP` or `logV`, but is specific to what is coded 
#' into `mod`)
#' @param method parameter sequence generation method
#' @param vary use `each` to vary one parameter at a time or `grid`
#' to vary all combinations of parameters
#' @param ... passed to `method` function
#' @param sargs a named list of arguments passed to [sens_each()] or 
#' [sens_grid()] and eventually
#' to [mrgsolve::mrgsim()]
#' 
#' @examples
#' mod <- mrgsolve::house()
#' 
#' dose <- mrgsolve::ev(amt = 100)
#' 
#' sens_run(
#'   mod, 
#'   par = "CL,VC", 
#'   method = "cv", 
#'   vary = "each", 
#'   sargs = list(events = dose)
#' )
#' 
#' @md
#' @export
sens_run <- function(mod, 
                     par = NULL, 
                     var = NULL,
                     method = c("factor", "cv", "range", "manual"),
                     vary = c("each", "grid"), ..., sargs = list()) {
  method <- match.arg(method)
  method <- paste0("parseq_", method)
  meth <- get(method, mode = "function")
  if(is.character(par)) {
    par <- cvec_cs(par)
    mod <-  select_par(mod,par)
  }
  if(is.character(var)) {
    var <- cvec_cs(var)
    mod <- update(mod, outvars = var)
  }
  mod <- meth(mod,...)
  vary <- match.arg(vary)
  simf <- ifelse(vary=="each", sens_each, sens_grid)
  assert_that(is.list(sargs))
  sargs$mod <- mod
  do.call(simf, sargs)
}

