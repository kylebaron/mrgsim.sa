#' Run ad-hoc parameter sensitivity analyses with mrgsolve
#' 
#' @param mod a mrgsolve model object
#' @param par parameter names for sensitivity analysis
#' @param method parameter sequence generation method
#' @param type use `each` to vary one parameter at a time
#' @param ... passed to [sens_each]
#' 
#' @export
mrgsens <- function(mod, par, method = c("factor", "cv", "range", "manual"),
                    type = "each", ...) {
  method <- match.arg(method)
  method <- paste0("parseq_", method)
  meth <- get(method, mode = "function")
  par <- cvec_cs(par)
  mod <-  select(mod,par)
  mod <- meth(mod,...)
  type <- match.arg(type)
  simf <- ifelse(type=="each", sens_each, sens_grid)
  sens_each(mod,...)
}

