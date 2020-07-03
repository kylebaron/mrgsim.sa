#' Run ad-hoc parameter sensitivity analyses with mrgsolve
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

