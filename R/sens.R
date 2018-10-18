##' Run a sensitivity analysis
##' 
##' Use \code{sens_each} to examine sequences of parameters one 
##' at a time.  Use \code{sens_grid} to examine all combinations of 
##' sequences of parameters. 
##' 
##' @param mod a model object 
##' @param ... passed to \code{\link[mrgsolve]{mrgsim_df}}
##' 
##' @rdname sens_fun
##' @name sens_fun
##' @export
sens_each <- function(mod, ...) {
  if(!exists("sens_values", mod@args)) {
    stop("Parameter values must be selected first.")    
  }
  pars <- mod@args[["sens_values"]] 
  pars <- pars %>% list_2_idata()
  p_mrgsim(mod,pars,...)    
}

##' @rdname sens_fun
##' @name sens_fun
##' @export
sens_grid <- function(mod, ...) {
  if(!exists("sens_values", mod@args)) {
    stop("Parameter values must be selected first.")    
  }
  pars <- mod@args[["sens_values"]] 
  pars <- do.call(expand.grid,pars) %>% mutate(ID = seq(n()))
  mod@args[["idata"]] <- NULL
  out <- mrgsim_df(mod, idata = pars, ...)
  mutate(out, name = "grid", value = 1) %>%
    left_join(pars, by = "ID")
}


p_mrgsim <- function(mod,pars, ...) {
  mod@args[["idata_set"]] <- NULL
  out <- map_df(pars, p_mrgsim_, mod = mod, ...)
  out  
}

p_mrgsim_ <- function(x,mod,...) {
  .name <- as.character(names(x)[2])
  mrgsim_df(mod, idata = x, ...) %>% 
    mutate(name = .name) %>% 
    left_join(set_names(x, c("ID", "value")),by="ID")
}

