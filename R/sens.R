##' Run a sensitivity analysis
##' 
##' Use \code{sens_each} to examine sequences of parameters one 
##' at a time.  Use \code{sens_grid} to examine all combinations of 
##' sequences of parameters. 
##' 
##' @param mod a model object 
##' @param idata included only to prevent users from passing through; the 
##' function will create an idata set if appropriate
##' @param ... passed to \code{\link[mrgsolve]{mrgsim_df}}
##' 
##' @rdname sens_fun
##' @export
sens_each <- function(mod, idata = NULL, ...) {
  if(!exists("sens_values", mod@args)) {
    stop("Parameter values must be selected first.")    
  }
  if(exists("idata_set", mod@args)) {
    stop("idata_set use is not allowed with this workflow.")    
  }
  if(!is.null(idata)) {
    stop("idata use is not allowed with this workflow.")
  }
  parlist <- mod@args[["sens_values"]] 
  pars <- parlist %>% list_2_idata()
  dims <- map_int(parlist,length)
  out <- data_frame(
    name = rep(names(dims),dims),
    value = unlist(parlist,use.names = FALSE),
    data = p_mrgsim(mod, pars, ...)
  )
  structure(out, class = c("sens_each",class(out)))
}

p_mrgsim <- function(mod, pars, ...) {
  mod@args[["idata_set"]] <- NULL
  pars %>% 
    map(p_mrgsim_, mod = mod, ...) %>% 
    map(split_id) %>% 
    flatten()
}

p_mrgsim_ <- function(x,mod,...) {
  mrgsim_df(mod, idata = x, ...) 
}


split_id <- function(x) {
  split(x,x$ID)  
}


##' @rdname sens_fun
##' @export
sens_grid <- function(mod, idata = NULL, ...) {
  if(!exists("sens_values", mod@args)) {
    stop("Parameter values must be selected first.")    
  }
  if(exists("idata", mod@args)) {
    stop("idata_set use is not allowed with this workflow.")    
  }
  if(!is.null(idata)) {
    stop("idata use is not allowed with this workflow.")
  }
  pars <- mod@args[["sens_values"]] 
  pars <- do.call(expand.grid,pars) %>% mutate(ID = seq(n()))
  out <- mrgsim_df(mod, idata = pars, ...)
  out <- mutate(as_data_frame(pars), data = split_id(out), ID = NULL)  
  structure(out, class = c("sens_grid",class(out)))
}

##' @export
as.data.frame.sens_each <- function(x, row.names = NULL, optional = FALSE, ...)  {
  unnest(x)  
}
##' @export
as.data.frame.sens_grid <- function(x, row.names = NULL, optional = FALSE, ...)  {
  unnest(x)
}
