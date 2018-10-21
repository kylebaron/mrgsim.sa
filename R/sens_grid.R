
##' @rdname sens_fun
##' @export
sens_grid <- function(mod, idata = NULL, ...) {
  if(is.data.frame(mod@args[["data"]])) {
    return(sens_grid_data(mod, data = mod@args[["data"]], idata = NULL, ...))  
  }
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
  out <- mutate(
    as_data_frame(pars), 
    data = split_id(out), 
    ID = NULL
  )  
  structure(out, class = c("sens_grid",class(out)))
}

##' @rdname sens_fun
##' @export
sens_grid_data <- function(mod, data, idata = NULL, ...) {
  mod@args[["data"]] <- NULL
  if(!exists("sens_values", mod@args)) {
    stop("Parameter values must be selected first.")    
  }
  if(exists("idata", mod@args)) {
    stop("idata_set use is not allowed with this workflow.")    
  }
  if(!is.null(idata)) {
    stop("idata use is not allowed with this workflow.")
  }
  parlist <- mod@args[["sens_values"]] 
  idata <- do.call(expand.grid,parlist) %>% mutate(ID = seq(n()))
  pars <- split_id(idata)
  out <- mutate(
    as_data_frame(idata), 
    ID = NULL,
    data = d_mrgsim(mod, pars, data = data, ...) 
  )
  structure(out, class = c("sens_data",class(out)))
}

##' @export
as.data.frame.sens_grid <- function(x, row.names = NULL, optional = FALSE, ...)  {
  unnest(mutate(x, .case = seq(n())))
}

##' @export
as.data.frame.sens_data <- function(x, row.names = NULL, optional = FALSE, ...)  {
  unnest(mutate(x, .case = seq(n())))
}

##' @export
as_data_frame.sens_grid <- function(x, row.names = NULL, optional = FALSE, ...)  {
  unnest(mutate(x, .case = seq(n())))
}

##' @export
as_data_frame.sens_data <- function(x, row.names = NULL, optional = FALSE, ...)  {
  unnest(mutate(x, .case = seq(n())))
}
