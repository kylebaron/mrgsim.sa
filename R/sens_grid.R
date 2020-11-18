
#' @rdname sens_fun
#' @export
sens_grid <- function(mod, idata = NULL, ...) {
  if(is.data.frame(mod@args[["data"]])) {
    return(sens_grid_data(mod, data = mod@args[["data"]], idata = NULL, ...))  
  }
  if(!exists("sens_values", mod@args)) {
    stop("parameter values must be selected first.", call.=FALSE)    
  }
  if(exists("idata", mod@args)) {
    stop("idata_set use is not allowed with this workflow.", call.=FALSE)    
  }
  if(!is.null(idata)) {
    stop("idata use is not allowed with this workflow.", call.=FALSE)
  }
  pars <- mod@args[["sens_values"]] 
  pars <- do.call(expand.grid,pars) 
  pars <- mutate(pars, ID = seq(n()), case = ID)
  out <- mrgsim_df(mod, idata = pars, ...)
  out <- mutate(
    as_tibble(pars), 
    data = split_id(out), 
    ID = NULL
  )  
  structure(out, class = c("sens_grid",class(out)))
}

#' @rdname sens_fun
#' 
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
    as_tibble(idata), 
    ID = NULL,
    data = d_mrgsim(mod, pars, data = data, ...) 
  )
  structure(out, class = c("sens_data", class(out)))
}

#' @method as.data.frame sens_grid
#' @export
as.data.frame.sens_grid <- function(x, row.names = NULL, optional = FALSE, ...)  {
  as.data.frame(denest(x))
}

#' @method as.data.frame sens_data
#' @export
as.data.frame.sens_data <- function(x, row.names = NULL, optional = FALSE, ...)  {
  unnest(mutate(x, .case = seq(n())),cols="data")
}

#' @export
as_tibble.sens_grid <- function(x, row.names = NULL, optional = FALSE, ...)  {
  denest(x)
}

#' @export
as_tibble.sens_data <- function(x, row.names = NULL, optional = FALSE, ...)  {
  unnest(mutate(x, .case = seq(n())),cols="data")
}
