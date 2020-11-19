
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
  mod@args[["carry_out"]] <- NULL
  pars <- mod@args[["sens_values"]] 
  parsdf <- do.call(expand.grid,pars) 
  parsdf <- mutate(parsdf, ID = seq(n()), case = .data[["ID"]])
  ref <- mrgsim_df(mod, ...)
  ref <- pivot_longer(
    ref, 
    seq(3,ncol(ref)), 
    names_to = "dv_name", 
    values_to = "ref_value"
  )
  ref <- mutate(ref, ID = NULL)
  out <- mrgsim_df(mod, idata = parsdf, ...)
  out <- mutate(
    as_tibble(parsdf), 
    data = split_id(out), 
    ID = NULL
  ) 
  out <- denest(out)
  out <- pivot_longer(
    out, 
    seq(3+length(pars),ncol(out)), 
    names_to = "dv_name", 
    values_to = "dv_value"
  )
  out <- left_join(out, ref, by = c("time", "dv_name"))
  structure(out, class = c("sens_grid",class(out)), pars = pars)
}

#' @rdname sens_fun
#' @export
sens_grid_data <- function(mod, data, idata = NULL, ...) {
  mod@args[["data"]] <- NULL
  if(!exists("sens_values", mod@args)) {
    stop("parameter values must be selected first",call.=FALSE)    
  }
  if(exists("idata", mod@args)) {
    stop("idata_set use is not allowed with this workflow",call.=FALSE)    
  }
  if(!is.null(idata)) {
    stop("idata use is not allowed with this workflow",call.=FALSE)
  }
  vars <- flatten_chr(outvars(mod))
  assert_that(is.data.frame(data))
  mod@args[["carry_out"]] <- NULL
  parlist <- mod@args[["sens_values"]] 
  idata <- do.call(expand.grid,parlist) 
  idata <- mutate(idata, ID = seq(n()))
  pars <- split_id(idata)
  ref <- mrgsim_df(mod, data = data, ...)
  ref <- pivot_longer(
    ref, 
    all_of(vars), 
    names_to = "dv_name", 
    values_to = "ref_value"
  )
  ref <- select(ref, "ID", "time", "dv_name", "ref_value")
  ref <- mutate(ref, .N = seq(n()))
  out <- mutate(
    as_tibble(idata), 
    ID = NULL,
    data = d_mrgsim(mod, pars, data = data, ...) 
  )
  out <- denest(out, keep_id = TRUE)
  out <- pivot_longer(
    out, 
    all_of(vars),
    names_to = "dv_name", 
    values_to = "dv_value"
  )
  out <- mutate(out, .N = rep(ref$.N, length(pars)))
  out <- left_join(out, ref, by = c(".N", "ID", "dv_name", "time"))
  out$.N <- NULL
  structure(out, class = c("sens_data", class(out)))
}

#' @method as.data.frame sens_grid
#' @export
as.data.frame.sens_grid <- function(x, row.names = NULL, optional = FALSE, ...)  {
  as.data.frame(structure(x, class = class(tibble())))
}

#' @method as.data.frame sens_data
#' @export
as.data.frame.sens_data <- function(x, row.names = NULL, optional = FALSE, ...)  {
  as.data.frame(structure(x, class = class(tibble())))
}

#' @export
as_tibble.sens_grid <- function(x, row.names = NULL, optional = FALSE, ...)  {
  structure(x, class = class(tibble()))
}

#' @export
as_tibble.sens_data <- function(x, row.names = NULL, optional = FALSE, ...)  {
  structure(x, class = class(tibble()))
}
