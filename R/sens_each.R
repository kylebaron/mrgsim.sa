
#' @rdname sens_fun
#' @name sens_each
#' @export
sens_each <- function(mod, idata = NULL, ...) {
  if(is.data.frame(mod@args[["data"]])) {
    return(sens_grid_data(mod, data = mod@args[["data"]], idata = NULL, ...))  
  }
  if(!exists("sens_values", mod@args)) {
    stop("parameter values must be selected first.")    
  }
  if(exists("idata_set", mod@args)) {
    stop("'idata_set' use is not allowed with this workflow.")    
  }
  if(!is.null(idata)) {
    stop("'idata use' is not allowed with this workflow.")
  }
  parlist <- mod@args[["sens_values"]] 
  pars <- list_2_idata(parlist)
  dims <- map_int(parlist,length)
  out <- tibble(
    .name = rep(names(dims),dims),
    .value = unlist(parlist,use.names = FALSE),
    data = p_mrgsim(mod, pars, ...)
  )
  sim.ref <- mrgsim_df(mod, ...)
  sim.ref$ID <- 0
  ref <- tibble(.name = ".reference", .value = 0, data = list(sim.ref))
  structure(out, class = c("sens_each",class(out)),ref = ref)
}

p_mrgsim <- function(mod, pars, ...) {
  mod@args[["idata_set"]] <- NULL
  pars %>% 
    map(p_mrgsim_, mod = mod, ...) %>% 
    map(split_id) %>% 
    flatten() %>% 
    unname()
}

p_mrgsim_ <- function(x,mod,...) {
  mrgsim_df(mod, idata = x, ...) 
}


#' @rdname sens_fun
#' @name sens_each_data
#' @export
sens_each_data <- function(mod, data, idata = NULL, ...) {
  mod@args[["data"]] <- NULL
  if(!exists("sens_values", mod@args)) {
    stop("parameter values must be selected first.",call.=FALSE)    
  }
  if(exists("idata_set", mod@args)) {
    stop("'idata_set' use is not allowed with this workflow.",call.=FALSE)    
  }
  if(!is.null(idata)) {
    stop("'idata' use is not allowed with this workflow.",call.=FALSE)
  }
  parlist <- mod@args[["sens_values"]] 
  pars <-list_2_idata(parlist)
  pars <- map(pars,split_id) 
  dims <- map_int(pars,length)
  pars <- flatten(pars)
  out <- tibble(
    .name = rep(names(dims),dims),
    .value = unlist(parlist,use.names = FALSE),
    data = d_mrgsim(mod, pars, data = data, ...)
  )
  structure(out, class = c("sens_data",class(out)))
}

d_mrgsim <- function(mod, pars, data, ...) {
  mod@args[["idata_set"]] <- NULL
  map(pars, d_mrgsim_, mod = mod,  data = data, ...)  
}

d_mrgsim_ <- function(x, mod, data,...) {
  mod <- param(mod,x)
  mrgsim_df(mod, data = data,  ...) 
}

#' Coerce sens_each output to data frame
#' 
#' @param x a `sense_each` object
#' @param row.names not used
#' @param optional not used
#' @param ... not used
#' @export
as.data.frame.sens_each <- function(x, row.names = NULL, optional = FALSE, ...)  {
  as.data.frame(as_tibble(x))
}

#' @export
as_tibble.sens_each <- function(x, row.names = NULL, optional = FALSE,
                                unnest = TRUE, ...)  {
  cl <- class(x)
  cl <- cl[cl!="sens_each"]
  x <- structure(x, class = cl)
  if(unnest) x <- denest(x)
  x
}

#' Unnest a sens_each object
#' @param x a sens_each object
#' @export
denest <- function(x) {
  x <- mutate(x,.case=seq_len(nrow(x)))
  x <- x[,unique(c(".case", names(x))),drop=FALSE]
  unnest(x,cols="data")  
}

#' @export
print.sens_each <- function(x,...) {
  print(as_tibble(x,unnest=FALSE))
}
