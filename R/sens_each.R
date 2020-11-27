#' Select sensitivity runs from a sens_each object
#' 
#' @param x a sens_each object
#' @param dv_name character names of dependent variables to select
#' @param p_name character names of parameters to select
#' 
#' @examples
#' library(dplyr)
#' 
#' mod <- mrgsolve::house()
#' 
#' out1 <- mod %>% parseq_factor(CL,VC) %>% sens_each()
#' 
#' out2 <- select_sens(out1, dv_name = "CP", p_name = "CV")
#' 
#' @export
select_sens <- function(x, dv_name = NULL, p_name = NULL) {
  x <- as_tibble(x)
  if(!is.null(dv_name)) {
    x <- filter(x, dv_name == .env[["dv_name"]])
    x <- rename(x, !!dv_name := .data[["dv_value"]])
    x[["dv_name"]] <-  NULL
  }
  if(!is.null(p_name)) {
    x <- filter(x, .env[["p_name"]] %in% .data[["p_name"]]) 
    x <- mutate(x, p_name = factor(.env[["p_name"]], levels = unique(.data[["p_name"]])))
  }
  x
}

#' @rdname sens_fun
#' @name sens_each
#' @export
sens_each <- function(mod, idata = NULL, ...) {
  if(is.data.frame(mod@args[["data"]])) {
    return(sens_grid_data(mod, data = mod@args[["data"]], idata = NULL, ...))  
  }
  if(!exists("sens_values", mod@args)) {
    stop("parameter values must be selected first")    
  }
  if(exists("idata_set", mod@args)) {
    stop("'idata_set' use is not allowed with this workflow")    
  }
  if(!is.null(idata)) {
    stop("'idata use' is not allowed with this workflow")
  }
  ref <- p_mrgsim_(NULL,mod,...)
  ref <- mutate(
    ref, 
    ref_value = .data[["dv_value"]], 
    dv_value = NULL, 
    ID = NULL
  )
  parlist <- mod@args[["sens_values"]] 
  pars <- list_2_idata(parlist)
  dims <- map_int(parlist,length)
  out <- tibble(
    p_name = rep(names(dims),dims),
    p_value = unlist(parlist,use.names = FALSE),
    data = p_mrgsim(mod, pars,...)
  )
  out <- denest(out)
  out[["ID"]] <- NULL
  out <- left_join(out, ref, by = c("time", "dv_name"))
  out <- out[, unique(c("case", "time",names(out))), drop=FALSE]
  structure(out, class = c("sens_each",class(out)))
}

p_mrgsim <- function(mod, pars, ...) {
  mod@args[["idata_set"]] <- NULL
  pars %>% 
    map(p_mrgsim_, mod = mod, ...) %>% 
    map(split_id) %>% 
    flatten() %>% 
    unname()
}

p_mrgsim_ <- function(x,mod, ...) {
  ans <- mrgsim_df(mod, idata = x, ...) 
  ans <- pivot_longer(
    ans,
    cols = seq(3,ncol(ans)), 
    names_to = "dv_name", 
    values_to = "dv_value"
  )
  names(ans)[2] <- "time"
  ans
}


#' @rdname sens_fun
#' @name sens_each_data
#' @export
sens_each_data <- function(mod, data, idata = NULL, ...) {
  mod@args[["data"]] <- NULL
  if(!exists("sens_values", mod@args)) {
    stop("parameter values must be selected first",call.=FALSE)    
  }
  if(exists("idata_set", mod@args)) {
    stop("'idata_set' use is not allowed with this workflow",call.=FALSE)    
  }
  if(!is.null(idata)) {
    stop("'idata' use is not allowed with this workflow",call.=FALSE)
  }
  parlist <- mod@args[["sens_values"]] 
  pars <-list_2_idata(parlist)
  pars <- map(pars,split_id) 
  dims <- map_int(pars,length)
  pars <- flatten(pars)
  out <- tibble(
    p_name = rep(names(dims),dims),
    p_value = unlist(parlist,use.names = FALSE),
    data = d_mrgsim(mod, pars, data = data, ...)
  )
  out <- denest(out, keep_id = TRUE)
  structure(out, class = c("sens_data",class(out)))
}

d_mrgsim <- function(mod, pars, data, ...) {
  mod@args[["idata_set"]] <- NULL
  map(pars, d_mrgsim_, mod = mod,  data = data, ...)  
}

d_mrgsim_ <- function(x, mod, data, ...) {
  mod <- param(mod,x)
  mrgsim_df(mod, data = data,  ...) 
}

#' Coerce sens_each output to data frame
#' 
#' @param x a `sense_each` object
#' @param row.names not used
#' @param optional not used
#' @param ... not used
#' @method as.data.frame sens_each
#' @keywords internal
#' @export
as.data.frame.sens_each <- function(x, row.names = NULL, optional = FALSE, ...)  {
  as.data.frame(as_tibble(x))
}

#' @keywords internal
#' @export
as_tibble.sens_each <- function(x, row.names = NULL, optional = FALSE,
                                unnest = TRUE, ...)  {
  cl <- class(x)
  cl <- cl[cl!="sens_each"]
  structure(x, class = cl)
}

#' Unnest a sens_each object
#' 
#' @param x a sens_each object
#' @param keep_id if `FALSE` then the `ID` column is removed 
#' @return
#' `x` is returned after unnesting and possible modification
#' @keywords internal
#' @export
denest <- function(x, keep_id = FALSE) {
  x <- structure(x, class = class(tibble()))
  x <- mutate(x, case = seq_len(nrow(x)))
  x <- unnest(x, cols = "data")  
  if(!isTRUE(keep_id)) x[["ID"]] <- NULL
  x[,unique(c("case", names(x))),drop=FALSE]
}

#' @keywords internal
#' @export
print.sens_each <- function(x,...) {
  print(as_tibble(x,unnest=FALSE))
}
