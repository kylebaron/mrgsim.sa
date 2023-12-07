#' Select sensitivity runs from a sens_each object
#' 
#' @param x a `sens_each` object.
#' @param dv_name character names of dependent variables to select; can be a 
#' comma-separated string.
#' @param p_name character names of parameters to select; can be a 
#' comma-separated string.
#' 
#' @return 
#' The updated `sens_each` object is returned.
#' 
#' @examples
#' library(dplyr)
#' 
#' mod <- mrgsolve::house()
#' 
#' out1 <- mod %>% parseq_factor(CL,VC) %>% sens_each()
#' 
#' out2 <- select_sens(out1, dv_name = "CP,RESP", p_name = "CL")
#' 
#' 
#' @export
select_sens <- function(x, dv_name = NULL, p_name = NULL) {
  cl <- class(x)
  x <- as_tibble(x)
  if(!is.null(dv_name)) {
    dv_name <- cvec_cs(dv_name)
    x <- filter(x, dv_name %in% .env[["dv_name"]])
    if(nrow(x)==0) {
      msg <- "Could not find dv named `{dv_name}` in simulated data."
      abort(glue(msg))  
    }
    if(length(dv_name)==1) {
      x <- mutate(x, !!dv_name := .data[["dv_value"]])
    } 
  }
  if(!is.null(p_name)) {
    p_name <- cvec_cs(p_name)
    x <- filter(x, .data[["p_name"]] %in% .env[["p_name"]]) 
    if(nrow(x)==0) {
      msg <- "Could not find parameter named `{p_name}` in simulated data."
      abort(glue(msg))  
    }
  }
  structure(x, class = cl)
}

sens_names_to_factor <- function(x) {
  if("p_name" %in% names(x)) {
    x <- mutate(
      x, 
      p_name = factor(.data[["p_name"]], levels = unique(.data[["p_name"]]))
    )
  }
  if("dv_name" %in% names(x)) {
    x <- mutate(
      x, 
      dv_name = factor(.data[["dv_name"]], levels = unique(.data[["dv_name"]]))
    )
  }
  x
}

#' @rdname sens_fun
#' @name sens_each
#' @export
sens_each <- function(mod, idata = NULL, ...) {
  if(is.data.frame(mod@args[["data"]])) {
    ans <- sens_grid_data(
      mod, 
      data = mod@args[["data"]], 
      idata = NULL, 
      ...
    )
    return(ans)  
  }
  if(is.ev(e <- mod@args[["events"]])) {
    if(!is.null(e$ID)) {
      abort("Event objects cannot contain an ID column.")  
    }
  }
  if(!exists("sens_values", mod@args)) {
    abort("Parameter values must be selected first.")    
  }
  if(exists("idata_set", mod@args)) {
    abort("`idata_set` use is not allowed with this workflow.")    
  }
  if(!is.null(idata)) {
    abort("`idata` use is not allowed with this workflow.")
  }
  
  parlist <- mod@args[["sens_values"]] 
  mod <- clear_args(mod)
  
  ref <- p_mrgsim_(NULL, mod, ...)
  ref <- mutate(
    ref, 
    ref_value = .data[["dv_value"]], 
    dv_value = NULL, 
    ID = NULL
  )
  pars <- list_2_idata(parlist)
  dims <- vapply(parlist, length, 1L)
  out <- tibble(
    p_name = rep(names(dims), dims),
    p_value = unlist(parlist, use.names = FALSE),
    data = p_mrgsim(mod, pars,...)
  )
  out <- denest(out)
  out[["ID"]] <- NULL
  out <- left_join(out, ref, by = c("time", "dv_name"))
  out <- out[, unique(c("case", "time", names(out))), drop=FALSE]
  structure(out, class = c("sens_each", class(out)))
}

p_mrgsim <- function(mod, pars, ...) {
  mod@args[["idata_set"]] <- NULL
  ans <- lapply(pars, p_mrgsim_, mod = mod, ...)
  ans <- lapply(ans, split_id)
  ans <- flatten(ans)
  ans <- unname(ans)
  ans
}

p_mrgsim_ <- function(x,mod, ...) {
  ans <- mrgsim_df(mod, idata = x, ...) 
  ans <- pivot_longer(
    ans,
    cols = seq(3, ncol(ans)), 
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
    abort("Parameter values must be selected first.")    
  }
  if(exists("idata_set", mod@args)) {
    abort("`idata_set` use is not allowed with this workflow.")    
  }
  if(!is.null(idata)) {
    abort("`idata` use is not allowed with this workflow.")
  }
  parlist <- mod@args[["sens_values"]]
  mod <- clear_args(mod)
  pars <-list_2_idata(parlist)
  pars <- lapply(pars, split_id) 
  dims <- vapply(pars, length, 1L)
  pars <- flatten(pars)
  out <- tibble(
    p_name = rep(names(dims),dims),
    p_value = unlist(parlist,use.names = FALSE),
    data = d_mrgsim(mod, pars, data = data, ...)
  )
  out <- denest(out, keep_id = TRUE)
  class(out) <- c("sens_data",class(out))
  out
}

d_mrgsim <- function(mod, pars, data, ...) {
  mod@args[["idata_set"]] <- NULL
  lapply(pars, d_mrgsim_, mod = mod,  data = data, ...)  
}

d_mrgsim_ <- function(x, mod, data, ...) {
  mod <- param(mod, x)
  mrgsim_df(mod, data = data,  ...) 
}

#' Coerce sens_each output to data frame
#' 
#' @param x a `sense_each` object.
#' @param row.names not used.
#' @param optional not used.
#' @param ... not used.
#' @method as.data.frame sens_each
#' @keywords internal
#' @export
as.data.frame.sens_each <- function(x, row.names = NULL, optional = FALSE, 
                                    ...)  {
  as.data.frame(as_tibble(x))
}

#' @keywords internal
#' @export
as_tibble.sens_each <- function(x, row.names = NULL, optional = FALSE,
                                unnest = TRUE, ...)  {
  cl <- class(x)
  cl <- cl[cl != "sens_each"]
  class(x) <- cl
  x
}

#' Unnest a sens_each object
#' 
#' @param x a sens_each object.
#' @param keep_id if `FALSE` then the `ID` column is removed .
#' @return
#' `x` is returned after unnesting and possible modification.
#' @keywords internal
#' @export
denest <- function(x, keep_id = FALSE) {
  x <- structure(x, class = class(tibble()))
  x <- mutate(x, case = seq_len(nrow(x)))
  x <- unnest(x, cols = "data")  
  if(!isTRUE(keep_id)) x[["ID"]] <- NULL
  x[, unique(c("case", names(x))), drop = FALSE]
}

#' @keywords internal
#' @export
print.sens_each <- function(x,...) {
  print(as_tibble(x, unnest = FALSE))
}
