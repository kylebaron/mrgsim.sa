#' @include utils.R
NULL

save_sens_values <- function(mod, values) {
  if(!is.list(mod@args[["sens_values"]])) {
    mod@args[["sens_values"]] <- list()
  }
  mod@args[["sens_values"]] <- combine_list(mod@args[["sens_values"]], values)
  mod
}

#' Generate a sequence of parameters
#' 
#' @param mod a model object
#' @param ... unquoted parameter names
#' @param .n number of parameters to simulate between the minimum
#' and maximum parameter values
#' @param .factor a numeric vector used to divide and multiply the 
#' parameter value thus generating the minimum and maximum parameter values,
#' respectively, for the sequence; if `.factor` is length 1 it will be 
#' recycled to length 2; the first value is used to divide the nominal 
#' value generating the minimum value; the second value is used to multiply
#' the nominal value generating the maximum value
#' @param .geo if `TRUE` a geometric sequence is generated (evenly
#' spaced from min to max on log scale); otherwise, the sequence is 
#' evenly spaced on Cartesian scale
#' 
#' @details
#' - `.n`       is passed to [seq_fct()] as `n`
#' - `.factor`  is passed to [seq_fct()] as `factor`
#' 
#' @seealso [parseq_cv()], [parseq_range()], [parseq_manual()]
#' 
#' @export
parseq_factor <- function(mod, ..., .n = 5, .factor = 2, .geo = TRUE) {
  qpars <- quos(...)
  if(length(qpars) > 0) {
    sel <- vars_select(names(param(mod)),!!!qpars) 
  } else {
    if(exists("select", mod@args)) {
      sel <- mod@args[["select"]]      
    } else {
      .stop("parameter names must be passed via `...` or selected")
    }
  }
  point <- as.list(param(mod))[sel]
  pars <- map(point, seq_fct, n = .n, factor = .factor, geo = .geo)
  mod <- save_sens_values(mod, pars)
  mod
}

#' Generate a sequence of parameters based on CV
#' 
#' @param mod a model object
#' @param ... model parameter names
#' @param .cv a coefficient of variation used to determine 
#' range of test parameters
#' @param .n number of parameters to simulate in the sequence
#' @param .nsd number of standard deviations used to determine the range
#' 
#' @details
#' - `.cv`  is passed to [seq_cv()] as `cv`
#' - `.n`   is passed to [seq_cv()] as `n`
#' - `.nsd` is passed to [seq_cv()] as `nsd`
#' 
#' @seealso [parseq_factor()], [parseq_range()], [parseq_manual()]
#' 
#' @export
parseq_cv <- function(mod, ..., .cv = 30, .n = 5, .nsd = 2) {
  qpars <- quos(...)
  if(length(qpars) > 0) {
    sel <- vars_select(names(param(mod)),!!!qpars) 
  } else {
    if(exists("select", mod@args)) {
      sel <- mod@args[["select"]]      
    } else {
      .stop("parameter names must be passed or selected")
    }
  }
  point <- as.list(param(mod))[sel]
  pars <- map(point, seq_cv, n = .n, cv = .cv, nsd = .nsd)
  mod <- save_sens_values(mod, pars)
  mod
}


#' Simulation helper to manually specify parameter sequences
#' 
#' @param mod mrgsolve model object
#' @param ... named numeric vectors of parameter values to 
#' simulate; names must correspond to parameters in the model 
#' object
#' 
#' @seealso [parseq_cv()], [parseq_range()], [parseq_factor()]
#' 
#' @export
parseq_manual <- function(mod, ...) {
  pars <- as.list(param(mod))
  values <- withr::with_environment(
    list2env(pars), list(...)
  )
  for(i in seq_along(values)) {
    values[[i]] <- eval(values[[i]], envir = c(values,pars))  
  }
  mod <- save_sens_values(mod, values)
  mod
}

#' Simulation helper to generate a sequence of parameters from a range
#' 
#' @param mod mrgsolve model object
#' @param ... unquoted parameter names, 
#' @param .n number of values to simulate for each parameter sequence
#' @param .geo if `TRUE` generate a geometric sequence; otherwise,
#' generate a sequence evenly spaced on Cartesian scale; see [seq_geo()]
#' 
#' @details
#' - `.n`  is passed to [seq_geo()] as `n`
#' 
#' @seealso [parseq_cv()], [parseq_factor()], [parseq_manual()]
#' 
#' @export
parseq_range <- function(mod, ..., .n = 5, .geo = TRUE) {
  pars <- list(...)
  l <- map_int(pars,length)
  if(!all(l==2)) {
    .stop("all parameter entries must be length 2")  
  }
  pars <- map(pars, function(x) {
    seq_geo(x[1], x[2], n = .n)
  })
  mod <- save_sens_values(mod, pars)
  mod
}

#' Set reference values for each parameter
#' 
#' @param mod a model object
#' @param auto if `TRUE` then the model parameter list is used
#' @md
#' @export
parseq_reference <- function(mod, auto = TRUE) {
  if(!exists("sens_values", mod@args)) {
    .stop("the test parameters and values must be specified first")  
  }
  if(auto) {
    mod@args[["sens_reference"]] <- as.list(param(mod))  
  }
  mod
}
