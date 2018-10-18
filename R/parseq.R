
##' Generate a sequence of parameters
##' 
##' @param mod a model object
##' @param ... unquoted parameter names
##' @param .n number of parameters to simulate between the minimum
##' and maximum parameter values
##' @param .factor a numeric vector used to divide and multiply the 
##' parameter value thus generating the minimum and maximum parameter values,
##' respectively, for the sequence; if \code{.factor} is length 1 it will be 
##' recycled to length 2; the first value is used to divide the nominal 
##' value generating the minimum value; the second value is used to multiply
##' the nominal value generating the maximum value
##' @param .geo if \code{TRUE} a geometric sequence is generated (evenly
##' spaced from min to max on log scale); otherwise, the sequence is 
##' evenly spaced on cartesian scale
##' 
##' @export
parseq_factor <- function(mod, ..., .n = 5, .factor = 2, .geo = TRUE) {
  qpars <- quos(...)
  
  if(length(qpars) > 0) {
    sel <- vars_select(names(param(mod)),!!!qpars) 
  } else {
    if(exists("select", mod@args)) {
      sel <- mod@args[["select"]]      
    } else {
      stop("Parameter names must be passed or selected.")
    }
  }
  point <- as.list(param(mod))[sel]
  pars <- map(point, fct_seq, .n = .n, .factor = .factor, .geo=.geo)
  mod@args[["sens_values"]] <- pars
  mod
}

##' Simulation helper to manually specify parameter sequences
##' 
##' @param mod mrgsolve model object
##' @param ... named numeric vectors of parameter values to 
##' simulate; names must correspond to parameters in the model 
##' object
##' 
##' @export
parseq_manual <- function(mod,...) {
  pars <- as.list(param(mod))
  values <- withr::with_environment(
    list2env(pars), list(...)
  )
  for(i in seq_along(values)) {
    values[[i]] <- eval(values[[i]], envir=c(values,pars))  
  }
  mod@args[["sens_values"]] <- values
  mod
}

##' Simulation helper to generate a sequence of parameters from a range
##' 
##' @param mod mrgsolve model object
##' @param ... unquoted parameter names, 
##' @param .n number of values to simulate for each parameter sequence
##' @param .geo if \code{TRUE} generate a geometric sequence; otherwise,
##' generate a sequence evenly spaced on cartesian scale
##' 
##' 
##' @export
parseq_range <- function(mod, ...,.n = 5, .geo = TRUE) {
  pars <- list(...)
  l <- map_int(pars,length)
  if(!all(l==2)) {
    stop("All parameter entries must be length 2.")  
  }
  pars <- map(pars,function(x) {
    geo_seq(x[1],x[2],.n)
  })
  mod@args[["sens_values"]] <- pars
  mod
}


