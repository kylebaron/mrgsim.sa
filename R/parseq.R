

list_2_idata <- function(x) {
  imap(x, function(value,name) {
    data_frame(ID = seq(length(value)),!!sym(name) := value)
  }) %>% re_id()
}

re_id <- function(x) {
  maxid <- 0
  for(i in seq_along(x)) {
    if(i==1) {
      maxid <- max(x[[i]][["ID"]])  
      next
    }
    x[[i]][["ID"]] <- x[[i]][["ID"]] + maxid
    maxid <- max(x[[i]][["ID"]])
  }
  x
}

p_expand <- function(x,...) {
  do.call(expand.grid, x) %>% mutate(ID = seq(n()))
}

p_vec <- function(...) {
  list(...)
}

##' @export
select.mrgmod <- function(mod, ...) {
  p <- vars_select(names(param(mod)),!!!quos(...))
  mod@args[["select"]] <- p
  mod
}


##' Generate a geometric sequence of parameter values
##' 
##' @param from passed to \code{\link{seq}} 
##' @param to passed to \code{\link{seq}}
##' @param .n passed to \code{\link{seq}} as \code{length.out}
##' 
##' @examples
##' geo_seq(1,10,10)
##' 
##' @export
geo_seq <- function(from, to, .n=5) {
  exp(seq(log(from),log(to),length.out=.n))
}

geo_seq_ <- function(point,.n=5) {
  map(point, function(x) {
    geo_seq(x[1],x[2], .n)
  })
}


##' Generate a sequence by fold increase and decrease from a point
##' 
##' @param point a numeric vector of length 1
##' @param .n number of elements in the sequence
##' @param .factor an integer vector of length 1 or 2; if length 1, 
##' values will be recycled to length 2; the first number used to divide
##' \code{point} to generate the minimum value in the sequence; the second 
##' number is used to multiply \code{point} to generate the 
##' maximum value in the sequence
##' @param .geo if \code{TRUE}, \code{\link{geo_seq}} is used to generate
##' the sequence; otherwise, \code{\link{even_seq}} is used to generate 
##' the sequence
##' 
##' @examples
##' 
##' fct_seq(10)
##' 
##' @export
fct_seq <- function(point, .n = 5, .factor = c(3,3), .geo=TRUE) {
  assert_that(length(point)==1)
  if(length(.factor)==1) .factor <- c(.factor, .factor)
  point <- c(point/.factor[1], point*.factor[2])
  if(.geo) {
    return(geo_seq(point[1], point[2], .n = .n))
  } else {
    return(even_seq(point[1],point[2], .n = .n))
  }
}

##' Generate evenly spaced sequence
##' 
##' @param from passed to \code{\link{seq}} 
##' @param to passed to \code{\link{seq}}
##' @param .n passed to \code{\link{seq}} as \code{length.out}
##' 
##' @examples
##' even_seq(1, 10, 4)
##' 
##' @export
even_seq <- function(from, to, .n = 5) {
  seq(from, to, length.out=.n) 
}

even_seq_ <- function(point,.n=5) {
  map(point, function(x) {
    even_seq(x[1],x[2], .n)
  }) 
}

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

