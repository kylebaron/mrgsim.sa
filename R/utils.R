
list_2_idata <- function(x) {
  ans <- imap(x, function(value, name) {
    tibble(ID = seq(length(value)), !!sym(name) := value)
  })
  re_id(ans)
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

#' Identify parameters in a model for sensitivity analysis
#' 
#' @param mod an mrgsolve model object.
#' @param ... unquoted parameter names.
#' 
#' @examples
#' mod <- mrgsolve::house()
#' select_par(mod, CL, VC)
#' 
#' @export
select_par <- function(mod, ...) {
  assert_that(inherits(mod, "mrgmod"))
  p <- vars_select(names(param(mod)), !!!quos(...))
  mod@args[["select"]] <- p
  mod
}

split_id <- function(x) {
  split(x, x$ID)  
}

cvec_cs <- function(x) {
  if(is.null(x) | length(x)==0) return(character(0))
  x <- unlist(strsplit(as.character(x),",",fixed=TRUE), use.names = FALSE)
  x <- unlist(strsplit(x," ", fixed = TRUE), use.names = FALSE)
  x <- x[x!=""]
  if(length(x)==0) {
    return(character(0))
  } else {
    return(x) 
  }
}

combine_list <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    abort("Input are not lists.")
  }
  left[names(right)] <-  right
  left
}

update_list <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    abort("Input are not lists.")
  }
  common <- intersect(names(left), names(right))
  left[common] <-  right[common]
  left
}

.stop <- function(...) stop(..., call. = FALSE)

is.ev <- function(x) inherits(x, "ev")
