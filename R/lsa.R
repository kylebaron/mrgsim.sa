
make_long <- function(data,cols) {
  remain <- setdiff(names(data), cols)
  remain_data <- data[, remain, drop = FALSE]
  ans <- lapply(cols, function(col_name) {
    remain_data[["dv_name"]] <- col_name
    remain_data[["dv_value"]] <- data[, col_name]
    remain_data
  })
  bind_rows(ans)
}

dvalue <- function(sim,ref,scale) {
  (sim[["dv_value"]] - ref[["dv_value"]])/scale
}

#' Perform local sensitivity analysis
#'
#' @param mod a mrgsolve model object.
#' @param fun generating simulated for sensitivity analysis (see details).
#' @param par parameter names as character vector or comma-separated string.
#' @param var output names (compartment or capture) as character vector
#' or comma-separated string.
#' @param eps parameter change value for sensitivity analysis.
#' @param ... arguments passed to `fun`.
#' 
#' @return 
#' A tibble with class `lsa`. 
#' 
#' @examples
#' mod <- mrgsolve::house(delta=0.1)
#'
#' par <- "CL,VC,KA"
#'
#' var <- "CP"
#'
#' dose <- ev(amt = 100)
#'
#' fun <- function(mod, ...) mrgsolve::mrgsim_e(mod, dose, output="df")
#'
#' out <- lsa(mod, par, var, fun)
#'
#' head(out)
#'
#' lsa_plot(out)
#' 
#' @export
lsa <- function(mod, par, var, fun = .lsa_fun, eps = 1E-7, ...) {
  if(!inherits(mod, "mrgmod")) {
    abort("`mod` argument must have class 'mrgmod'.")
  }
  if(!is.numeric(eps)) {
    abort("`eps` argument must be numeric.")
  }
  parameters <- mrgsolve::param(mod)
  par_names <- names(parameters)
  par_sens <- cvec_cs(par)
  if(!all(par_sens %in% par_names)) {
    par_bad <- setdiff(par_sens,par_names)
    par_bad <- paste0(par_bad,collapse=",")
    abort(
      message = "Invalid parameter name(s): ",
      body = par_bad
    )
  }
  parm <- as.numeric(parameters)[par_sens]
  var <- cvec_cs(var)
  base <- as.data.frame(fun(mod, ..., .p = parm))
  if(!any(c("time", "TIME") %in% names(base))) {
    abort(
      "Output from `fun` must contain a column of time or TIME."
    )
  }
  if(!all(var %in% names(base))) {
    col_bad <- setdiff(var, names(base))
    col_bad <- paste0(col_bad, collapse = ',')
    abort(
      message = "Invalid output name(s): ",
      body = col_bad
    )
  }
  delta_p <- abs(parm*eps)
  new_p <- parm + delta_p
  base_par <- as.list(parm)
  dpar <- parm/delta_p
  cols_keep <- c("time", "TIME", var)
  cols_keep <- intersect(cols_keep, names(base))
  base <- base[, cols_keep]
  base_long <- make_long(base,var)
  scale <- base_long[["dv_value"]]
  scale[scale==0] <- eps*1E-20
  tosim <- lapply(seq_along(new_p), function(i) {
    base_par[i] <- new_p[i]
  })
  out <- lapply(tosim, FUN = function(p) {
    mod <- mrgsolve::update(mod, param = p)
    as.data.frame(fun(mod, ..., .p = p))
  })
  out <- lapply(out, function(x) x[,cols_keep])
  out <- lapply(out, make_long, cols = var)
  for(i in seq_along(tosim)) {
    out[[i]][["p_name"]] <- par_sens[i]
    out[[i]][["sens"]] <- dvalue(out[[i]],base_long,scale)*dpar[i]
  }
  ans <- as_tibble(bind_rows(out))
  class(ans) <- c("lsa", class(ans))
  ans
}

#' @param x output from `lsa()`.
#' @param ... passed to [plot.lsa()].
#' @rdname lsa
#' @export
lsa_plot <- function(x, ...) {
  assert_that(inherits(x, "lsa"))
  plot(x,...)    
}

#' Plot a lsa object
#'
#' @param x output from [lsa()].
#' @param y not used.
#' @param pal a color palette passed to [ggplot2::scale_color_brewer()]; use 
#' `NULL` to use default ggplot color scale.
#' @param ... not used.
#' 
#' @return 
#' A ggplot.
#'
#' @method plot lsa
#' @keywords internal
#' @export
plot.lsa <- function(x, y = NULL, pal = NULL, ...) {
  stopifnot(requireNamespace("ggplot2"))
  tcol <- "time"
  if("TIME" %in% names(x)) tcol <- "TIME"
  if(!exists(tcol, x)) {
    abort("Couldn't find a time column.")
  }
  x[["vera__plot__time"]] <- x[[tcol]]
  x[["dv_name"]] <- factor(x[["dv_name"]], levels = unique(x[["dv_name"]]))
  x[["parameter"]] <- factor(x[["p_name"]], levels = unique(x[["p_name"]]))
  ans <- 
    ggplot(x,aes_string("vera__plot__time","sens",col="parameter")) +
    geom_line(lwd=1) +
    theme_bw() +
    theme(legend.position="top") +
    xlab("Time") +
    ylab("Sensitivity") +
    facet_wrap(~dv_name)
  if(is.character(pal)) {
    ans <- ans + scale_color_brewer(palette = pal)  
  } 
  ans
}

.lsa_fun <- function(mod, ..., .p = list()) {
  mrgsim(mod, ...)
}
