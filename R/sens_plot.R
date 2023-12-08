sens_factor <- function(data, .name, prefix = "sens_facet_", digits = 2) {
  ux <- sort(unique(data[[.name]]))
  new_col <- paste0(prefix,.name)
  mutate(
    data,
    !!new_col := factor(
      .data[[.name]],
      ux,
      paste0(.name, " ", signif(ux,digits))
    )
  )
}

sens_color_n <- function(data, group) {
  if(is.character(group)) group <- sum(group)
  data <- group_by(data, .data[["p_name"]])  
  data <- mutate(data, .col = match(!!group, unique(!!group)))
  data <- mutate(data, .col = (.data[[".col"]] - 1)/max(.data[[".col"]]-1))
  data <- ungroup(data)
  data
}

#' Plot sensitivity analysis results
#' 
#' @param data output from [sens_each()] or 
#' [sens_grid()].
#' @param ... arguments passed on to methods.
#' @param dv_name dependent variable names to plot; can be a comma-separated 
#' string; if `NULL`, then the unique values of `dv_name` in `data` are used.
#' @param p_name parameter names to plot; can be a comma-separates string. 
#' @param logy if `TRUE`, y-axis is transformed to log scale
#' @param ncol passed to [ggplot2::facet_wrap()].
#' @param lwd passed to [ggplot2::geom_line()].
#' @param digits used to format numbers on the strips.
#' @param plot_ref if `TRUE`, then the reference case will be plotted in a black
#' dashed line.
#' @param grid if `TRUE`, plots from the `sens_each` method
#' will be arranged on a page with [patchwork::wrap_plots()]; see the `ncol`
#' argument.
#' 
#' @return 
#' A `ggplot` object when one `dv_name` is specified or a list of `ggplot` 
#' objects when multiple `dv_name`s are specified. 
#' 
#' @examples
#' mod <- mrgsolve::house()
#' 
#' dose <- mrgsolve::ev(amt = 100)
#' 
#' out <- sens_run(mod, sargs = list(events = dose),  par = "CL,VC") 
#' 
#' sens_plot(out, dv_name = "CP")
#' 
#' @export
sens_plot <- function(data,...) UseMethod("sens_plot")

#' @param xlab x-axis title.
#' @param ylab y-axis title; not used for `facet_grid` or `facet_wrap` layouts.
#' @param layout specifies how plots should be returned when `dv_name` requests
#' multiple dependent variables; see `Details`. 
#' 
#' @details
#' 
#' The `layout` argument is only used for the `sens_each` method. It lets 
#' you get the plots back in different formats when multiple dependent 
#' variables are requested via `dv_name`. 
#' 
#' - Use `default` to get the plots back in a list if multiple dependent 
#'   variables are requested otherwise a single plot is returned.
#' - Use `facet_grid` to get a single plot, with parameters in columns and 
#'   dependent variables in rows. 
#' - Use `facet_wrap` to get a plot with faceted using [ggplot2::facet_wrap()], 
#'   with both the parameter name and the dependent variable name in the strip.
#' - Use `list` to force output to be a list of plots; this output can be 
#'   further arranged using [patchwork::wrap_plots()] if desired. 
#' 
#' When `grid` is `TRUE`, a list of plots will be returned when multiple 
#' dependent variables are requested. 
#' 
#' @rdname sens_plot
#' @export
sens_plot.sens_each <- function(data, dv_name = NULL, p_name = NULL,
                                logy = FALSE, 
                                ncol = NULL, lwd = 0.8, 
                                digits = 3, plot_ref = TRUE,
                                xlab = "time", ylab = dv_name[1],
                                layout = c("default", "facet_grid", 
                                           "facet_wrap", "list"),
                                grid = FALSE, ...) {
  
  layout <- match.arg(layout)
  
  grid <- isTRUE(grid)
  if(grid) layout <- "grid"
  list <- layout=="list"
  default <- layout=="default"
  facet <- layout %in% c("facet_wrap", "facet_grid")
  facet_wrap <- layout=="facet_wrap"
  
  if(is.null(dv_name)) {
    dv_name <- unique(data[["dv_name"]])
  } else {
    assert_that(is.character(dv_name))
    dv_name <- cvec_cs(dv_name)
  }
  
  if((grid && length(dv_name) > 1)) {
    list <- TRUE    
  }
  
  if(length(dv_name) > 1 & default) list <- TRUE
  
  if(list) {
    args <- c(as.list(environment()), list(...))
    args$layout <- "default"
    return(sens_plot_list(dv_name, args))
  }
  
  if(!is.null(p_name)) {
    assert_that(is.character(p_name))
    pars <- cvec_cs(p_name)
  } else {
    pars <- unique(data[["p_name"]])  
  }
  
  pars <- unique(pars)
  npar <- length(pars)
  
  group <- sym("p_value")
  x <- sym("time")
  if(default || (facet && length(dv_name)==1) || grid) {
    y <- sym(dv_name)
  }
  if(facet && length(dv_name) > 1) {
    y <- sym("dv_value")  
  }
  data <- as_tibble(data)
  data <- select_sens(data, dv_name = dv_name, p_name = pars)
  data <- sens_names_to_factor(data)
  
  if(default || facet) {
    data <- sens_color_n(data, group)
    if(facet_wrap) {
      data <- mutate(
        data, 
        flip_strip = paste0(p_name, " { ", dv_name, " }")
      ) 
    }
  }
  
  if(default) {
    p <- ggplot(data=data, aes(!!x,!!y, group=!!group, col = .col))
    p <- 
      p + 
      theme_bw() + theme(legend.position = "top") + 
      facet_wrap(~ p_name, scales = "free_y", ncol = ncol) + 
      xlab(xlab) + ylab(ylab) + 
      scale_color_viridis_c(
        name = NULL, 
        breaks  = c(0,0.5,1), 
        labels = c("low", "mid", "high")
      )
    if(isTRUE(logy)) {
      p <- p + scale_y_log10()  
    }
    p <- p + geom_line(lwd = lwd)
    if(isTRUE(plot_ref)) {
      p <- p + geom_line(
        aes(.data[["time"]], .data[["ref_value"]]),
        lty = 2, lwd = lwd * 1.1, col = "black"
      )
    }
    return(p)
  } ## Simple case
  
  if(facet) {
    p <- ggplot(data = data, aes(!!x,!!y, group=!!group, col = .col))
    p <- p + theme_bw() + theme(legend.position = "top") 
    p <- p + xlab(xlab) + ylab("value")
    p <- p + scale_color_viridis_c(
      name = NULL, 
      breaks  = c(0, 0.5, 1), 
      labels = c("low", "mid", "high")
    )
    p <- p + geom_line(lwd = lwd)
    if(layout=="facet_wrap") {
      if(missing(ncol)) {
        ncol <- length(unique(data[["dv_name"]]))  
      }
      p <- p + facet_wrap(~flip_strip, scales = "free_y", ncol = ncol)  
    } else {
      p <- p + facet_grid(dv_name ~ p_name, scales = "free_y")
    }
    if(isTRUE(logy)) {
      p <- p + scale_y_log10()  
    }
    
    if(isTRUE(plot_ref)) {
      p <- p + geom_line(
        aes(.data[["time"]], .data[["ref_value"]]),
        lty = 2, lwd = lwd * 1.1, col = "black"
      )
    }
    return(p)
  }
  
  # Grid
  sp <- split(data, data[["p_name"]])
  
  plots <- lapply(sp, function(chunk) {
    chunk[["p_value"]] <- signif(chunk[["p_value"]], digits)
    chunk[["p_value"]] <- factor(chunk[["p_value"]])
    
    p <- ggplot(data=chunk, aes(!!x,!!sym(y),group=!!group,col=!!group))
    p <- 
      p + 
      geom_line(lwd = lwd) + 
      theme_bw() + xlab(xlab) + ylab(ylab) + 
      facet_wrap(facets = "p_name", scales = "free_y", ncol = ncol) + 
      theme(legend.position = "top") + 
      scale_color_discrete(name = "")
    if(isTRUE(logy)) {
      p <- p + scale_y_log10()  
    }
    if(isTRUE(plot_ref)) {
      p <- p + geom_line(
        aes(.data[["time"]],.data[["ref_value"]]),
        col="black", lty = 2, lwd = lwd * 1.1
      )
    }
    p 
  })
  if(isTRUE(grid)) {
    plots$ncol <- ncol
    return(do.call(wrap_plots, plots))
  }
  return(plots)
}

sens_plot_list <- function(dv_name, args) {
  args$ylab <- NULL
  out <- vector(mode = "list", length = length(dv_name))
  i <- 1
  for(this_dv_name in dv_name) {
    args$dv_name <- this_dv_name
    out[[i]] <- do.call(sens_plot.sens_each, args)
    i <- i+1
  }
  return(out)
}

#' @rdname sens_plot
#' @export
sens_plot.sens_grid <- function(data, dv_name = NULL, digits = 2, ncol = NULL,
                                lwd = 0.8, logy = FALSE, 
                                plot_ref = TRUE, ...) { #nocov start
  
  if(is.null(dv_name)) {
    dv_name <- unique(data[["dv_name"]])  
  } else {
    assert_that(is.character(dv_name))
    dv_name <- cvec_cs(dv_name)  
  }
  
  if(length(dv_name) > 1) {
    args <- c(as.list(environment()), list(...))
    out <- lapply(dv_name, function(this_dv_name) {
      args$dv_name <- this_dv_name
      do.call(sens_plot.sens_grid, args)
    })
    return(out)
  }
  
  pars <- names(attr(data, "pars"))
  npar <- length(pars)
  if(npar > 3) {
    abort(
      message = "Too many parameters to make this plot.", 
      body = c(
        "Found more than 3 parameters in this `sens_grid` object.", 
        "Please construct your own `ggplot` call to plot these data ",
        "or select 3 or fewer parameters for sensitivity analysis."
      )
    )  
  }
  
  data <- select_sens(data, dv_name = dv_name)
  data <- sens_names_to_factor(data)
  group <- sym(pars[1])
  tcol <- "time"
  if(exists("TIME", data)) tcol <- "TIME"
  x <- sym(tcol)
  y <- sym(dv_name)
  formula <- NULL
  data[[as_string(group)]] <- signif(data[[as_string(group)]],3)
  if(npar==2) {
    formula <- as.formula(paste0("~sens_facet_",pars[2]))
    data <- sens_factor(data, pars[2], digits = digits) 
  }
  if(npar==3) {
    formula <- as.formula(paste0("sens_facet_", pars[3], "~sens_facet_", pars[2]))
    data <- sens_factor(data, pars[2], digits = digits)
    data <- sens_factor(data, pars[3], digits = digits)
  }
  p <- ggplot(data = data, aes(!!x, !!y, group=!!group, col=factor(!!group)))  
  p <- p + geom_line(lwd=lwd) + scale_color_discrete(name = pars[1])
  p <- p + theme_bw() + theme(legend.position = "top")
  if(npar==2) p <- p + facet_wrap(formula, ncol = ncol)
  if(npar==3) p <- p + facet_grid(formula)
  if(isTRUE(logy)) p <- p + scale_y_log10()
  if(isTRUE(plot_ref)) {
    p <- p + geom_line(
      aes(.data[["time"]],.data[["ref_value"]]),
      col = "black", lty = 2, lwd = lwd
    )
  }
  p
} # nocov end

#' @export
sens_plot.sens_each_data <- function(data, ...) {
  abort(
    message = "No plotting method for objects of this class.", 
    body = "Use 'as_tibble' to coerce to a data frame and then plot with ggplot2." 
  )
}
