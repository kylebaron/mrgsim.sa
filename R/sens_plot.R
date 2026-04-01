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

pick_palette <- function(ncol, name = ggplot2::waiver()) {
  colors <- trellis.par.get("superpose.symbol")$col
  ntrel <- length(colors)
  if(ncol <= ntrel) {
    palette <- scale_color_manual(values = colors, name = name)
    return(palette)
  } else {
    colors <- hcl.colors(ncol, "Dark 2")
    palette <- scale_color_manual(values = colors, name = name)
  }
  palette
}

sens_grid_plot_vars <- function(pars, group = NULL, facet = NULL) {
  checkvar <- function(x, pars) {
    if(!is.element(x, pars)) {
      msg <- glue("`{x}` is not a sensitivity parameter.")
      abort(msg)  
    }
  }
  if(is.null(group)) return(pars)
  pars0 <- pars
  lp <- length(pars)
  p <- vector(mode = "character", length = lp)
  if(is.character(facet)) {
    facet <- cvec_cs(facet)
  }
  if(is.character(group)) {
    checkvar(group, pars0)
    p[1] <- group
    pars <- pars[!pars==group]
  } else {
    p[1] <- pars[1]
    pars <- pars[-1]
  }
  if(lp==1) return(p)
  if(is.character(facet)) {
    checkvar(facet[1], pars0) 
    p[2] <- facet[1] 
    pars <- pars[!pars==facet[1]]
  } else {
    p[2] <- pars[1]
    pars <- pars[-1]
  }
  if(lp==2) return(p)
  if(length(facet) > 1) {
    checkvar(facet[2], pars0)
    p[3] <- facet[2]
  } else {
    p[3] <- pars[length(pars)]  
  }
  if(any(duplicated(p))) {
    warn("duplicated grouping or faceting variables.")  
  }
  p
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
#' @param palette a discrete color scale; like what you get from calling
#' [ggplot2::scale_color_discrete()]. For `sens_each`, this is only applied
#' when `grid = TRUE`; it is ignored for all other layouts, which use a
#' continuous viridis color scale.
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
#' out <- sens_run(
#'   mod, 
#'   sargs = list(events = dose),  
#'   par = "CL,VC"
#' ) 
#' 
#' sens_plot(out, "CP")
#' 
#' out <- sens_run(
#'   mod, 
#'   sargs = list(events = dose), 
#'   par = "CL,VC", 
#'   vary  = "grid", 
#'   .n = 3
#' )
#' 
#' sens_plot(out, "CP")
#' 
#' sens_plot(out, "CP", group = "VC")
#' 
#' if(requireNamespace("ggsci")) {
#' 
#'   color <- ggsci::scale_color_atlassian()
#'   
#'   sens_plot(out, "CP", palette = color)
#' 
#' }
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
                                xlab = "time", ylab = NULL,
                                layout = c("default", "facet_grid", 
                                           "facet_wrap", "list"),
                                grid = FALSE, 
                                palette = NULL, 
                                ...) {
  
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
  
  if(is.null(ylab)) {
    ylab <- dv_name  
  }
  
  assert_that(is.character(xlab))
  xlab <- xlab[1]
  
  if(length(dv_name) != length(ylab)) {
    ndv <- length(dv_name)
    ny <- length(ylab)
    msg <- glue("`dv_name` ({ndv}) and `ylab` ({ny}) have different lengths.")
    abort(glue(msg))
  }
  
  if((grid && length(dv_name) > 1)) {
    list <- TRUE    
  }
  
  if(length(dv_name) > 1 & default) list <- TRUE
  
  if(list) {
    args <- c(as.list(environment()), list(...))
    args$layout <- "default"
    return(sens_plot_list(dv_name, ylab, args))
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
    
    if(is.null(palette)) {
      ncolor <- length(unique(chunk[["p_value"]]))
      palette <- pick_palette(ncolor, chunk[["p_name"]][1])
    }
    
    chunk[["p_value"]] <- signif(chunk[["p_value"]], digits)
    chunk[["p_value"]] <- factor(chunk[["p_value"]])
    
    p <- ggplot(data=chunk, aes(!!x,!!sym(y),group=!!group,col=!!group))
    p <- 
      p + 
      geom_line(lwd = lwd) + 
      theme_bw() + xlab(xlab) + ylab(ylab) + 
      facet_wrap(facets = "p_name", scales = "free_y", ncol = ncol) + 
      theme(legend.position = "top") + 
      palette + labs(color = chunk[["p_name"]][1])
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

sens_plot_list <- function(dv_name, ylab, args) {
  out <- vector(mode = "list", length = length(dv_name))
  for(i in seq_along(dv_name)) {
    args$dv_name <- dv_name[i]
    args$ylab <- ylab[i]
    out[[i]] <- do.call(sens_plot.sens_each, args)
  }
  return(out)
}

#' @param group sensitivity variable for within-panel grouping; defaults to the 
#' first sensitivity variable.
#' @param facet sensitivity variable for faceting when 3 sensitivity variables
#' are being plotted; the `facet` variable will run left to right and the other
#' variable will run up and down; this argument is ignored / not needed when 
#' there are fewer than 3 sensitivity variables.
#' @rdname sens_plot
#' @export
sens_plot.sens_grid <- function(data, 
                                dv_name = NULL, 
                                logy = FALSE,
                                ncol = NULL,
                                lwd = 0.8,
                                digits = 2, 
                                plot_ref = TRUE, 
                                xlab = "time", 
                                ylab = dv_name,
                                group = NULL,
                                facet = NULL,
                                palette = NULL,
                                ...) { #nocov start
  
  if(is.null(dv_name)) {
    dv_name <- unique(data[["dv_name"]])  
  } else {
    assert_that(is.character(dv_name))
    dv_name <- cvec_cs(dv_name)  
  }
  
  if(is.null(ylab)) {
    ylab <- dv_name  
  }
  
  assert_that(is.character(xlab))
  xlab <- xlab[1]
  
  if(length(dv_name) != length(ylab)) {
    ndv <- length(dv_name)
    ny <- length(ylab)
    msg <- glue("`dv_name` ({ndv}) and `ylab` ({ny}) have different lengths.")
    abort(glue(msg))
  }
  
  if(length(dv_name) > 1) {
    args <- c(as.list(environment()), list(...))
    out <- Map(dv_name, ylab, f = function(this_dv_name, this_ylab) {
      args$dv_name <- this_dv_name
      args$ylab <- this_ylab
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
  pars <- sens_grid_plot_vars(pars, group, facet)

  if(is.null(palette)) {
    ncolor <- length(unique(data[[pars[1]]]))
    palette <- pick_palette(ncolor, name = pars[1]) 
  }
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
  p <- p + geom_line(lwd=lwd) 
  p <- p + theme_bw() + theme(legend.position = "top")
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + palette + labs(color = pars[1])
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
