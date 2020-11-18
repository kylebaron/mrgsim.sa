
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


#' Plot sensitivity analysis results
#' 
#' @param data output from [sens_each()] or 
#' [sens_grid()]
#' @param ... arguments passed on to methods
#' @param dv_name output column name to plot
#' @param logy if `TRUE`, y-axis is transformed to log scale
#' @param ncol passed to [ggplot2::facet_wrap()]
#' @param bw if `TRUE` a simple black and white plot will be generated
#' when the [sens_each()] method is used
#' @param digits used to format numbers on the strips
#' @param plot_ref if `TRUE`, then the reference case will be plotted in a black
#' dashed line
#' @param cowplot if `TRUE`, plots from the `sens_each` method
#' will be passed through [cowplot::plot_grid()]
#' 
#' @export
sens_plot <- function(data,...) UseMethod("sens_plot")

#' @param xlab x-axis title
#' @param ylab y-axis title
#' @rdname sens_plot
#' @export
sens_plot.sens_each <- function(data, dv_name, logy = FALSE, ncol=NULL, 
                                bw = FALSE, digits = 3, plot_ref = TRUE,
                                xlab = "time", ylab = dv_name[1],
                                cowplot = TRUE, ...) {
  pars <- unique(data[["p_name"]])
  npar <- length(unique(pars))
  
  group <- sym("p_value")
  x <- sym("time")
  y <- sym(dv_name)
  
  data <- as_tibble(data)
  data <- select_sens(data, dv_name = dv_name)
  
  if(bw) {
    p <- ggplot(data=data, aes(!!x,!!y,group=!!group))
    p <- 
      p + 
      geom_line(lwd=0.8) + 
      theme_bw() + 
      facet_wrap(~p_name,scales = "free_y", ncol = ncol) + 
      xlab(xlab) + ylab(ylab)
    if(isTRUE(logy)) {
      p <- p + scale_y_log10()  
    }
    return(p)
  } ## Simple case
  
  sp <- split(data,data[["p_name"]])
  
  plots <- lapply(sp, function(chunk) {
    chunk[["p_value"]] <- signif(chunk[["p_value"]],digits)
    chunk[["p_value"]] <- factor(chunk[["p_value"]])
    p <- ggplot(data=chunk, aes(!!x,!!sym(y),group=!!group,col=!!group))
    p <- 
      p + 
      geom_line(lwd=0.8) + 
      theme_bw() + xlab(xlab) + ylab(ylab) + 
      facet_wrap(~ p_name, scales = "free_y", ncol = ncol) + 
      theme(legend.position = "top") + 
      scale_color_discrete(name = "value")
    if(isTRUE(logy)) {
      p <- p + scale_y_log10()  
    }
    if(isTRUE(plot_ref)) {
      p <- p + geom_line(
        aes(.data[["time"]],.data[["ref_value"]]),
        col="black", lty = 2, lwd = 0.7
      )
    }
    p 
  })
  if(cowplot) {
    if(!requireNamespace("cowplot")) {
      stop("couldn't load cowplot namespace; please install this package from CRAN.")
    }
    return(cowplot::plot_grid(plotlist=plots,ncol=ncol))
  }
  return(plots)
}

#' @rdname sens_plot
#' @export
sens_plot.sens_grid <- function(data, dv_name, digits = 2, ncol = NULL,
                                logy = FALSE, ...) { #nocov start
  pars <- names(attr(data, "pars"))
  npar <- length(pars)
  if(npar > 3) {
    stop(
      "found more than 3 parameters in this `sens_grid` object; ",
      "please construct your own `ggplot` call to plot these data ",
      "or select 3 or fewer parameters for sensitivity analysis",
      call. = FALSE
    )  
  }
  force(dv_name)
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
  p <- p + geom_line(lwd=0.8) + scale_color_discrete(name = pars[1])
  p <- p + theme_bw() + theme(legend.position = "top")
  if(npar==2) p <- p + facet_wrap(formula, ncol = ncol)
  if(npar==3) p <- p + facet_grid(formula)
  if(isTRUE(logy)) p <- p + scale_y_log10()
  p
} # nocov end

#' @export
sens_plot.sens_each_data <- function(data, ...) {
  stop(
    "there is no plotting method for objects of this class.  
Use 'as_tibble' to coerce to a data frame and then plot with ggplot2."
  )
}
