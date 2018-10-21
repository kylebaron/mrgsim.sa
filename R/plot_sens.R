
plot_sens <- function(data, y, x = "time", col = split, 
                      log = FALSE, split = FALSE, lwd = 0.75) {
  
  assert_that(requireNamespace("ggplot2"))
  
  y <- enexpr(y)
  x <- enexpr(x)
  
  if(split){
    sp <- split(data, data$name)
  } else {
    sp <- split(data, rep(1,nrow(data)))  
  }
  
  out <- lapply(sp, function(.data) {
    
    .data <- mutate(.data, ID = paste(.data[["ID"]], .data[[".parid"]]))
    
    nlev <- length(unique(.data$value))
    
    if(nlev <=1) col <- FALSE
    
    if(nlev <= 16 & col) {
      .data <- mutate(.data, sens_value = factor(signif(.data[["value"]],3)))
    } else {
      .data <- mutate(.data, sens_value = .data[["value"]])  
    }
    
    p <- ggplot2::ggplot(.data) 
    if(col) {
      p <- p + 
        ggplot2::geom_line(
          ggplot2::aes_string(x,y,group="ID",col="sens_value"),lwd = lwd
        )  
      if(nlev > 8 & nlev <= 16) {
        p <- p + ggplot2::guides(color=ggplot2::guide_legend(ncol=6))
      }
    } else {
      p <- p + 
        ggplot2::geom_line(
          ggplot2::aes_string(x,y,group="ID"), lwd = lwd
        )  
    }
    
    p <- p + ggplot2::facet_wrap(~name) + ggplot2::labs(color = "")
    
    p + ggplot2::theme_bw() + ggplot2::theme(legend.position = "top")
    
  })
  
  if(log) {
    out <- map(out, .f = function(x) {
      x + ggplot2::scale_y_continuous(trans = "log10", breaks = 10^seq(-50,50))  
    })
  }
  if(length(out)==1) return(out[[1]])
  return(out)
}



sens_factor <- function(data, name, prefix = "sens_facet_", digits = 2) {
  ux <- sort(unique(data[[name]]))
  new_col <- paste0(prefix,name)
  mutate(
    data, 
    !!new_col := factor(
      .data[[name]],
      ux,
      paste0(name, " ", signif(ux,digits))
    )
  )
}


##' Plot sensitivity analysis results
##' 
##' @param data output from \code{\link{sens_each}} or 
##' \code{\link{sens_grid}}
##' @param ... arguments passed on to methods
##' @param col output column name to plot
##' @param log if \code{TRUE}, y-axis is transformed to log scale
##' @param ncol passed to \code{\link[ggplot2]{facet_wrap}}
##' @param digits used to format numbers on the strips
##' 
##' @export
sens_plot <- function(data,...) UseMethod("sens_plot")

##' @rdname sens_plot
##' @export
sens_plot.sens_each <- function(data, col, log = FALSE, ncol=NULL,...) {
  pars <- unique(data[["name"]])
  npar <- length(unique(pars))
  data <- as.data.frame(data) 
  group <- sym("value")
  x <- sym("time")
  y <- enexpr(col)
  p <- ggplot(data=data, aes(!!x,!!y,group=!!group))
  p <- 
    p + 
    geom_line(lwd=0.8) + 
    theme_bw() + 
    facet_wrap(~name,scales = "free_y", ncol = ncol)
  if(log) {
    p <- p + scale_y_log10()  
  }
  p
}

##' @rdname sens_plot
##' @export
sens_plot.sens_grid <- function(data, col, digits = 2, ncol = NULL,...) {
  npar <- ncol(data)-1
  if(npar > 3) {
    stop("More than 3 parameters not allowed in the plot method for this object.")  
  }
  data <- mutate(data,.case = seq(n()))
  data <- unnest(data)
  pars <- names(data)[seq(npar)]
  group <- sym(pars[1])
  tcol <- "time"
  if(exists("TIME", data)) tcol <- "TIME"
  x <- sym(tcol)
  y <- enexpr(col)
  formula <- NULL
  if(npar==2) {
    formula <- as.formula(paste0("~sens_facet_",pars[2]))
    data <- sens_factor(data,pars[2], digits = digits) 
  }
  if(npar==3) {
    formula <- as.formula(paste0("sens_facet_",pars[3],"~sens_facet_",pars[2]))
    data <- sens_factor(data,pars[2],digits = digits)
    data <- sens_factor(data,pars[3], digits = digits)
  }
  p <- ggplot(data = data, aes(!!x,!!y,group=!!group))  
  p <- p + geom_line(lwd=0.8) + theme_bw() 
  if(npar==2) p <- p + facet_wrap(formula,ncol=ncol)
  if(npar==3) p <- p + facet_grid(formula)
  p
}

##' @export
sens_plot.sens_each_data <- function(data, ...) {
  stop(
"There is no plotting method for objects of this class.  
Use `as_data_frame` to coerce to a data frame and then plot with ggplot2."
  )
}