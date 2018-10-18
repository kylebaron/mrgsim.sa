##' Plot the results of a sensitivity analysis
##' 
##' @param data simulated data
##' @param y the column to use for the y-axis
##' @param x the column to use for the x-axis
##' @param col logical; if \code{TRUE} the plot will be colored by 
##' the parameter values
##' @param log if \code{TRUE}, the y-axis will be presented on log scale
##' @param split if \code{TRUE}, a list of plots will be returned, one 
##' for each parameter in the sensitivity analysis
##' @param lwd passed to \code{\link[ggplot2]{geom_line}}
##' 
##' @export
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
    out <- purrr::map(out, .f = function(x) {
      x + ggplot2::scale_y_continuous(trans = "log10", breaks = 10^seq(-50,50))  
    })
  }
  if(length(out)==1) return(out[[1]])
  return(out)
}
