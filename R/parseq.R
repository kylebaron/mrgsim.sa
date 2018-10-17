

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

##' @export
plot_sens <- function(data, y, x = "time", col = split, 
                      log = FALSE, split = FALSE, lwd = 0.75, wrap=NULL) {
  
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
      .data <- mutate(.data, sens_value = factor(signif(value,3)))
    } else {
      .data <- mutate(.data, sens_value = value)  
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


##' @export
geo_seq <- function(point, .n=5, .factor = c(3,3)) {
  
  if(length(point)==1) {
    if(length(.factor)==1) .factor <- c(.factor,.factor)
    ans <- exp(seq(log(point/.factor[1]),log(point*.factor[2]),length.out=.n))
    return(ans)
  }
  
  if(length(point)==2) {
    ans <- exp(seq(log(point[1]),log(point[2]),length.out=.n))
    return(ans)
  }
  
  stop("The length of point must be either 1 or 2.")
}

##' @export
geo_seq_ <- function(point,.n=10,.factor=c(3,3)) {
  if(length(.factor)==1) .factor <- c(.factor,.factor)
  map(point, geo_seq, .n, .factor)
}

##' @export
even_seq_ <- function(point,.n=5,.factor=c(3,3)) {
  if(length(.factor)==1) .factor <- c(.factor,.factor)
  imap(point, even_seq, .n, .factor) 
}

##' @export
even_seq <- function(point,.n = 5, .factor = c(3,3)) {
  if(length(.factor)==1) .factor <- c(.factor,.factor)
  seq(point/.factor[1],point*.factor[2],length.out=.n) 
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
  if(.geo) {
    pars <- geo_seq_(point,.n,.factor)
  } else {
    pars <- even_seq_(point,.n,.factor)
  }
  mod@args[["sens_values"]] <- pars
  mod
}

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

##' @export
sens_each <- function(mod, ...) {
  if(!exists("sens_values", mod@args)) {
    stop("Parameter values must be selected first.")    
  }
  pars <- mod@args[["sens_values"]] 
  pars <- pars %>% list_2_idata()
  p_mrgsim(mod,pars,...)    
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
  pars <- map(pars,geo_seq,.n)
  mod@args[["sens_values"]] <- pars
  mod
}



