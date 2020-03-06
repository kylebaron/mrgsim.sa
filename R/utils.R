
list_2_idata <- function(x) {
  imap(x, function(value,name) {
    tibble(ID = seq(length(value)),!!sym(name) := value)
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

##' @export
select.mrgmod <- function(mod, ...) {
  p <- vars_select(names(param(mod)),!!!quos(...))
  mod@args[["select"]] <- p
  mod
}

split_id <- function(x) {
  split(x,x$ID)  
}

