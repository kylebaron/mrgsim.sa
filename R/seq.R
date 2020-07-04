#' Generate a geometric sequence of parameter values
#' 
#' @param from passed to \code{\link{seq}} 
#' @param to passed to \code{\link{seq}}
#' @param .n passed to \code{\link{seq}} as \code{length.out}
#' 
#' @examples
#' geo_seq(1,10,10)
#' 
#' @export
geo_seq <- function(from, to, .n=5) {
  exp(seq(log(from),log(to),length.out=.n))
}

geo_seq_ <- function(point,.n=5) {
  map(point, function(x) {
    geo_seq(x[1],x[2], .n)
  })
}


#' Generate a sequence by fold increase and decrease from a point
#' 
#' @param point a numeric vector of length 1
#' @param .n number of elements in the sequence
#' @param .factor an integer vector of length 1 or 2; if length 1, 
#' values will be recycled to length 2; the first number used to divide
#' \code{point} to generate the minimum value in the sequence; the second 
#' number is used to multiply \code{point} to generate the 
#' maximum value in the sequence
#' @param .geo if \code{TRUE}, \code{\link{geo_seq}} is used to generate
#' the sequence; otherwise, \code{\link{even_seq}} is used to generate 
#' the sequence
#' 
#' @examples
#' 
#' fct_seq(10)
#' 
#' @export
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

#' Generate evenly spaced sequence
#' 
#' @param from passed to \code{\link{seq}} 
#' @param to passed to \code{\link{seq}}
#' @param .n passed to \code{\link{seq}} as \code{length.out}
#' 
#' @examples
#' even_seq(1, 10, 4)
#' 
#' @export
even_seq <- function(from, to, .n = 5) {
  seq(from, to, length.out=.n) 
}

even_seq_ <- function(point,.n=5) {
  map(point, function(x) {
    even_seq(x[1],x[2], .n)
  }) 
}


#' Generate a sequence based on coefficient of variation
#' 
#' @param point reference parameter value
#' @param .cv coefficient of variation
#' @param .n number of values to simulate in the sequence
#' @param .nsig number of standard deviations defining the 
#' range of simulated parameter values
#' 
#' @export 
cv_seq <- function(point, .cv=30, .n=5, .nsig=2) {
  std <- sqrt((.cv/100)^2)
  from <- log(point) - .nsig*std
  to <-   log(point) + .nsig*std
  exp(seq(from, to, length.out = .n))
}

