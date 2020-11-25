#' Generate a geometric sequence of parameter values
#' 
#' @param from passed to [base::seq()]
#' @param to passed to [base::seq()]
#' @param n passed to [base::seq()] as `length.out`
#' @param digits number of significant digits in the answer; if `NULL` (the 
#' default) all digits are retained
#' 
#' @examples
#' seq_geo(1,10,10)
#' 
#' @export
seq_geo <- function(from, to, n = 5, digits = NULL) {
  ans <- exp(seq(log(from),log(to), length.out = n))
  if(is.numeric(digits)) ans <- signif(ans, digits = digits)
  ans
}

geo_seq_ <- function(point, n = 5) { #nocov start
  map(point, function(x) {
    seq_geo(x[1], x[2], n)
  })
} # nocov end


#' Generate a sequence by fold increase and decrease from a point
#' 
#' @inheritParams seq_geo
#' @param point a numeric vector of length 1
#' @param n number of elements in the sequence
#' @param factor an integer vector of length 1 or 2; if length 1, 
#' values will be recycled to length 2; the first number used to divide
#' `point` to generate the minimum value in the sequence; the second 
#' number is used to multiply `point` to generate the 
#' maximum value in the sequence
#' @param geo if `TRUE`, [seq_geo()] is used to generate
#' the sequence; otherwise, [seq_even()] is used to generate 
#' the sequence
#' 
#' @examples
#' seq_fct(10)
#' 
#' @export
seq_fct <- function(point, n = 5, factor = c(3,3), geo = TRUE, digits = NULL) {
  assert_that(length(point)==1)
  if(length(factor)==1) factor <- c(factor, factor)
  point <- c(point / factor[1], point * factor[2])
  if(geo) {
    ans <- seq_geo(point[1], point[2], n = n)
  } else {
    ans <- seq_even(point[1], point[2], n = n)
  }
  if(is.numeric(digits)) ans <- signif(ans, digits = digits)
  ans
}

#' Generate evenly spaced sequence
#' 
#' @inheritParams seq_geo
#' @param from passed to [base::seq()]
#' @param to passed to [base::seq()]
#' @param n passed to [base::seq()] as `length.out`
#' 
#' @examples
#' seq_even(1, 10, 4)
#' 
#' @export
seq_even <- function(from, to, n = 5, digits = NULL) {
  ans <- seq(from, to, length.out = n) 
  if(is.numeric(digits)) ans <- signif(ans, digits = digits)
  ans
}

even_seq_ <- function(point, n = 5) { #nocov start
  map(point, function(x) {
    seq_even(x[1], x[2], n)
  }) 
} # nocov end


#' Generate a sequence based on coefficient of variation
#' 
#' @inheritParams seq_geo
#' @param point reference parameter value
#' @param cv coefficient of variation
#' @param n number of values to simulate in the sequence
#' @param nsd number of standard deviations defining the 
#' range of simulated parameter values
#' 
#' @examples
#' seq_cv(10)
#' 
#' @export 
seq_cv <- function(point, cv = 30, n = 5, nsd = 2, digits = NULL) {
  std <- sqrt((cv/100)^2)
  from <- log(point) - nsd * std
  to <-   log(point) + nsd * std
  ans <- exp(seq(from, to, length.out = n))
  if(is.numeric(digits)) ans <- signif(ans, digits = digits)
  ans
}

