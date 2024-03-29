% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseq.R
\name{parseq_manual}
\alias{parseq_manual}
\title{Simulation helper to manually specify parameter sequences}
\usage{
parseq_manual(mod, ...)
}
\arguments{
\item{mod}{mrgsolve model object.}

\item{...}{named numeric vectors of parameter values to
simulate; names must correspond to parameters in the model
object.}
}
\description{
Simulation helper to manually specify parameter sequences
}
\examples{
mod <- mrgsolve::house()

mod \%>\%
  parseq_manual(CL = c(0.5, 1, 1.5)) \%>\% 
  sens_each()

}
\seealso{
\code{\link[=parseq_cv]{parseq_cv()}}, \code{\link[=parseq_range]{parseq_range()}}, \code{\link[=parseq_fct]{parseq_fct()}}
}
