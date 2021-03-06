% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_cru_wwm.R
\name{calc_cru_wwm}
\alias{calc_cru_wwm}
\title{Calculate peak root reinforcement according to WWMc model}
\usage{
calc_cru_wwm(phir, tru, k = 1.2)
}
\arguments{
\item{phir}{root area ratio in class (array)}

\item{tru}{representative tensile strength in class (array)
(numeric array)}

\item{k}{Wu/Waldron factor accounting for root orientations at failure
(numeric scalar, default 1.2)}
}
\value{
numeric array with peak reinforcement preductions `cru_wwm`
}
\description{
This function calculates the peak root reinforcement according to the
Wu/Waldron model. It sums the contributions of all roots in all classes
}
\examples{
calc_cru_wwm(seq(0.001,0.002,l=6), 10e3*seq(1,5,l=6))
}
