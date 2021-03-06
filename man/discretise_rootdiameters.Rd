% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discretise_rootdiameters.R
\name{discretise_rootdiameters}
\alias{discretise_rootdiameters}
\title{Discretise root diameters into diameter classes}
\usage{
discretise_rootdiameters(drmin, drmax, nc, phirt, betaphi, du = NULL)
}
\arguments{
\item{drmin}{minimum root diamete (scalar)}

\item{drmax}{maximum root diameter (scalar)}

\item{nc}{number of root classes to use (integer scalar)}

\item{phirt}{total root area ratio}

\item{betaphi}{root area ratio power law coefficient (scalar)}

\item{du}{dataframe with unit system}
}
\value{
dataframe with root diameter classes. Each class is assigned
  a minimum and maximum diameter (`drmin` and `drmax`), a representative
  root diameter (`dr`) and a root area ratio for that class (`phir`)
}
\description{
Function that assumes a power-law distribution of root area ratio
across a diameter range, and discretises this range into a user-defined
number of discrete root diameter classes
}
\examples{
discretise_rootdiameters(1, 5, 10, 0.01, -1)
}
