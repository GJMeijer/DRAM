% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shearzone_stabilitycriterion.R
\name{shearzone_stabilitycriterion}
\alias{shearzone_stabilitycriterion}
\title{Check yielding of the soil just outside the shear zone}
\usage{
shearzone_stabilitycriterion(
  h,
  us,
  c,
  phi,
  sign,
  taui,
  Cr,
  Ar,
  Lr,
  phir,
  alpha0,
  beta0,
  try,
  tru,
  Ere,
  Erp,
  kappat = NULL,
  fb0 = NULL
)
}
\arguments{
\item{h}{current shear zone thickness}

\item{us}{current soil displacement}

\item{c}{soil (apparent) cohesion}

\item{phi}{soil angle of internal friction}

\item{sign}{normal stress acting on the shear plane}

\item{taui}{root-soil interface shear stress}

\item{Cr}{root circumference}

\item{Ar}{root cross-sectional area}

\item{Lr}{root length}

\item{phir}{root area ratio per root}

\item{alpha0}{initial root orientation: azimuth}

\item{beta0}{initial root orientation: elevation}

\item{try}{root yield strength}

\item{tru}{root tensile strength}

\item{Ere}{root elastic stiffness}

\item{Erp}{root elasto-plastic stiffness}

\item{kappat}{Weibull shape parameter for root failure}

\item{fb0}{current values of root failure parameters}
}
\value{
soil shear strength
}
\description{
Calculates the failure criterion of the soil just outside the shear zone.
if the result exceeds zero, the soil will yield.
}
\examples{
shearzone_stabilitycriterion(30,10, 2,30*(2*pi/180),10,1, 1,1,100,0.01,0.1,0.1, 10,20,200,200)
}
