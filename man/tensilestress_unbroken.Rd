% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tensilestress_unbroken.R
\name{tensilestress_unbroken}
\alias{tensilestress_unbroken}
\title{Current tensile stress in a unbroken root}
\usage{
tensilestress_unbroken(Cr, Ar, Lr, try, Ere, Erp, taui, h, cosbeta, cosbeta0)
}
\arguments{
\item{Cr}{root circumference (array)}

\item{Ar}{root cross-sectional area (array)}

\item{Lr}{root length (array)}

\item{try}{root yield strength (array)}

\item{Ere}{root elastic stiffness (array)}

\item{Erp}{root elasto-plastic stiffness (array)}

\item{taui}{root-soil interface shear stress (scalar)}

\item{h}{thickness of the shear plane (scalar)}

\item{cosbeta}{cosine of the elevation angle of the current orientation
of the root within the shear zone (array)}

\item{cosbeta0}{cosine of the elevation angle of the initial orientation
of the root within the shear zone (array)}
}
\value{
data.frame with two fields. `tr` = tensile stress in middle of
  root under the stated assumptions, `flag` = the type of root behaviour
  (0 = not in tension; 1 = anchored, elastic; 2 = anchored, elasto-plastic;
  3 = slipping, elastic; 4 = slipping, elasto-plastic
}
\description{
Function calculates the tensile stress in a root where is crosses the shear
zone in the soil. It assumes that the root is still unbroken. This function
will detect whether the bit of root in the shear zone behaves elastic or
elasto-plastic, and whether the root slips or remains anchored.
The root is assumed to be straight, unbranched, have two freely supported
ends and has the middle of the root located in the shear zone.
}
\examples{
tensilestress_unbroken(pi, pi/4, 200, 2, 100, 80, 0.002, 30, 0.8, 0.9)
}
