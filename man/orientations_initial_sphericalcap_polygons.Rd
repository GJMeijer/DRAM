% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orientations_initial_sphericalcap_polygons.R
\name{orientations_initial_sphericalcap_polygons}
\alias{orientations_initial_sphericalcap_polygons}
\title{Create polygons for each discrete cell on spherical cap}
\usage{
orientations_initial_sphericalcap_polygons(
  beta0max,
  n,
  resolution = 5 * pi/180,
  add = NULL,
  full = TRUE
)
}
\arguments{
\item{beta0max}{the maximum elevation angle describing the spherical cap}

\item{n}{number of discrete orientation requested}

\item{resolution}{the number of polygon points to use per 1 radians of
rotation}

\item{add}{an array of values to add to the coordinates for each cell.
This may be useful for quickly plotting traces - many traces can be
plotted in one command by seperating the coordinates for each cell by
a single `NA`, when all coordinates are in a single vector}

\item{full}{if TRUE, the coordinates for the full cell are returned.
if FALSE, only those for the bottom (`beta0_1`) and left side (`alpha0_0`)
are returned.}
}
\value{
dataframe with an entry for each cell. For each, the band it belongs
  to (`band`), the cell number on the band (`cell`), the total number of cells
  in the current band (`nband`), and the lower and upper azimuth
  (`alpha0_0`, `alpha0_1`) and elevation angles (`beta0_0`, `beta0_1`) are returned
}
\description{
Discretises a spherical cap, bescribed by elevation angle `betamax`,
into a number of discrete cells. For each cell, the azimuth and
elevation angles are returned in a single dataframe
}
\examples{
orientations_initial_sphericalcap_polygons(pi/4, 15)
}
