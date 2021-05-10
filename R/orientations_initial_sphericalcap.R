#' Create initial root orientations on a 3D-spherical cap
#'
#' @description
#' Determines discrete number of root orientations that approximate a
#' continuous distribution of initial root orienations on a spherical
#' cap described by a maximum elevation angle `beta0max`.
#'
#' Root orientations are distributed among a range of equal-width
#' rings along the spherical cap. The number of cells in each band
#' increases with 4 for every ring, similar to an rectangular grid
#' of cells.
#'
#' In each cell, a representative points is located such that the
#' point is located in the centre of gravity of each cell.
#'
#' The number of returned discrete orientation may be larger than the
#' number requested (`n`) because of the grid method.
#'
#' Each discrete orientation is assigned a certain `weight` according to the
#' area of the cell it represents. The sum of weights for all orientations
#' is 1.
#'
#' @param beta0max the maximum elevation angle describing the spherical cap
#' @param n number of discrete orientation requested
#' @param band_offset offset each row of cells by a certain fraction of the
#'   azimuth width of the cell
#' @return dataframe with the initial azimuth (`alpha0`), the initial
#'   elevation (`beta0`) and the relative weight that should be assigned to
#'   each orientation (`weight`).
#' @examples
#' orientations_initial_sphericalcap(pi/4, 15)
#' orientations_initial_sphericalcap(pi/4, 25)
#' @export

orientations_initial_sphericalcap <- function(beta0max, n, band_offset = 0.5){
  #discretise spherical cap into number of cells
  do <- orientations_initial_sphericalcap_discretise(beta0max, n, band_offset = band_offset)
  #elevation angle - average in terms of area
  do$beta0 <- with(do, (sin(beta0_1)-sin(beta0_0)+beta0_0*cos(beta0_0)-beta0_1*cos(beta0_1))/(cos(beta0_0)-cos(beta0_1)))
  #first band, if only one cell --> beta = 0
  do$beta0[do$band==1 & do$ncell==1] <- 0
  #weight assigned to each cell
  do$weight <- with(do, (cos(beta0_0)-cos(beta0_1))/(1-cos(beta0max))/ncell)
  #average azimuth
  do$alpha0 <- with(do, 0.5*(alpha0_0 + alpha0_1))
  #return
  return(do[,c('alpha0','beta0','weight')])
}
