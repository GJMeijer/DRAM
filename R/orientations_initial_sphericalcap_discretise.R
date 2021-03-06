#' Split spherical cap into a number of discrete cells
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
#' The number of returned discrete orientation may be larger than the
#' number requested (`n`) because of the grid method.
#'
#' @param beta0max the maximum elevation angle describing the spherical cap
#' @param n number of discrete orientation requested
#' @param band_offset offset each row of cells by a certain fraction of the
#'   azimuth width of the cell
#' @return dataframe with an entry for each cell. For each, the band it belongs
#'   to (`band`), the cell number on the band (`cell`), the total number of cells
#'   in the current band (`nband`), and the lower and upper azimuth
#'   (`alpha0_0`, `alpha0_1`) and elevation angles (`beta0_0`, `beta0_1`) are returned
#' @examples
#' orientations_initial_sphericalcap_discretise(pi/4, 15)
#' orientations_initial_sphericalcap_discretise(pi/4, 25)
#' @export

orientations_initial_sphericalcap_discretise <- function(beta0max, n, band_offset = 0.5){
  #round requested number of orientations to nearest sqrt
  nused <- ceiling(sqrt(n))^2
  #number of bands along the spherical cap
  nband <- ceiling(sqrt(nused)/2)
  #check is number of bands is even - then there is a middle cell
  even <- ((sqrt(nused)/2)==(round(sqrt(nused)/2)))
  #create dataframe for bands and get ranges of elevation and number of cells
  dr <- data.frame(band = seq(nband))
  if (even==T){
    dr$ncell <- 4 + (dr$band-1)*8
    dr$beta0_0 <- (dr$band-1)/nband*beta0max
    dr$beta0_1 <- (dr$band-0)/nband*beta0max
  } else {
    dr$ncell <- pmax(1,(dr$band-1)*8)
    dr$beta0_0 <- pmax(0,(dr$band-1.5)/(nband-0.5)*beta0max)
    dr$beta0_1 <- (dr$band-0.5)/(nband-0.5)*beta0max
  }
  #expand to include all azimuths
  do <- dr[rep(seq(nband), dr$ncell),]
  #reset row names
  row.names(do) <- NULL
  #cell number
  do$cell <- sequence(dr$ncell)
  #assign azimuth boundaries
  do$alpha0_0 <- -pi+2*pi*((do$cell-1)/do$ncell)
  do$alpha0_1 <- -pi+2*pi*((do$cell-0)/do$ncell)
  #offset every even band
  row_even <- with(do, (band/2==round(band/2)))
  do$alpha0_0[do$row_even==T] <- with(do[do$row_even==T,], alpha0_0 + band_offset*2*pi/ncell)
  do$alpha0_1[do$row_even==T] <- with(do[do$row_even==T,], alpha0_1 + band_offset*2*pi/ncell)
  #return all orientations
  return(do)
}
