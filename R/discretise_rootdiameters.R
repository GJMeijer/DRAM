#' Discretise root diameters into diameter classes
#'
#' @description
#' Function that assumes a power-law distribution of root area ratio
#' across a diameter range, and discretises this range into a user-defined
#' number of discrete root diameter classes
#'
#' @param drmin minimum root diamete (scalar)
#' @param drmax maximum root diameter (scalar)
#' @param nc number of root classes to use (integer scalar)
#' @param phirt total root area ratio
#' @param betaphi root area ratio power law coefficient (scalar)
#' @param du dataframe with unit system
#' @return dataframe with root diameter classes. Each class is assigned
#'   a minimum and maximum diameter (`drmin` and `drmax`), a representative
#'   root diameter (`dr`) and a root area ratio for that class (`phir`)
#' @examples
#' discretise_rootdiameters(1, 5, 10, 0.01, -1)
#' @export

discretise_rootdiameters <- function(drmin, drmax, nc, phirt, betaphi, du = NULL){
  #convert dimensioned units to SI if required
  if (!is.null(du)){
    drmin <- drmin * du['drmin','unit_factor']
    drmax <- drmax * du['drmax','unit_factor']
    phirt <- phirt * du['phirt','unit_factor']
  }
  #split into root classes and assign fraction of root area ratio per diameter
  if (is_near(drmin, drmax)){
    d <- data.frame(
      dr = drmin,
      drmin = drmin,
      drmax = drmax,
      phir = phirt
    )
  } else {
    if (drmin > drmax){
      temp <- drmin
      drmin <- drmax
      drmax <- temp
    }
    d <- data.frame(
      drmin = (drmin + (drmax-drmin) * seq(0, 1-1/nc, l = nc)),
      drmax = (drmin + (drmax-drmin) * seq(1/nc, 1, l = nc))
    )
    d$dr <- 0.5*(d$drmin + d$drmax)
    if (is_near(1+betaphi, 0)) {
      d$phir <- phirt * log(d$drmax/d$drmin) / log(drmax/drmin)
    } else {
      d$phir <- phirt * (d$drmax^(1+betaphi)-d$drmin^(1+betaphi)) /
        (drmax^(1+betaphi)-drmin^(1+betaphi))
    }
  }
  #return
  return(d)
}
