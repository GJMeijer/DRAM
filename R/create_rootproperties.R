#' Create all root properties
#'
#' @description
#' Function that creates dataframe with root properties based on input.
#'
#' @param drmin minimum root diamete (scalar)
#' @param drmax maximum root diameter (scalar)
#' @param nc number of root classes to use (integer scalar)
#' @param phirt total root area ratio
#' @param betaphi root area ratio power law coefficient (scalar)
#' @param Lr0 root length of root with reference diameter (scalar)
#' @param betaL power law coefficient for root length (scalar)
#' @param tru0 root tensile strength of root with reference diameter (scalar)
#' @param betat power law coefficient for root tensile strength (scalar)
#' @param trytru ratio of yield and tensile strength (scalar)
#' @param epsru0 tensile strain to peak for root with reference diameter
#'   (scalar)
#' @param betaeps power law coefficient for root tensile strain to
#'   peak (scalar)
#' @param epsryepsru ratio of yield and tensile strain (scalar)
#' @param kappat Weibull shape parameter for tensile strength (scalar)
#' @param dr0 reference diameter (scalar)
#' @param du dataframe with unit conversions
#' @return dataframe with root diameter classes and root properties
#' @examples
#' create_rootproperties(2,5, 10, 0.01,-0.2, 1,0.5, 10e6,-0.5,0.5, 0.2,0,0.1, 5)
#' @export

create_rootproperties <- function(drmin, drmax, nc, phirt,betaphi, Lr0,betaL, tru0,betat,trytru, epsru0,betaeps,epsryepsru, kappat, dr0 = 1, du = NULL){
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
  #root length
  d$Lr <- Lr0 * (d$dr/dr0)^betaL
  #tensile strength and yield strength
  d$tru <- tru0 * (d$dr/dr0)^betat
  d$try <- d$tru * trytru
  #tensile strain to failure and to yield
  d$epsru <- epsru0 * (d$dr/dr0)^betaeps
  d$epsry <- d$epsru * epsryepsru
  #bilinear stiffness - Youngs modulus and elastoplastic stiffness
  d$Ere <- with(d, ifelse(is_near(epsry, 0), tru/epsru, try/epsry))
  d$Erep <- with(d, ifelse(is_near(epsry, epsru), tru/epsru, (tru-try)/(epsru-epsry)))
  #add weibull coefficient
  d$kappat <- kappat
  #convert all parameters to SI units if requested
  if (!is.null(du)){
    d <- data.frame(mapply(`*`, d, du[colnames(d),'unit_factor'], SIMPLIFY = FALSE))
  }
  #root area and length
  d$Ar <- pi/4 * (d$dr)^2
  d$Cr <- pi * (d$dr)
  #return
  return(d)
}
