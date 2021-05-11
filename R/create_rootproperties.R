#' Add root properties to a dataframe
#'
#' @description
#' Function that adds various root properties to a dataframe containing
#' root diameter classes
#'
#' @param d dataframe with root classes. Should contain field (`dr`) for
#'   the representative root diameter in the class. This diameter is already
#'   converted to a SI unit system
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
#' d <- data.frame(dr = c(0.001, 0.002, 0.003, 0.004))
#' create_rootproperties(d, 1, 0.5, 10e6, -0.5, 0.5, 0.2, 0, 0.1, 5)
#' @export

create_rootproperties <- function(d,  Lr0,betaL, tru0,betat,trytru, epsru0,betaeps,epsryepsru, kappat, dr0 = 1, du = NULL){
  #convert dimensioned units if required
  if (!is.null(du)){
    Lr0 <- Lr0 * du['Lr0','unit_factor']
    tru0 <- tru0 * du['tru0','unit_factor']
    epsru0 <- epsru0 * du['epsru0','unit_factor']
    dr0 <- dr0 * du['dr0','unit_factor']
  }
  #root length
  d$Lr <- Lr0 * (d$dr/dr0)^betaL
  #tensile strength and yield strength
  d$tru <- tru0 * (d$dr/dr0)^betat
  d$try <- d$tru * trytru
  #tensile strain to failure and to yield
  d$epsru <- epsru0 * (d$dr/dr0)^betaeps
  d$epsry <- d$epsru * epsryepsru
  #add weibull coefficient
  d$kappat <- kappat
  #bilinear stiffness - Youngs modulus and oplastic stiffness
  d$Ere <- with(d, ifelse(is_near(epsry, 0), tru/epsru, try/epsry))
  d$Erp <- with(d, ifelse(is_near(epsry, epsru), tru/epsru, (tru-try)/(epsru-epsry)))
  #root area and length
  d$Ar <- pi/4 * (d$dr)^2
  d$Cr <- pi * (d$dr)
  #return
  return(d)
}
