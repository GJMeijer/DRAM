#' Calculate shear strength of soil
#'
#' @description
#' Calculate the shear strength of soil according to the Mohr-Coulomb
#' failure criterion
#' 
#' @param c soil (apparent) cohesion
#' @param phi soil angle of internal friction
#' @param sign normal stress acting on the shear plane
#' @return soil shear strength
#' @examples
#' soil_shearstrength(1, 30*(2*pi/360), 10)
#' @export

soil_shearstrength <- function(c, phi, sign){
  return(c + sign*tan(phi))
}
