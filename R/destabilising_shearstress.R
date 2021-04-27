#' Calculate shear stress on soil outside shear zone
#'
#' @description
#' Calculate the shear stress roots apply on the soil just outside the 
#' shear zone. This destabilising stress may result in an increase in 
#' the shear zone thickness if this stress exceeds the shear stress in
#' the soil
#' 
#' @param tr soil (apparent) cohesion (array)
#' @param phir root area ratio (array)
#' @param phi soil angle of internal friction (scalar)
#' @param cosalpha cosine of azimuth angle of displaced root in shear zone
#' @param sinbeta sine of elevation angle of displaced root in shear zone
#' @param cosbeta cosine of elevation angle of displaced root in shear zone
#' @return shear stress applied by roots on soil just outside the shear zone
#' @examples
#' destabilising_shearstress(10, 0.01, 30*(2*pi/360), cos(0.1), sin(0.2), cos(0.2))
#' @export

## FUNCTION TO CALCULATE DESTABILISING ROOT STRESS
destabilising_shearstress <- function(tr, phir, phi, cosalpha, sinbeta, cosbeta){
  #soil volume fraction
  phis <- 1 - sum(phir)
  #destabilising stress
  taur <- 1/phis * sum(phir*tr*(cosalpha*sinbeta - cosbeta*tan(phi)))
  #return destabilising stress roots apply on soil just outside shear zone
  return(taur)
}
