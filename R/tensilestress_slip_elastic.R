#' Tensile stress in slipping, elastic root
#'
#' @description
#' Function calculates the tensile stress in a root where is crosses the shear
#' zone in the soil. It is assumed that the root slips (i.e. the ends move) 
#' and that it behaves linear elastically. The root is assumed to be straight,
#' unbranched, have two freely supported ends and has the middle of the root
#' located in the shear zone.
#' 
#' @param Cr root circumference (array)
#' @param Ar root cross-sectional area (array)
#' @param Lr root length (array)
#' @param Ere root elastic stiffness (array)
#' @param taui root-soil interface shear stress (array)
#' @param h thickness of the shear plane (array)
#' @param cosbeta cosine of the elevation angle of the current orientation
#'   of the root within the shear zone
#' @return tensile stress in middle of root under the stated assumptions
#' @examples
#' tensilestress_slip_elastic(pi, pi/4, 200, 100, 0.002, 30, 0.8)
#' @export

tensilestress_slip_elastic <- function(Cr, Ar, Lr, Ere, taui, h, cosbeta) {
  #quadric polynomial terms
  xi2 <- -1/Ere
  xi1 <- Lr/2 * taui*Cr/Ar * 1/Ere - 1 
  xi0 <- 1/2 * taui*Cr/Ar * (Lr - h/cosbeta) 
  #solve quadratic equation
  trslip <- solve_polynomial_quadratic(xi2, xi1, xi0)
  #return
  return(trslip)
}