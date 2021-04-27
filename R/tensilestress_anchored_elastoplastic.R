#' Tensile stress in anchored, elasto-plastic root
#'
#' @description
#' Function calculates the tensile stress in a root where is crosses the shear
#' zone in the soil. It is assumed that the root is anchored (i.e. the ends 
#' do not move) and that the middle of the root is behaving elasto-plastic.
#' The root is assumed to be straight, unbranched, have two freely supported
#' ends and has the middle of the root located in the shear zone.
#' 
#' @param Cr root circumference (array)
#' @param Ar root cross-sectional area (array)
#' @paray try root yield strength (array)
#' @param Ere root elastic stiffness (array)
#' @param Erep root elasto-plastic stiffness (array)
#' @param taui root-soil interface shear stress (array)
#' @param h thickness of the shear plane (array)
#' @param cosbeta cosine of the elevation angle of the current orientation
#'   of the root within the shear zone
#' @param cosbeta0 cosine of the elevation angle of the initial orientation
#'   of the root within the shear zone
#' @return tensile stress in middle of root under the stated assumptions
#' @examples
#' tensilestress_anchored_elastoplastic(pi, pi/4, 2, 100, 80, 0.002, 30, 0.8, 0.9)
#' @export

tensilestress_anchored_elastoplastic <- function(Cr, Ar, try, Ere, Erep, taui, h, cosbeta, cosbeta0) {
  #intermediate variables
  zeta <- try/Erep - try/Ere
  #cubic polynomial terms
  xi3 <- Ar/(taui*Cr) * 1/(Erep^2)
  xi2 <- Ar/(taui*Cr) * 1/Erep * (1 - 3*zeta)
  xi1 <- h/cosbeta0 * 1/Erep + zeta*Ar/(taui*Cr) * (try/Erep - 2*(1-zeta))
  xi0 <- h/cosbeta0 - h/cosbeta + zeta * (Ar/(taui*Cr)*try*(1-zeta) - h/cosbeta0)
  #solve cubic equation
  tranch <- solve_polynomial_cubic(xi3, xi2, xi1, xi0)
  #return
  return(tranch)
}