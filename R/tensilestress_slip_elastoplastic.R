#' Tensile stress in slipping, elasto-plastic root
#'
#' @description
#' Function calculates the tensile stress in a root where is crosses the shear
#' zone in the soil. It is assumed that the root slips (i.e. the ends move)
#' and that the middle of the root is behaving elasto-plastic.
#' The root is assumed to be straight, unbranched, have two freely supported
#' ends and has the middle of the root located in the shear zone.
#'
#' @param Cr root circumference (array)
#' @param Ar root cross-sectional area (array)
#' @param Lr root length (array)
#' @paray try root yield strength (array)
#' @param Ere root elastic stiffness (array)
#' @param Erp root plastic stiffness (array)
#' @param taui root-soil interface shear stress (array)
#' @param h thickness of the shear plane (array)
#' @param cosbeta cosine of the elevation angle of the current orientation
#'   of the root within the shear zone
#' @return tensile stress in middle of root under the stated assumptions
#' @examples
#' tensilestress_slip_elastoplastic(pi, pi/4, 200, 10, 100, 80, 0.002, 30, 0.8)
#' @export

tensilestress_slip_elastoplastic <- function(Cr, Ar, Lr, try, Ere, Erp, taui, h, cosbeta) {
  #intermediate variables
  zeta <- try/Erp - try/Ere
  #quadric polynomial terms
  xi2 <- -1/Erp
  xi1 <- Lr/2 * taui*Cr/Ar * 1/Erp - 1 + zeta
  xi0 <- 1/2 * taui*Cr/Ar * (Lr*(1-zeta) - h/cosbeta)
  #solve quadratic equation
  trslip <- solve_polynomial_quadratic(xi2, xi1, xi0)
  #return
  return(trslip)
}
