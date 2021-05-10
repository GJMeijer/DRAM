#' Check yielding of the soil just outside the shear zone
#'
#' @description
#' Calculates the failure criterion of the soil just outside the shear zone.
#' if the result exceeds zero, the soil will yield.
#'
#' @param h current shear zone thickness
#' @param us current soil displacement
#' @param c soil (apparent) cohesion
#' @param phi soil angle of internal friction
#' @param sign normal stress acting on the shear plane
#' @param taui root-soil interface shear stress
#' @param Cr root circumference
#' @param Ar root cross-sectional area
#' @param Lr root length
#' @param phir root area ratio per root
#' @param alpha0 initial root orientation: azimuth
#' @param beta0 initial root orientation: elevation
#' @param try root yield strength
#' @param tru root tensile strength
#' @param Ere root elastic stiffness
#' @param Erp root elasto-plastic stiffness
#' @param kappat Weibull shape parameter for root failure
#' @param fb0 current values of root failure parameters
#' @return soil shear strength
#' @examples
#' shearzone_stabilitycriterion(30,10, 2,30*(2*pi/180),10,1, 1,1,100,0.01,0.1,0.1, 10,20,200,200)
#' @export

shearzone_stabilitycriterion <- function(h,us, c,phi,sign,taui, Cr,Ar,Lr,phir,alpha0,beta0, try,tru,Ere,Erp,kappat=NULL,fb0=NULL) {
  #deformed root orientations
  cosalpha <- calc_cosalpha(alpha0, beta0, us, h)
  sinbeta <- calc_sinbeta(alpha0, beta0, us, h)
  cosbeta <- calc_cosbeta(alpha0, beta0, us, h)
  #root tensile strength (no breakage included yet)
  tr_unbroken <- tensilestress_unbroken(Cr,Ar,Lr, try,Ere,Erp, taui,h,cosbeta,cos(beta0))$tr
  #breakage parameter
  fb <- calc_breakageparameter(tr_unbroken, tru, kappat=kappat, fb0=fb0)
  #fallow soil strength
  taus <- soil_shearstrength(c, phi, sign)
  #shear load applied by roots on soil
  taur <- destabilising_shearstress(fb*tr_unbroken, phir, phi, cosalpha,sinbeta,cosbeta)
  #return yield criterion (negative=stable, positive=unstable)
  return(taur - taus)
}
