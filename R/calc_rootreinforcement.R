#' Calculate root reinforcement from known root stresses
#'
#' @description
#' Calculates the current amount of root reinforcement
#' 
#' @param phi soil angle of internal friction
#' @param tr tensile stress in root (including breakage effects)
#' @param phir root area ratio per root
#' @param cosalpha cosine of displaced root azimuth in shear zone
#' @param sinbeta sine of displaced root elevation in shear zone
#' @param cosbeta cosine of displaced root elevation in shear zone
#' @return root reinforcement for every root in input
#' @examples
#' calc_rootreinforcement(30*(2*pi/180), 10, 0.01, 1, sin(pi/5), cos(pi/6))
#' @export

calc_rootreinforcement <- function(phi, tr,phir, cosalpha,sinbeta,cosbeta){
  return(tr*phir*(cosalpha*sinbeta + cosbeta*tan(phi)))
}
