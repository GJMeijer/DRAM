#' Calculate cosine of elevation of displaced orienation
#'
#' @description
#' Function that calculated the cosine of the elevation angle describing
#' the displaced root orientation
#' 
#' @param alpha0 azimuth angle of the initial root orientation
#' @param beta0 elevation angle of the initial root orientation
#' @param us shear displacement 
#' @param h shear zone thickness
#' @return cosine of the displaced elevation 
#' @examples
#' calc_cosbeta(pi/4, pi/4, 10, 30)
#' @export

calc_cosbeta <- function(alpha0, beta0, us, h){
  xi_x     <- us + h*cos(alpha0)*tan(beta0)
  xi_y     <- h*sin(alpha0)*tan(beta0)
  return(h / sqrt(xi_x^2 + xi_y^2 + h^2))
}
