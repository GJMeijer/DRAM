#' Calculate peak root reinforcement according to WWMc model
#'
#' @description
#' This function calculates the peak root reinforcement according to the
#' Wu/Waldron model. It sums the contributions of all roots in all classes
#'
#' @param phir root area ratio in class (array)
#' @param tru representative tensile strength in class (array)
#'   (numeric array)
#' @param k Wu/Waldron factor accounting for root orientations at failure
#'   (numeric scalar, default 1.2)
#' @return numeric array with peak reinforcement preductions `cru_wwm`
#' @examples
#' calc_cru_wwm(seq(0.001,0.002,l=6), 10e3*seq(1,5,l=6))
#' @export

calc_cru_wwm <- function(phir, tru, k = 1.2){
  return(sum(k*phir*tru))
}
