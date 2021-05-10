#' Calculate peak root reinforcement according to FBM model
#'
#' @description
#' This function calculates the peak root reinforcement according to the
#' FBM, assuming discrete root classes
#'
#' @param dr representative diameter of root class (numeric array, size m)
#' @param phir root area ratio of roots in class (numeric array, size m)
#' @param tru tensile strength of roots in class (numeric array, size m)
#' @param betaF load sharing parameter (numeric scalar)
#' @param tru0 Tensile strength of root with reference diameter (numeric scalar)
#' @param k Wu/Waldron factor accounting for root orientations at failure
#'   (numeric scalar, default 1.2)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @return numeric scalar with root reinforcement preductions `cru_fbm`
#' @examples
#' calc_cru_fbm(seq(1,4), rep(0.01,4), 10e3*seq(4)^-0.5, 1, 10e3)
#' @export

calc_cru_fbm <- function(dr, phir, tru, betaF, tru0, k = 1.2, dr0 = 1){
  #number of strain steps and root classes
  m <- length(dr)
  #reference strain to failure in each class
  epsr0relu <- tru/tru0*(dr/dr0)^(2-betaF)
  #matrices - for each strain (row) and class (column)
  epsr0rel_m <- matrix(epsr0relu, nrow=m, ncol=m, byrow=FALSE)
  epsr0relu_m <- matrix(epsr0relu, nrow=m, ncol=m, byrow=TRUE)
  #breakage parameter
  fb_m <- (epsr0rel_m<=epsr0relu_m)
  #stiffness of the reinforcement per class
  dcrdeps_m <- matrix(k*phir*tru0*(dr/dr0)^(betaF-2), nrow=m, ncol=m, byrow=TRUE)
  #calculate current reinforcement
  cru_fbm <- max(rowSums(dcrdeps_m*epsr0rel_m*fb_m))
  #return
  return(cru_fbm)
}
