#' Calculate peak root reinforcement according to WWMc model
#'
#' @description
#' This function calculates the peak root reinforcement according to the
#' Wu/Waldron model, assuming a continuous, power-law distribution of root area ratio
#' across a range of root diameters
#'
#' @param drmin,drmax minumum and maximum root diameter in bundle
#'   (numeric array)
#' @param tru0 Tensile strength of root with reference diameter (numeric array)
#' @param betat Power law coefficient for root diameter-root tensile strength
#'   fit (numeric array)
#' @param phir0 Root area ratio of root with reference diameter (numeric array)
#' @param betaphi Power law coefficient for root diameter-root area ratio
#'   fit (numeric array)
#' @param k Wu/Waldron factor accounting for root orientations at failure
#'   (numeric scalar, default 1.2)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @return numeric array with peak reinforcement preductions `cru_wwmc`
#' @examples
#' calc_cru_wwmc(1, 5, 10, -0.5, 0.01, -0.2)
#' @export

calc_cru_wwmc <- function(drmin, drmax, tru0, betat, phir0, betaphi, k = 1.2, dr0 = 1){
  #create dataframe for ease of calculations
  dp <- data.frame(drmin=drmin, drmax=drmax, tru0=tru0, betat=betat, phir0=phir0, betaphi=betaphi)
  #initialise
  cru_wwmc <- rep(NA, nrow(dp))
  #drmin=drmax - all roots the same diameter
  i1 <- with(dp, is_near(drmin, drmax))
  cru_wwmc[i1] <- with(dp[i1,], k*phir0*tru0*(drmax/dr0)^(betaphi+betat))
  #drmin!=drmax, 1+bphi+bt==0
  i2 <- with(dp, (!is_near(drmin, drmax)) & (is_near((1+betaphi+betat), 0)))
  cru_wwmc[i2] <- with(dp[i2,], k*phir0*tru0*dr0*log(drmax/drmin))
  #drmin!=drmax, 1+bphi+bt!=0
  i3 <- with(dp, (!is_near(drmin, drmax)) & (!is_near((1+betaphi+betat), 0)))
  cru_wwmc[i3] <- with(dp[i3,], k*phir0*tru0/(1+betaphi+betat)*(drmax*(drmax/dr0)^(betaphi+betat)-drmin*(drmin/dr0)^(betaphi+betat)))
  #return
  return(cru_wwmc)
}
