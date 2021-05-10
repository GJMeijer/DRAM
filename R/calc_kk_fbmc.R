#' Calculate root reinforcement at current strain according to FBMc model
#'
#' @description
#' This function calculates the current root reinforcement, given a strain
#' according to the FBMc model. The results are normalised by the corresponding
#' WWMc model predictions, resulting in a k" coefficient.
#'
#' @param epsr0rel tensile strain in reference root, normalised by the tensile
#'   strain to peak in this root (numeric array)
#' @param drmin,drmax minumum and maximum root diameter in bundle
#'   (numeric array)
#' @param betaF Load sharing coefficient (numeric array)
#' @param betat Power law coefficient for root diameter-root tensile strength
#'   fit (numeric array)
#' @param betaphi Power law coefficient for root diameter-root area ratio
#'   fit (numeric array)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @return numeric array with peak reinforcement preductions `cr_fbmc`
#' @examples
#' calc_kk_fbmc(seq(0,1,l=25), 1, 5, 1, -0.5, -0.2)
#' @export

calc_kk_fbmc <- function(epsr0rel, drmin, drmax, betaF, betat, betaphi, dr0 = 1){
  #create dataframe
  dp <- data.frame(epsr0rel=epsr0rel, drmin=drmin, drmax=drmax, betaF=betaF, betat=betat,betaphi=betaphi)
  #WWM part (divided by k*tru0*phir0)
  i0 <- with(dp, is_near((1+betat+betaphi), 0))
  wwm <- rep(NA, nrow(dp))
  wwm[i0] <- with(dp[i0,], dr0*log(drmax/drmin))
  wwm[!i0] <- with(dp[!i0,], (drmax*(drmax/dr0)^(betat+betaphi)-drmin*(drmin/dr0)^(betat+betaphi))/(1+betat+betaphi))
  #calculate strain to failure in thicknest and thinnest root
  dp$epsr0relmin <- with(dp, (drmin/dr0)^(2-betaF+betat))
  dp$epsr0relmax <- with(dp, (drmax/dr0)^(2-betaF+betat))
  #initiate lower and upper diameter limits for integration
  dp$d1 <- dp$drmin  #default, all still unbroken
  dp$d2 <- dp$drmax  #default, all still unbroken
  #diameter limits - some thin broken
  i1 <- with(dp, (epsr0relmin<epsr0rel) & (epsr0rel<=epsr0relmax))
  dp$d1[i1] <- with(dp[i1,], dr0*(1/epsr0rel)^(1/(betaF-2-betat)))
  #diameter limits - some thick broken
  i2 <- with(dp, (epsr0relmax<epsr0rel) & (epsr0rel<=epsr0relmin))
  dp$d2[i2] <- with(dp[i2,], dr0*(1/epsr0rel)^(1/(betaF-2-betat)))
  #diameter limits - all broken
  i3 <- with(dp, (epsr0rel>epsr0relmin) & (epsr0rel>epsr0relmax))
  dp$d1[i3] <- dp$drmax[i3]
  #initiate output vector
  fbmc <- rep(NA,nrow(dp))
  #integrate over all intact diameters
  i4 <- with(dp, is_near((betaF-1+betaphi),0))
  fbmc[i4] <- with(dp[i4,], dr0*log(d2/d1)*epsr0rel)
  fbmc[!i4] <- with(dp[!i4,], 1/(betaF-1+betaphi) *
                      (d2*(d2/dr0)^(betaF-2+betaphi) - d1*(d1/dr0)^(betaF-2+betaphi)) *
                      epsr0rel)
  #get kk ratio
  kk <- fbmc/wwm
  #special case: drmin=drmax
  i5 <- with(dp, is_near(drmin,drmax) & (epsr0rel>epsr0relmin))
  kk[i5] <- 0
  i6 <- with(dp, is_near(drmin,drmax) & (epsr0rel<=epsr0relmin))
  kk[i6] <- with(dp[i6,], epsr0rel/epsr0relmin)
  #return ratio: kk=fbmc/wwm
  return(kk)
}
