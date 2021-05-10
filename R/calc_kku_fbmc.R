#' Calculate sequential mobilisation reduction factor k" for FBMc model
#'
#' @description
#' This function calculates reduction in peak root reinforcement due to
#' sequential mobilisation of roots according to the FBMc model, compared
#' to the reinforcement predicted by the WWMc model
#'
#' @param drmin,drmax minumum and maximum root diameter in bundle
#'   (numeric array)
#' @param betaF load sharing coefficient (numeric array)
#' @param betat Power law coefficient for root diameter-root tensile strength
#'   fit (numeric array)
#' @param betaphi Power law coefficient for root diameter-root area ratio
#'   fit (numeric array)
#' @return numeric array with reduction coefficients k" (numeric array)
#' @examples
#' calc_kku_fbmc(1, 5, 1, -0.5, -1)
#' @export

calc_kku_fbmc <- function(drmin, drmax, betaF, betat, betaphi){
  #create dataframe
  dp <- data.frame(drmin=drmin, drmax=drmax, betaF=betaF, betat=betat, betaphi=betaphi)
  #calculate zeta values
  dp$zeta1 <- with(dp, (2+betat-betaF)*log(drmax/drmin))
  dp$zeta2 <- with(dp, (1+betat+betaphi)*log(drmax/drmin))
  #if zeta1<0, swap signs for zeta1 and zeta2
  dp$zeta2[dp$zeta1<0] <- -dp$zeta2[dp$zeta1<0]
  dp$zeta1[dp$zeta1<0] <- -dp$zeta1[dp$zeta1<0]
  #initialise output
  kku_fbmc <- rep(1,nrow(dp))
  #get solution for all cases:
  i1 <- with(dp, (zeta1>0) & is_near(zeta1,zeta2) & (zeta1<=1))
  kku_fbmc[i1] <- with(dp[i1,], zeta2*exp(-zeta2)/(1-exp(-zeta2)))
  i2 <- with(dp, (zeta1>0) & is_near(zeta1,zeta2) & (zeta1>1))
  kku_fbmc[i2] <- with(dp[i2,], exp(-1)/(1-exp(-zeta2)))
  i3 <- with(dp, (zeta1>0) & (zeta2>0) & !is_near(zeta1,zeta2) & ((zeta1/zeta2)^(1/(zeta2-zeta1))>exp(-1)))
  kku_fbmc[i3] <- with(dp[i3,], (zeta1/zeta2)^(zeta1/(zeta2-zeta1))/(1-exp(-zeta2)))
  i4 <- with(dp, (zeta1>0) & (zeta2>0) & !is_near(zeta1,zeta2) & ((zeta1/zeta2)^(1/(zeta2-zeta1))<=exp(-1)))
  kku_fbmc[i4] <- with(dp[i4,], zeta2/(zeta1-zeta2)*(exp(-zeta2)-exp(-zeta1))/(1-exp(-zeta2)))
  i5 <- with(dp, (zeta1>0) & is_near(zeta2,0))
  kku_fbmc[i5] <- with(dp[i5,], (1-exp(-zeta1))/zeta1)
  i6 <- with(dp, (zeta1>0) & (zeta2<0))
  kku_fbmc[i6] <- with(dp[i6,], zeta2/(zeta1-zeta2)*(exp(-zeta2)-exp(-zeta1))/(1-exp(-zeta2)))
  #return
  return(kku_fbmc)
}
