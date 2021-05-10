#' Calculate root area ratio at reference diameter
#'
#' @description
#' This function calculated the root area ratio at the reference diameter
#' based on the total root area ratio, the diameter range and a power-law
#' coefficient describing the root bundle.
#'
#' @param phirt Total root area ratio in bundle (numeric array)
#' @param drmin,drmax minumum and maximum root diameter in bundle
#'   (numeric array)
#' @param betaphi Power law coefficient for root diameter-root area ratio
#'   fit (numeric array)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @return numeric array with `phir0` values
#' @examples
#' calc_phir0(0.01, 1, 5, -1)
#' @export

calc_phir0 <- function(phirt, drmin, drmax, betaphi, dr0 = 1){
  #make dataframe
  dp <- data.frame(phirt=phirt, drmin=drmin, drmax=drmax, betaphi=betaphi)
  #initialise output
  phir0 <- rep(0, nrow(dp))
  #all roots the same diameter - if so, betaphi=0 and phir0=phirt
  i1 <- with(dp, is_near(drmin, drmax))
  phir0[i1] <- with(dp[i1,], phirt)
  #range of roots presented - 1+betaphi != 0
  i2 <- with(dp, (!i1) & (!is_near((1+betaphi), 0)))
  phir0[i2] <- with(dp[i2,], phirt*(1+betaphi)/(drmax*(drmax/dr0)^betaphi - drmin*(drmin/dr0)^betaphi))
  #range of roots presented - 1+betaphi == 0
  i3 <- with(dp, (!i1) & (is_near((1+betaphi), 0)))
  phir0[i3] <- with(dp[i3,], phirt/(dr0*log(drmax/drmin)))
  #return
  return(phir0)
}
