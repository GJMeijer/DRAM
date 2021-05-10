#' Calculate root reinforcement at current strain according to FBMcw model
#'
#' @description
#' This function calculates the current root reinforcement, given a strain
#' according to the FBMcw model. The results are normalised by the corresponding
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
#' @param kappa Weibull shape parameter for the root survival function
#'   (numeric array)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @return numeric array with peak reinforcement preductions `cr_fbmcw`
#' @examples
#' calc_kk_fbmc(seq(0,2,l=50), 1, 5, 1, -0.5, -0.2, 2.5)
#' @export

calc_kk_fbmcw <- function(epsr0rel, drmin, drmax, betaF, betat, betaphi, kappa, dr0 = 1){
  #integral function - not vectorised
  f_integral_single <- function(dr, epsr0rel, betaF, betat, betaphi, kappa, dr0=1.0){
    return(
      epsr0rel *
        (dr/dr0)^(betaF-2+betaphi) *
        exp(-(gamma(1+1/kappa)*epsr0rel*(dr/dr0)^(betaF-2-betat))^kappa)
    )
  }
  #get current k-values - not vectorised
  f_getkk_single <- function(epsr0rel, drmin, drmax, betaF, betat, betaphi, kappa, dr0 = 1.0){
    if (((2+betat-betaF)*log(drmax/drmin))==0){
      #zeta1 = 0 solution - all roots break at same time --> analytical solution
      epsbreak <- (drmax/dr0)^(2-betaF+betat)
      return((epsr0rel/epsbreak)*exp(-(gamma(1+1/kappa)*epsr0rel/epsbreak)^kappa))
    } else {
      #numerical solution
      integral_single <- stats::integrate(f_integral_single, lower=drmin, upper=drmax, epsr0rel=epsr0rel,betaF=betaF,betat=betat,betaphi=betaphi,kappa=kappa,dr0=dr0)$value
      if (1+betaphi+betat==0){
        return(
          integral_single /
            (dr0*log(drmax/drmin))
        )
      } else {
        return(
          integral_single *
            (1+betaphi+betat) /
            (drmax*(drmax/dr0)^(betat+betaphi)-drmin*(drmin/dr0)^(betat+betaphi))
        )
      }
    }
  }
  #create vectorised function
  f_temp <- Vectorize(
    f_getkk_single,
    c('epsr0rel', 'drmin', 'drmax', 'betaF', 'betat', 'betaphi', 'kappa', 'dr0')
  )
  #return values for kk_fbmcw
  return(f_temp(epsr0rel, drmin, drmax, betaF, betat, betaphi, kappa, dr0))
}
