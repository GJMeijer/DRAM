#' Calculate sequential mobilisation reduction factor k" for FBMcw model
#'
#' @description
#' This function calculates reduction in peak root reinforcement due to
#' sequential mobilisation of roots according to the FBMcw model, compared
#' to the reinforcement predicted by the WWMc model
#' The peak reinforcement is found using the `optimize` function in base-R
#'
#' @param drmin,drmax minumum and maximum root diameter in bundle
#'   (numeric array)
#' @param betaF load sharing coefficient (numeric array)
#' @param betat Power law coefficient for root diameter-root tensile strength
#'   fit (numeric array)
#' @param betaphi Power law coefficient for root diameter-root area ratio
#'   fit (numeric array)
#' @param kappa Weibull shape parameter for the root survival function
#'   (numeric array)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @return numeric array with reduction coefficients k" (numeric array)
#' @examples
#' calc_kku_fbmcw(1, 5, 1, -0.5, -1, 2.5)
#' @export

calc_kku_fbmcw <- function(drmin, drmax, betaF, betat, betaphi, kappa, dr0 = 1){
  #reduction factor to find peak tensile stress function when using weibull curves
  reduc <- (1/kappa)^(1/kappa)/(gamma(1+1/kappa))
  #values of reference strain at failure of thinnest and thickest root
  epsr0relmin <- (drmin/dr0)^(2-betaF+betat)
  epsr0relmax <- (drmax/dr0)^(2-betaF+betat)
  #search limits for maximum
  epsr0rel0 <- 0.99*reduc*pmin(epsr0relmin, epsr0relmax)
  epsr0rel1 <- 1.01*reduc*pmax(epsr0relmin, epsr0relmax)
  #function to optimize (find maximum) - not vectorised
  f_findmax <- function(epsr0rel0, epsr0rel1, drmin, drmax, betaF, betat, betaphi, kappa, dr0=1.0){
    if (is_near((2+betat-betaF)*log(drmax/drmin),0)){
      #zeta1 = 0 solution - all roots break at the same time
      kk <- (1/kappa)^(1/kappa) * exp(-1/kappa) / gamma(1+1/kappa)
      return(kk)
    } else {
      #calculate peak solution
      opt <- stats::optimize(
        calc_kk_fbmcw,
        interval = c(epsr0rel0,epsr0rel1),
        drmin=drmin,drmax=drmax,betaF=betaF,betat=betat,betaphi=betaphi,kappa=kappa,dr0=dr0,
        maximum = T
      )
       #return peak strain and kk
       #return(c(opt$maximum, opt$objective))
      return(opt$objective)
    }
  }
  #create vectorised function
  f_temp2 <- Vectorize(
    f_findmax,
    c('epsr0rel0', 'epsr0rel1', 'drmin', 'drmax', 'betaF', 'betat', 'betaphi', 'kappa', 'dr0')
  )
  #return
  return(f_temp2(epsr0rel0, epsr0rel1, drmin, drmax, betaF, betat, betaphi, kappa, dr0))
}
