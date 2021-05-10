#' Calculate peak root reinforcement according to FBMw model
#'
#' @description
#' This function calculates the peak root reinforcement according to the
#' FBMw, assuming discrete root classes.
#'
#' @param dr representative diameter of root classes (numeric array, size m)
#' @param phir root area ratio of roots in classes (numeric array, size m)
#' @param tru tensile strength of roots in classes (numeric array, size m)
#' @param betaF load sharing parameter (numeric scalar)
#' @param tru0 Tensile strength of root with reference diameter (numeric scalar)
#' @param kappa Weibull shape parameter required for the root survival
#'   function (scalar)
#' @param k Wu/Waldron factor accounting for root orientations at failure
#'   (numeric scalar, default 1.2)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @param optim_method Method used to find peak in optimalisation procedure.
#'   `optim_method='BFGS' uses the `optim` function and makes an informed guess
#'   for the starting value. `optim_method='Brent'` uses the `optimize` function
#'   and seaches within bounds. For high values of `kappa`, the latter method
#'   does not always find the global maximum if located near the edges of the
#'   search domain, probably because of 'sawtooth' nature of the trace, and
#'   therefore `optim_method='BFGS'` is the default.
#' @return numeric scalar with peak root reinforcement preductions `cru_fbmw`
#' @examples
#' calc_cru_fbmw(seq(1,4), rep(0.01,4), 10e3*seq(4)^-0.5, 1, 10e3, 4)
#' @export

calc_cru_fbmw <- function(dr, phir, tru, betaF, tru0, kappa, k = 1.2, dr0 = 1, optim_method='BFGS'){
  #strain to peak for all roots in input
  epsr0relu <- (tru/tru0)*(dr/dr0)^(2-betaF)
  #reduction factor to find peak tensile stress function when using weibull curves
  reduc <- (1/kappa)^(1/kappa)/(gamma(1+1/kappa))
  #find maximum
  if (optim_method=='Brent'){
    ## FIRST METHOD - 'optimize' with 'Brent' algorithm
    #  (does not work very well for high values of kappa -> finds
    #  local rather than global max)
    #search limits for maximum reinforcement
    epsr0rel0 <- 0.99*reduc*min(epsr0relu)
    epsr0rel1 <- 1.01*reduc*max(epsr0relu)
    #find maximum
    opt <- stats::optimize(
      calc_cr_fbmw,
      interval = c(epsr0rel0,epsr0rel1),
      dr=dr,phir=phir,tru=tru,betaF=betaF,tru0=tru0,kappa=kappa,k=k,dr0=dr0,
      maximum = T
    )
    #return
    return(opt$objective)
  } else {
    ## SECOND METHOD - 'BFGS' method, no search bounds
    #  First calculates strain to peak for each individual class. Than evaluates
    #  which strain results in the largest total reinforcement, and uses that
    #  as a starting point in the seach for the maximum
    #evaluate function at peak of each individual trace
    ind_max <- which.max(calc_cr_fbmw(reduc*epsr0relu,dr=dr,phir=phir,tru=tru,betaF=betaF,tru0=tru0,kappa=kappa,k=k,dr0=dr0))
    #optimize
    opt2 <- stats::optim(
      reduc*epsr0relu[ind_max],
      calc_cr_fbmw,
      dr=dr,phir=phir,tru=tru,betaF=betaF,tru0=tru0,kappa=kappa,k=k,dr0=dr0,sumoutput=T,
      method = 'BFGS',
      control = list(fnscale = -1)
    )
    #return
    return(opt2$value)
  }
}
