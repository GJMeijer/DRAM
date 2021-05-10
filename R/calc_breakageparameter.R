#' Calculate current breakage parameter `fb` based on tensile stress
#'
#' @description
#' Calculate the breakage parameter `fb`, i.e. the fraction of roots still
#' unbroken given the current conditions in the root. This fraction is
#' calculated using the current tensile stress in the root.
#' Weibull failure can be incorporated if a `kappa` parameter, a Weibull
#' shape parameter, is specified in the input. A current value of
#' breakage parameter (`fb0`) that cannot be exceeded (to ensure roots
#' cannot 'unbreak' again) may be specified.
#'
#' @param tr current tensile stress in root (assuming unbroken) (array, size m)
#' @param tru root tensile strength (array, size m)
#' @param kappat Weibull shape parameter (array or scalar)
#' @param fb0 Current value of breakage parameter that may not be exceeded
#'   (array or scalar)
#' @return array with new values of breakage parameter `fb`
#' @examples
#' calc_breakageparameter(c(1,2,3), 2.5)
#' calc_breakageparameter(c(1,2,3), 2.5, kappat = 4)
#' calc_breakageparameter(c(1,2,3), 2.5, kappat = 4, fb0 = c(0.6, 0.6, 0.6))
#' @export

## CALCULATE BREAKAGE PARAMETER
calc_breakageparameter <- function(tr, tru, kappat = NULL, fb0 = NULL){
  if (is.null(kappat)) {
    #sudden breakage
    fb <- as.numeric((tr<=tru))
  } else {
    #weibull scale parameter
    lambdat <- tru / gamma(1+1/kappat)
    #root breakage parameter
    fb <- exp(-(tr/lambdat)^kappat)
  }
  #limit by fb0 or not
  if (is.null(fb0)) {
    #do not limit
    return(fb)
  } else{
    #limit by fb0
    return(pmin(fb, fb0))
  }
}
