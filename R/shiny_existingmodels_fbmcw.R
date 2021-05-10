#' Calculate root reinforcement according to FBMc and FBMcw model
#'
#' @description
#' Wrapper function to calculate the peak root reinforcement according to
#' the FBMc and FBMcw models.
#'
#' @param drmin,drmax minumum and maximum root diameter in bundle
#'   (numeric array)
#' @param betaF Load sharing coefficient (numeric array)
#' @param phirt Total root area ratio (numeric array)
#' @param betaphi Power law coefficient for root diameter-root area ratio
#'   fit (numeric array)
#' @param tru0 tensile strength in reference root (numeric array)
#' @param betat Power law coefficient for root diameter-root tensile strength
#'   fit (numeric array)
#' @param kappa Weibull shape parameter for the root survival function
#'   (numeric array)
#' @param k WWM orientation factor (default k = 1.2)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @param du dataframe with Shiny unit system
#' @return dataframe with fields for models (`model`) and peak root
#'   reinforcement predictions (`cru`)
#' @examples
#' shiny_existingmodels_fbmcw(1, 5, 1.5, 0.01, -1, 10e6, -0.5, 2.5)
#' @export

shiny_existingmodels_fbmcw <- function(drmin, drmax, betaF, phirt, betaphi, tru0, betat, kappa, du = NULL, k = k, dr0 = dr0){
  #account for unit system - for input straight from Shiny UI
  if (!is.null(du)){
    drmin <- drmin * du['drmin','unit_factor']
    drmax <- drmax * du['drmax','unit_factor']
    phirt <- phirt * du['phirt','unit_factor']
    tru0 <- tru0 * du['tru0','unit_factor']
    dr0 <- dr0 * du['dr0','unit_factor']
  }
  #phirt0
  phir0 <- calc_phir0(phirt, drmin, drmax, betaphi, dr0 = dr0)
  #WWMc
  cru_wwmc <- calc_cru_wwmc(drmin, drmax, tru0, betat, phir0, betaphi, k = k, dr0 = dr0)
  #create existing models
  return(data.frame(
    model = c(
      'FBMc',
      'FBMcw'
    ),
    cru = c(
      cru_wwmc * calc_kku_fbmc(drmin, drmax, betaF, betat, betaphi),
      cru_wwmc * calc_kku_fbmcw(drmin, drmax, betaF, betat, betaphi, kappa, dr0 = dr0)
    )
  ))
}
