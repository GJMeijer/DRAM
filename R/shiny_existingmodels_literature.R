#' Calculate root reinforcement according to existing reinforcement models
#'
#' @description
#' Wrapper function to calculate the peak root reinforcement according to
#' various FBM models, the WWM model and the RBMw model
#'
#' @param dr discrete root diameters (numeric array)
#' @param phir root area ratio per diameter class (numeric array)
#' @param tru root tensile strength per diameter class (numeric array)
#' @param tru0 tensile strength in reference root (numeric array)
#' @param betat Root strength power law coefficient
#' @param betaeps Root strain power law coefficient
#' @param betaL Root length power law coefficient
#' @param kappa Weibull shape parameter for the root survival function
#'   (numeric array)
#' @param k WWM orientation factor (default k = 1.2)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @param du dataframe with Shiny unit system
#' @return dataframe with fields for models (`model`) and peak root
#'   reinforcement predictions (`cru`)
#' @examples
#' shiny_existingmodels_literature(c(1,2,3), rep(0.01,3), 10e6*c(1,2,3), 10e6, -0.5, 0, 1, 2.5)
#' @export

shiny_existingmodels_literature <- function(dr, phir, tru, tru0, betat,betaeps,betaL, kappa, du = NULL, k = 1.2, dr0 = 1){
  #account for unit system - for input straight from Shiny UI
  if (!is.null(du)){
    tru0 <- tru0 * du['tru','unit_factor']
    dr0 <- dr0 * du['dr0','unit_factor']
  }
  #create existing models
  return(data.frame(
    model = c(
      'WWM',
      'FBM0',
      'FBM1',
      'FBM2',
      'RBMw'
    ),
    cru = c(
      calc_cru_wwm(phir, tru, k = k),
      calc_cru_fbm(dr, phir, tru, 0, tru0, k = k, dr0 = dr0),
      calc_cru_fbm(dr, phir, tru, 1, tru0, k = k, dr0 = dr0),
      calc_cru_fbm(dr, phir, tru, 2, tru0, k = k, dr0 = dr0),
      calc_cru_fbmw(dr, phir, tru, 2+betat-betaeps-betaL, tru0, kappa, k = k, dr0 = dr0)
    )
  ))
}
