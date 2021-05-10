#' Calculate root reinforcement according to FBM and FBMw model
#'
#' @description
#' Wrapper function to calculate the peak root reinforcement according to
#' the FBM and FBMw models.
#'
#' @param dr discrete root diameters (numeric array)
#' @param phir root area ratio per diameter class (numeric array)
#' @param tru root tensile strength per diameter class (numeric array)
#' @param tru0 tensile strength in reference root (numeric array)
#' @param betaF FBM load sharing coefficient
#' @param kappa Weibull shape parameter for the root survival function
#'   (numeric array)
#' @param k WWM orientation factor (default k = 1.2)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @param du dataframe with Shiny unit system
#' @return dataframe with fields for models (`model`) and peak root
#'   reinforcement predictions (`cru`)
#' @examples
#' shiny_existingmodels_fbmw(c(1,2,3), rep(0.01,3), 10e6*c(1,2,3), 10e6, 1.5, 2.5)
#' @export

shiny_existingmodels_fbmw <- function(dr, phir, tru, tru0, betaF, kappa, du = NULL, k = 1.2, dr0 = 1){
  #account for unit system - for input straight from Shiny UI
  if (!is.null(du)){
    tru0 <- tru0 * du['tru','unit_factor']
    dr0 <- dr0 * du['dr0','unit_factor']
  }
  #create existing models
  return(data.frame(
    model = c(
      'FBM',
      'FBMw'
    ),
    cru = c(
      calc_cru_fbm(dr, phir, tru, betaF, tru0, k = k, dr0 = dr0),
      calc_cru_fbmw(dr, phir, tru, betaF, tru0, kappa, k = k, dr0 = dr0)
    )
  ))
}
