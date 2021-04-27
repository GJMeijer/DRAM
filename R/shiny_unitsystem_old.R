#' Generate unit system in Shiny based on user-input
#'
#' @description
#' Defines a unit system and conversions for Shiny parameters
#'
#' @param lengthunit_radio Shiny input from radio button selection for
#'   length unit
#' @param rootstressunit_radio Shiny input from radio button selection
#'   for root stress/strength/stiffness unit
#' @param soilstressunit_radio Shiny input from radio button selection
#'   for soil stress/strength/stiffness unit
#' @return returns a dataframe with multiplication factor to get from
#'   input unit to SI unit (`factor`), the user-chosen unit (`unit_user`),
#'   the SI unit (`unit_SI`). The rownames of the dataframe refer to the
#'   parameter of interest
#' @examples
#' shiny_unitsystem_old(1, 1, 1)
#' @export

shiny_unitsystem_old <- function(lengthunit_radio, rootstressunit_radio, soilstressunit_radio){
  #Create empty list
  du <- data.frame(
    factor = rep(1, 3),
    unit_user = rep(NA, 3),
    unit_SI = rep(NA, 3),
    row.names = c('length', 'rootstress', 'soilstress')
  )
  #Length scale
  du['length','unit_SI'] <- 'm'
  if (lengthunit_radio == 1) {
    du['length','factor']    <- 1e-3
    du['length','unit_user'] <- 'mm'
  } else if (lengthunit_radio == 2) {
    du['length','factor']    <- 1e-2
    du['length','unit_user'] <- 'cm'
  } else if (lengthunit_radio == 3) {
    du['length','factor']    <- 1
    du['length','unit_user'] <- 'm'
  }
  #Root stress scale
  du['rootstress', 'unit_SI'] <- 'Pa'
  if (rootstressunit_radio == 1) {
    du['rootstress', 'factor']    <- 1
    du['rootstress', 'unit_user'] <- 'Pa'
  } else if (rootstressunit_radio == 2) {
    du['rootstress', 'factor']    <- 1e3
    du['rootstress', 'unit_user'] <- 'kPa'
  } else if (rootstressunit_radio == 3) {
    du['rootstress', 'factor']    <- 1e6
    du['rootstress', 'unit_user'] <- 'MPa'
  } else if (rootstressunit_radio == 4) {
    du['rootstress', 'factor']    <- 1e9
    du['rootstress', 'unit_user'] <- 'GPa'
  }
  #Soil stress scale
  du['soilstress', 'unit_SI'] <- 'Pa'
  if (soilstressunit_radio == 1) {
    du['soilstress', 'factor']    <- 1
    du['soilstress', 'unit_user'] <- 'Pa'
  } else if (soilstressunit_radio == 2) {
    du['soilstress', 'factor']    <- 1e3
    du['soilstress', 'unit_user'] <- 'kPa'
  } else if (soilstressunit_radio == 3) {
    du['soilstress', 'factor']    <- 1e6
    du['soilstress', 'unit_user'] <- 'MPa'
  } else if (soilstressunit_radio == 4) {
    du['soilstress', 'factor']    <- 1e9
    du['soilstress', 'unit_user'] <- 'GPa'
  }
  #return
  return(du)
}
