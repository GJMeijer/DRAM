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
#' @param rootstrainunit_radio Shiny input from radio button selection
#'   for root strain unit
#' @param angleunit_radio Shiny input from radio button selection
#'   for angles (degrees or radians)
#' @return returns a dataframe with multiplication factor to get from
#'   input unit to SI unit (`factor`), the user-chosen unit (`unit_user`),
#'   the SI unit (`unit_SI`). The rownames of the dataframe refer to the
#'   parameter of interest
#' @examples
#' dp <- read.csv('inst/shiny-examples/DRAM/parameter_list.csv', stringsAsFactors = FALSE)
#' row.names(dp) <- dp$parameter
#' shiny_unitsystem(dp, 1, 1, 1, 1, 1)
#' @export

shiny_unitsystem <- function(dp, lengthunit_radio, rootstressunit_radio, soilstressunit_radio, rootstrainunit_radio, angleunit_radio){
  #add user units
  dp$unit_user <- dp$unit_si
  dp$unit_factor <- 1
  #length scale
  if (lengthunit_radio == 1) {
    dp$unit_user[dp$unit_type%in%c('rootlength','soillength')] <- 'mm'
    dp$unit_factor[dp$unit_type%in%c('rootlength','soillength')] <- 1e-3
  } else if (lengthunit_radio == 2) {
    dp$unit_user[dp$unit_type%in%c('rootlength','soillength')] <- 'cm'
    dp$unit_factor[dp$unit_type%in%c('rootlength','soillength')] <- 1e-2
  } else if (lengthunit_radio == 3) {
    dp$unit_user[dp$unit_type%in%c('rootlength','soillength')] <- 'm'
    dp$unit_factor[dp$unit_type%in%c('rootlength','soillength')] <- 1
  }
  #root stress/stiffness scale
  if (rootstressunit_radio == 1) {
    dp$unit_user[dp$unit_type=='rootstress'] <- 'Pa'
    dp$unit_factor[dp$unit_type=='rootstress'] <- 1
  } else if (rootstressunit_radio == 2) {
    dp$unit_user[dp$unit_type=='rootstress'] <- 'kPa'
    dp$unit_factor[dp$unit_type=='rootstress'] <- 1e3
  } else if (rootstressunit_radio == 3) {
    dp$unit_user[dp$unit_type=='rootstress'] <- 'MPa'
    dp$unit_factor[dp$unit_type=='rootstress'] <- 1e6
  } else if (rootstressunit_radio == 4) {
    dp$unit_user[dp$unit_type=='rootlength'] <- 'GPa'
    dp$unit_factor[dp$unit_type=='rootlength'] <- 1e9
  }
  #soil stress/stiffness scale
  if (soilstressunit_radio == 1) {
    dp$unit_user[dp$unit_type=='soilstress'] <- 'Pa'
    dp$unit_factor[dp$unit_type=='soilstress'] <- 1
  } else if (soilstressunit_radio == 2) {
    dp$unit_user[dp$unit_type=='soilstress'] <- 'kPa'
    dp$unit_factor[dp$unit_type=='soilstress'] <- 1e3
  } else if (soilstressunit_radio == 3) {
    dp$unit_user[dp$unit_type=='soilstress'] <- 'MPa'
    dp$unit_factor[dp$unit_type=='soilstress'] <- 1e6
  } else if (soilstressunit_radio == 4) {
    dp$unit_user[dp$unit_type=='soilstress'] <- 'GPa'
    dp$unit_factor[dp$unit_type=='soilstress'] <- 1e9
  }
  #Root strain
  if (rootstrainunit_radio == 1) {
    dp$unit_user[dp$unit_type=='rootstrain'] <- '-'
    dp$unit_factor[dp$unit_type=='rootstrain'] <- 1
  } else if (rootstrainunit_radio == 2) {
    dp$unit_user[dp$unit_type=='rootstrain'] <- '%'
    dp$unit_factor[dp$unit_type=='rootstrain'] <- 1e-2
  }
  #angles
  if (rootstrainunit_radio == 1) {
    dp$unit_user[dp$unit_type=='angle'] <- 'deg'
    dp$unit_factor[dp$unit_type=='angle'] <- pi/180
  } else if (angleunit_radio == 2){
    dp$unit_user[dp$unit_type=='angle'] <- 'rad'
    dp$unit_factor[dp$unit_type=='angle'] <- 1
  }
  #return
  return(dp)
}
