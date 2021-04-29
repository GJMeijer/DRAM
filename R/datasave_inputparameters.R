#' Generate dataframe with all input parameters, values and units
#'
#' @description
#' Function that takes the current input into the Shiny application and
#' generates a dataframe with parameter names, values and units.
#' @param input structure with all Shiny interactive input
#' @param norientation_used actual number of discrete root orientations used
#' @param du dataframe with unit conventions
#' @return dataframe with all properties
#' @export

datasave_inputparameters <- function(input, norientation_used = NULL, du = NULL){
  #create dataframe with parameters and input values
  dout <- data.frame(
    parameter = c(
      'phirt', 'betaphi', 'nc', 'drmin', 'drmax',
      'norientation', 'ndimension', 'beta0max', 'alphaoffset', 'betaoffset',
      'dr0', 'tru0', 'betat', 'epsru0', 'betaeps',
      'Lr0', 'betaL', 'trytru', 'epsryepsru','c',
      'phi', 'sign', 'h0', 'hmax', 'taui',
      'usmax', 'nstep'
    ),
    value = c(
      input$phirt, input$betaphi, input$nc, input$drmin,input$drmax,
      input$norientation, input$ndimension, input$beta0max, input$alphaoffset, input$betaoffset,
      input$dr0, input$tru0, input$betat, input$epsru0, input$betaeps,
      input$Lr0, input$betaL, input$trytru, input$epsryepsru, input$c,
      input$phi, input$sign, input$h0, input$hmax, input$taui,
      input$usmax, input$nstep
    )
  )
  #data type
  dout$parameter <- as.character(dout$parameter)
  dout$value = as.numeric(as.character(dout$value))
  #add used number of orientations if inputted
  if (!is.null(norientation_used)){
    dout <- rbind(dout, data.frame(parameter = 'norientation_used', value = norientation_used))
  }
  #add units if inputted
  if (!is.null(du)){
    dout$unit <- du[dout$parameter,'unit_user']
  }
  #return
  return(dout)
}
