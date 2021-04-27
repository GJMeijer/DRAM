
###########################################################
### FUNCTIONS - CREATE DATAFRAME  PARAMETERS FOR SAVING ###
###########################################################

#input parameters
f_dataframeinputparameters <- function(input, nori_usedinmodel, diu){
  # FUNCTION: function to create a dataframe with all user input variables, units and values
  #           that cna be used to export .csv file
  # INPUT:
  # - <input>:            Shiny data structure containing all Shiny input variables
  # - <nori_usedinmodel>: Number of root orientations that was used in the model (may be different from requested number of orientations)
  # - <diu>:              Dataframe with unit system used
  # OUTPUT:
  # - <df>:               Dataframe with all variable names, units and values
  
  #units used
  ul <- diu['length','unit_user']
  us <- diu['stress','unit_user']
  ua <- 'degrees'

  #create dataframe
  return(data.frame(Parameter = c('phirt','bphi','ndia','drmin','drmax',
                   'nori_requested','nori_usedinmodel','ndim','beta0max','alpha0offset','beta0offset',
                   'drref','at','bt','aepsilon','bepsilon',
                   'aL','bL','trytru','eryeru','c',
                   'phi','sign','h0','hmax','taui',
                   'umax','nstep'),
    Unit = c(paste(ul,'^3/',ul,'^3',sep=''), '-', '-', ul, ul,
                   '-', '-', '-', ua, ua, ua,
                   ul, us, '-', paste(ul,'/',ul,sep=''), '-',
                   ul, '-', paste(us,'/',us,sep=''), paste('(',ul,'/',ul,')/(',ul,'/',ul,')',sep=''), us,
                   ua, us, ul, ul, us,
                   ul, '-'),
    InputValue = c(input$phir,input$bphi,input$nd,input$drmin,input$drmax,
                   input$nori,nori_usedinmodel,input$ndim,input$beta0max,input$alphaoffset,input$betaoffset,
                   input$drref,input$at,input$bt,input$aepsilon,input$bepsilon,
                   input$aL,input$bL,input$trytru,input$eryeru,input$c,
                   input$phi,input$sign,input$h0,input$hmax,input$taui,
                   input$umax,input$nstep)
  ))
}

#output parameters
f_dataframeoutputparameters <- function(dout, diu){
  # FUNCTION: function to create a dataframe with all user input variables, units and values
  #           that cna be used to export .csv file
  # INPUT:
  # - <dout>:             Dataframe with fields that need outputting
  # - <diu>:              Dataframe with unit system used
  # OUTPUT:
  # - <df>:               Dataframe with all variable names, units and values
  
  #units used
  ul <- diu['length','unit_user']
  us <- diu['stress','unit_user']

  #field names that need conversion
  fields_length <- c('u','h')
  fields_stress <- c('cr')
  #convert values - length
  dout[,fields_length] <- dout[,fields_length] /  diu['length','factor']
  #convert values - stress
  dout[,fields_stress] <- dout[,fields_stress] /  diu['stress','factor']

  #add new first row to dataframe
  dout <- rbind(dout[1,], dout)
  #change first row to row containing all units
  dout[1,] <- '[-]'  #default = dimensionless unit
  dout[1,colnames(dout)%in%fields_length] <- paste('[',ul,']', sep='')
  dout[1,colnames(dout)%in%fields_stress] <- paste('[',us,']', sep='')
  
  #return dataframe
  return(dout)
}