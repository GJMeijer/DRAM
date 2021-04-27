#####################
### DEFINE SERVER ###
#####################

# Define server
server <- function(input, output) {

  ## Create UI objects for input that is not dimensionless - Root diameters
  #root volume fraction
  output$ui_phirt <- renderUI({
    numericInput(
      "phirt",
      withMathJax(paste('Total root area ratio  \\(\\phi_{r,t}\\) [', du()['phirt','unit_user'], ']', sep='')),
      value = du()['phirt','value_default'] / du()['phirt','unit_factor']
    )
  })
  #minimum root diameter
  output$ui_drmin <- renderUI({
    numericInput(
      "drmin",
      withMathJax(paste('Minimum root diameter \\(d_{r,min}\\) [', du()['drmin','unit_user'] ,']', sep='')),
      value = du()['drmin','value_default'] / du()['drmin','unit_factor']
    )
  })
  #maximum root diameter
  output$ui_drmax <- renderUI({
    numericInput(
      "drmax",
      withMathJax(paste('Maximum root diameter \\(d_{r,max}\\) [', du()['drmax','unit_user'] ,']', sep='')),
      value = du()['drmax','value_default']/du()['drmax','unit_factor']
    )
  })

  ## Create UI objects for input that is not dimensionless - Root properties
  #Root tensile strength <tru0>
  output$ui_tru0 <- renderUI({
    numericInput(
      "tru0",
      withMathJax(paste('Tensile strength power law - multiplier \\(t_{r,u,0}\\) [', du()['tru0','unit_user'] ,']', sep='')),
      value = du()['tru0','value_default']/du()['tru0','unit_factor']
    )
  })
  #Root tensile strain <aepsilon>
  output$ui_epsru0 <- renderUI({
    numericInput(
      "epsru0",
      withMathJax(paste('Tensile strength power law - multiplier \\(\\epsilon_{r,u,0}\\) [', du()['epsru0','unit_user'] ,']', sep='')),
      value = du()['epsru0','value_default']/du()['epsru0','unit_factor']
    )
  })
  #reference diameter
  output$ui_dr0 <- renderUI({
    numericInput(
      "dr0",
      withMathJax(paste('Root reference diameter \\(d_{r,0}\\) [', du()['dr0','unit_user'] ,']', sep='')),
      value = du()['dr0','value_default']/du()['dr0','unit_factor']
    )
  })
  #Root length - multiplier
  output$ui_Lr0 <- renderUI({
    numericInput(
      "Lr0",
      withMathJax(paste('Root length power law - multiplier \\(L_{r,0}\\) [', du()['Lr0','unit_user'] ,']', sep='')),
      value = du()['Lr0','value_default']/du()['Lr0','unit_factor']
    )
  })
  #yield/ultimate strength ratio
  output$ui_trytru <- renderUI({
    sliderInput(
      "trytru",
      withMathJax(paste('Ratio yield/ultimate tensile strength \\(t_{r,y}/t_{r,u}\\) [', du()['trytru','unit_user'],']', sep='')),
      value = du()['trytru','value_default']/du()['trytru','unit_factor'],
      min = du()['trytru','value_min']/du()['trytru','unit_factor'],
      max = du()['trytru','value_max']/du()['trytru','unit_factor'],
      step = du()['trytru','value_step']/du()['trytru','unit_factor']
    )
  })
  #yield/ultimate strain ratio
  output$ui_epsryepsru <- renderUI({
    sliderInput(
      "epsryepsru",
      withMathJax(paste('Ratio yield/ultimate tensile strain \\(\\epsilon_{r,y}/\\epsilon_{r,u}\\) [', du()['epsryepsru','unit_user'],']', sep='')),
      value = du()['epsryepsru','value_default']/du()['epsryepsru','unit_factor'],
      min = du()['epsryepsru','value_min']/du()['epsryepsru','unit_factor'],
      max = du()['epsryepsru','value_max']/du()['epsryepsru','unit_factor'],
      step = du()['epsryepsru','value_step']/du()['epsryepsru','unit_factor']
    )
  })

  ## Create UI objects for input that is not dimensionless - Soil properties and analysis settings
  #soil cohesion
  output$ui_c <- renderUI({
    numericInput(
      "c",
      withMathJax(paste('Soil cohesion \\(c\\) [', du()['c','unit_user'] ,']', sep='')),
      value = du()['c','value_default']/du()['c','unit_factor']
    )
  })
  #effective soil stress on shear plane
  output$ui_sign <- renderUI({
    numericInput(
      "sign",
      withMathJax(paste("Normal effective soil stress on shear plane \\(\\sigma'_n\\) [", du()['sign','unit_user'] ,']', sep='')),
      value = du()['sign','value_default']/du()['sign','unit_factor']
    )
  })
  #initial shear zone thickness
  output$ui_h0 <- renderUI({
    numericInput(
      "h0",
      withMathJax(paste("Shear zone thickness - initial \\(h_0\\) [", du()['h0','unit_user'] ,']', sep='')),
      value = du()['h0','value_default']/du()['h0','unit_factor']
    )
  })
  #maximum shear zone thickness
  output$ui_hmax <- renderUI({
    numericInput(
      "hmax",
      withMathJax(paste("Shear zone thickness - maximum \\(h_{max}\\) [", du()['hmax','unit_user'] ,']', sep='')),
      value = du()['hmax','value_default']/du()['hmax','unit_factor']
    )
  })
  #soil-root interface friction
  output$ui_taui <- renderUI({
    numericInput(
      "taui",
      withMathJax(paste("Root-soil interface shear stress \\(\\tau_i\\) [", du()['taui','unit_user'] ,']', sep='')),
      value = du()['taui','value_default']/du()['taui','unit_factor']
    )
  })
  #maximum shear displacement
  output$ui_usmax <- renderUI({
    numericInput(
      "usmax",
      withMathJax(paste("Maximum shear displacement \\(u_{s,max}\\) [", du()['usmax','unit_user'] ,']', sep='')),
      value = du()['usmax','value_default']/du()['usmax','unit_factor']
    )
  })

  ## Create unit system and default/min/max/step settings required for UI input widgets
  #load from external file
  du <- reactive({
    shiny_unitsystem(dp, input$lengthunit_radio, input$rootstressunit_radio, input$rootstressunit_radio, 1, 1)
  })

  ## GENERATE ROOT PROPERTIES
  dr <- reactive({
    #validate input
    validate(
      need(input$phirt>=0, "Root volume fractions need to be larger than zero"),
      need(input$drmin>0, "Minimum root diameter needs to be larger than 0"),
      need(input$drmax>=input$drmin, "Maximum root diameter needs to be larger than minimum root diameter"),
      need(input$dr0>0, "Reference root diameter needs to be larger than 0"),
      need(input$nc>0, "At least one root diameter class is required"),
      need(input$Lr0>0, "Root length at reference diameter needs to be positive"),
      need(input$tru0>0, "Root tensile strength at reference diameter needs to be positive"),
      need(input$epsru0>0, "Root tensile strain to peak at reference diameter needs to be positive"),
      need(input$kappat>0, "Weibull shape parameter needs to be larger than 0")
    )
    #create root properties
    shiny_rootproperties(
      input$drmin, input$drmax, input$nc, input$phirt, input$betaphi,
      input$Lr0, input$betaL, input$tru0, input$betat, input$trytru,
      input$epsru0, input$betaeps, input$epsryepsru, input$kappat,
      dr0 = input$dr0, du = du()
    )
  })

  ## GENERATE ROOT PROPERTY PLOTS
  #root diameter
  output$p_diameters <- plotly::renderPlotly({
    plotly_diameterclasses(dr(), du = du())
  })
  #root tensile strength
  output$p_tensilestrength <- plotly::renderPlotly({
    plotly_tensilestrength(dr(), du = du())
  })
  #root strain
  output$p_tensilestrain <- plotly::renderPlotly({
    plotly_tensilestrain(dr(), du = du())
  })
  #root length
  output$p_rootlength <- plotly::renderPlotly({
    plotly_rootlength(dr(), du = du())
  })
  #root stress-strain plot
  output$p_stressstrain <- plotly::renderPlotly({
    plotly_stressstrain(dr()[1,], du = du())
  })

  ##Update input widgets also when they are hidden (Shiny only assigns/changes a value when the input is visible)
  ##otherwise, plotting some graphs without manually visiting all input tabs first will throw an error.
  #root diameter
  outputOptions(output, 'ui_phirt', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_drmin', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_drmax', suspendWhenHidden=FALSE)
  #root properties
  outputOptions(output, 'ui_dr0', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_tru0', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_epsru0', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_Lr0',  suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_trytru', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_epsryepsru', suspendWhenHidden=FALSE)
  #soil properties
  outputOptions(output, 'ui_c', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_sign', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_taui', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_h0', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_hmax', suspendWhenHidden=FALSE)
  #analysis settings
  outputOptions(output, 'ui_usmax', suspendWhenHidden=FALSE)
}

