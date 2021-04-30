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

  ## Create UI objects for input that is not dimensionless - Root orientations
  #elevation offset
  output$ui_beta0max <- renderUI({
    sliderInput(
      "beta0max",
      withMathJax(paste('Maximum root elevation angle \\(\\beta_{0,max}\\) [', du()['beta0max','unit_user'], ']', sep='')),
      value = round(du()['beta0max','value_default']/du()['beta0max','unit_factor'],4),
      min = round(du()['beta0max','value_min']/du()['beta0max','unit_factor'],4),
      max = round(du()['beta0max','value_max']/du()['beta0max','unit_factor'],4),
      step = round(du()['beta0max','value_step']/du()['beta0max','unit_factor'],4)
    )
  })
  #centre axis offset - azimuth
  output$ui_alphaoffset <- renderUI({
    sliderInput(
      "alphaoffset",
      withMathJax(paste('Azimuth offset \\(\\alpha_{offset}\\) [', du()['alphaoffset','unit_user'], ']', sep='')),
      value = round(du()['alphaoffset','value_default']/du()['alphaoffset','unit_factor'],4),
      min = round(du()['alphaoffset','value_min']/du()['alphaoffset','unit_factor'],4),
      max = round(du()['alphaoffset','value_max']/du()['alphaoffset','unit_factor'],4),
      step = round(du()['alphaoffset','value_step']/du()['alphaoffset','unit_factor'],4)
    )
  })
  #centre axis offset - elevation
  output$ui_betaoffset <- renderUI({
    sliderInput(
      "betaoffset",
      withMathJax(paste('Elevation offset \\(\\beta_{offset}\\) [', du()['betaoffset','unit_user'], ']', sep='')),
      value = round(du()['betaoffset','value_default']/du()['betaoffset','unit_factor'],4),
      min = round(du()['betaoffset','value_min']/du()['betaoffset','unit_factor'],4),
      max = round(du()['betaoffset','value_max']/du()['betaoffset','unit_factor'],4),
      step = round(du()['betaoffset','value_step']/du()['betaoffset','unit_factor'],4)
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
      withMathJax(paste("Soil cohesion \\(c'\\) [", du()['c','unit_user'] ,']', sep='')),
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
  #soil angle of internal friction
  output$ui_phi <- renderUI({
    numericInput(
      "phi",
      withMathJax(paste("Soil angle of internal friction \\(\\phi'\\) [", du()['phi','unit_user'], "]", sep='')),
      value = round(du()['phi','value_default']/du()['phi','unit_factor'],2)
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
    shiny_unitsystem(dp, input$lengthunit_radio, input$rootstressunit_radio, input$soilstressunit_radio, 1, 1)
  })

  ## GENERATE ROOT ORIENTATIONS
  do <- reactive({
    orientations_initial(
      input$ndimension,
      input$norientation,
      input$phirt * du()['phirt','unit_factor'],
      input$beta0max * du()['beta0max','unit_factor'],
      input$alphaoffset * du()['alphaoffset','unit_factor'],
      input$betaoffset * du()['betaoffset','unit_factor'],
      input_volume = FALSE
    )
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
    create_rootproperties(
      input$drmin, input$drmax, input$nc, input$phirt, input$betaphi,
      input$Lr0, input$betaL, input$tru0, input$betat, input$trytru,
      input$epsru0, input$betaeps, input$epsryepsru, input$kappat,
      dr0 = input$dr0, du = du()
    )
  })

  ## GENERATE ALL COMBINATIONS OF ROOT AND SOIL PROPERTIES
  da <- reactive({
    create_allorientationsproperties(dr(), do())
  })

  ## GENERATE SOIL PROPERTIES
  ds <- reactive({
    #validate input
    validate(
      need(input$c>=0, "Soil cohesion must be equal or larger than 0"),
      need(input$phi>=0, "Soil friction angle must be equal or larger than 0"),
      need(input$sign>=0, "Normal soil stress in shear zone must be equal or larger than 0"),
      need(input$taui>=0, "Root-soil interface friction cannot be negative"),
      need(input$h0>=0,   "Initial shear zone thickness cannot be negative"),
      need(input$hmax>=input$h0, "Maximum shear zone thickness cannot be smaller than initial shear zone thickness"),
      need(input$usmax>0, "Final shear displacements needs to be a positive number"),
      need(input$nstep>0, "Need a positive number of displacements steps"),
      need(input$nstep%%1==0, "Number of shear displacements steps needs to be a positive integer")
    )
    #create soil properties
    data.frame(
      c = input$c * du()['c','unit_factor'],
      phi = input$phi * du()['phi','unit_factor'],
      sign = input$sign * du()['sign','unit_factor'],
      taui = input$taui * du()['taui','unit_factor'],
      h0 = input$h0 * du()['h0','unit_factor'],
      hmax = input$hmax * du()['hmax','unit_factor'],
      usmax = input$usmax * du()['usmax','unit_factor'],
      nstep = input$nstep
    )
  })

  ## GENERATE ROOT ORIENTATION PLOTS
  #generate plot grid
  dgrid <- reactive({
    1
  })
  #polar plot root orientations
  output$p_rootorientations2D <- plotly::renderPlotly({
    plotly_orientations_2D(
      do(),
      input$ndimension,
      input$beta0max * du()['beta0max','unit_factor'],
      alphaoffset = input$alphaoffset * du()['alphaoffset','unit_factor'],
      betaoffset = input$betaoffset * du()['betaoffset','unit_factor'],
      dgrid = NULL
    )
  })
  #3D plot root orientations
  output$p_rootorientations3D <- plotly::renderPlotly({
    plotly_orientations_3D(
      do(),
      input$ndimension,
      input$beta0max * du()['beta0max','unit_factor'],
      alphaoffset = input$alphaoffset * du()['alphaoffset','unit_factor'],
      betaoffset = input$betaoffset * du()['betaoffset','unit_factor'],
      dgrid = NULL
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

  ## PLOT SOIL PROPERTY PLOTS
  #soil mohr-coulomb
  output$p_soilyieldcriterion <- plotly::renderPlotly({
    plotly_soilyieldcriterion(ds()[1,], du = du())
  })

  ## CALCULATE TAB
  ## Calculate reinforcements using DRAM
  #If button for calculation is pressed - create calculated values in SI units
  dout <- eventReactive(
    input$buttonCalculate, {
    ## Create progress bar
    #progress update function - is called in calculation function
    updateProgress <- function() {
      progress$inc(amount = 1/input$nstep)
    }
    #create progress object in Shiny
    progress <- shiny::Progress$new()
    #set initial value
    progress$set(message = "Calculating root-reinforcements", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    ## run analysis
    dram_runanalysis(da(), ds(), updateProgress)
  })

  ## postprocessing results
  #analyse root fractions slipping, breaking etc - add to summary data
  dsum <- reactive({
    postprocess_roottypes(dout()$sum, dout()$all, da())
  })
  #output$table <- renderTable(dsum())

  ## Create output plots - DRM
  #root-reinforcement
  output$p_reinforcement <- plotly::renderPlotly({
    plotly_reinforcement(dout()$sum, du = du())
  })
  #shear zone thickness
  output$p_shearzonethickness <- plotly::renderPlotly({
    plotly_shearzonethickness(dout()$sum, du = du())
  })
  #fractions
  output$p_behaviourfractions <- plotly::renderPlotly({
    plotly_behaviourfractions(dsum(), du = du())
  })

  ## DOWNLOAD DRM input and output
  ## Download output
  #check if calculate button has been pressed at least once
  calcpressed <- eventReactive(input$buttonCalculate, T)
  #show download output button (only if calculation button has been pressed at least once)
  output$DownloadOutputButton <- renderUI({
    if (calcpressed() ==T) {
      downloadButton('DownloadOutput', 'Download Output')
    }
  })
  ## provide download option - output
  output$DownloadOutput <- downloadHandler(
    filename = function() {
      paste('Output_model','.csv', sep = "")
    },
    content = function(file) {
      write.csv(
        datasave_outputparameters(dsum(), du = du()),
        file,
        row.names = FALSE
      )
    }
  )
  ## Download input
  #provide download option - input
  output$DownloadInput <- downloadHandler(
    filename = function() {
      paste('Input_model','.csv', sep = "")
    },
    content = function(file) {
      write.csv(
        datasave_inputparameters(input, norientation_used = nrow(do()), du = du()),
        file,
        row.names = FALSE
      )
    }
  )

  ##Update input widgets also when they are hidden (Shiny only assigns/changes a value when the input is visible)
  ##otherwise, plotting some graphs without manually visiting all input tabs first will throw an error.
  #root diameter
  outputOptions(output, 'ui_phirt', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_drmin', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_drmax', suspendWhenHidden=FALSE)
  #root orientations
  outputOptions(output, 'ui_beta0max', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_alphaoffset', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_betaoffset', suspendWhenHidden=FALSE)
  #root properties
  outputOptions(output, 'ui_dr0', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_tru0', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_epsru0', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_Lr0',  suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_trytru', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_epsryepsru', suspendWhenHidden=FALSE)
  #soil properties
  outputOptions(output, 'ui_c', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_phi', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_sign', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_taui', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_h0', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_hmax', suspendWhenHidden=FALSE)
  #analysis settings
  outputOptions(output, 'ui_usmax', suspendWhenHidden=FALSE)
}

