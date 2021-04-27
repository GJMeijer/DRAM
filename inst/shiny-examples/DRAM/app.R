#v0.1 - first working version
#v0.2 - added existing root-reinforcement prediction models
#v0.3 - incorporated JAK's suggestions for improvements: 3D plot of root orientations, user-defined unit sytem
#v0.4 - improvement of efficiency compared to v0.3
#v0.5 - added new scheme for 3D root orientations - based on grid approach
#v0.6 - power law root volume distribution function (instead of linear)


#####################
### LOAD PACKAGEs ###
#####################

#load packages
suppressWarnings(library('shiny'))        # to run Shiny app
suppressWarnings(library('shinythemes'))  # to load Shiny themes library
suppressWarnings(library('plotly'))       # to create interactive plots
suppressWarnings(library('tidyr'))        # data handling
suppressWarnings(library('dplyr'))        # data handling
suppressWarnings(library('RColorBrewer')) # color selection in plots

#load functions
source("Functions/20190812_WWMdisks_inputprocessing_functions.R")
source("Functions/20190812_WWMdisks_calculation_functions.R")
source("Functions/20190812_WWMdisks_plotting_functions.R")
source("Functions/20190812_WWMdisks_output_functions.R")
source("Functions/20190812_WWMdisks_othermodels_functions.R")


#########################
### INITIAL PROCESSES ###
#########################

#Load input default parameters
din <- read.csv('Functions/parameter_list.csv', stringsAsFactors=F)
#add parameter names as row names for easy access
rownames(din) <- din$Parameter


#################
### DEFINE UI ###
#################

#create tab structure
ui <- navbarPage(
  ## DETAILS
  title="Dundee Root Analysis Model",     #Application name, appears in top left corner
  theme=shinytheme("cerulean"),  #CSS theme used for formatting of the application. Theme loaded from the 'shinythemes' package
  
  ## INSTRUCTIONS
  tabPanel("Notes",
    #first part of rmarkdown
    includeMarkdown("Notes/Notes_rmarkdown_pt1.Rmd"),
    #download buttons for files
    fluidRow(
      column(6,
        wellPanel(
          actionButton(inputId='ab1', label="Link to journal publication", 
                      icon = icon("link"), 
                      onclick ="window.open('https://discovery.dundee.ac.uk/en/persons/gerrit-meijer', '_blank')")
        )
      ),
      column(6,
        wellPanel(
          actionButton(inputId='ab2', label="Manual for web application", 
                       icon = icon("file-pdf"), 
                       onclick ="window.open('20191016_DundeeRootModel_ShinyManual.pdf', '_blank')")
        )
      )
    ),
    #second part of rmarkdown
    includeMarkdown("Notes/Notes_rmarkdown_pt2.Rmd"),
    #buttons for units
    fluidRow(
      column(4,
        wellPanel(
          #diameter/length
          radioButtons("lengthunit_radio", 'Unit for length', choices=list("mm"=1, "cm"=2, "m"=3), selected=1)
        )
      ),
      column(4,
        wellPanel(
          #stress/strength
          radioButtons("stressunit_radio", 'Unit for stress/strength', choices=list("Pa"=1, "kPa"=2, "MPa"=3, "GPa"=4), selected=2)
        )
      ),
      column(4,
        wellPanel(
          #root reference diameter
          uiOutput("ui_drref")
        )
      )
    ),
    #rmarkdown
    includeMarkdown("Notes/Notes_rmarkdown_pt3.Rmd")
  ),
  
  ## ROOT VOLUME FRACTION AND DIAMETER
  tabPanel("Root diameters",
    #load mathjax for typesetting equations (has to be done only once)
    withMathJax(),
    #create input
    fluidRow(
      column(6,
        wellPanel(
          #header
          h3("Root volume fraction settings"),
          #root volume fraction
          uiOutput("ui_phir"),
          #root distribution parameter
          numericInput("bphi", 
                      paste('Root volume distribution - power \\(b_{\\phi}\\) [-]', sep=''),
                      value=din['bphi','Default'])
        )
      ),
      column(6,
        wellPanel(
          #header
          h3("Root diameter settings"),
          #root diameter range
          uiOutput("ui_drmin"),
          uiOutput("ui_drmax"),
          #number of root diameters
          #number of discrete root diameters
          sliderInput("nd", 
                      paste('Number of discrete diameter classes \\(n_{d}\\) [-]', sep=''),
                      value=din['nd','Default'],
                      min=  din['nd','Min'],
                      max=  din['nd','Max'])
        )
      )
    ),
    br(),
    fluidRow(
      column(6, 
        #plot distribution of root volume over root diameter classes
        plotlyOutput("p_diameters")
      ), 
      column(6,
      )
    ),
  ),
  
  ## ROOT ORIENTATIONS
  tabPanel("Root orientations",
    fluidRow(
      column(6,
        wellPanel(
          #header
          h3("General orientation settings"),
          #number of root orientation dimensions
          radioButtons("ndim", 'Root orientation dimensions \\(n_{dim}\\) [-]', choices=list("1-D"=1, "2-D"=2, "3-D"=3), selected=3),      
          #requested approximate number of root orientations 
          sliderInput("nori", 
                      paste('Requested number of discrete root orientations \\(n_{ori}\\) [-]', sep=''),
                      value=din['nori','Default'],
                      min=  din['nori','Min'],
                      max=  din['nori','Max'])          

        )
      ),
      column(6,
        wellPanel(
          #header
          h3("Root angle settings"),
          #max root elevation
          sliderInput("beta0max", 
                      paste('Maximum root elevation angle \\(\\beta_{0,max}\\) [deg]', sep=''),
                      value=din['beta0max','Default'],
                      min=  din['beta0max','Min'],
                      max=  din['beta0max','Max'],
                      step= din['beta0max','Step']),
          
          #centre axis offset - azimuth
          sliderInput("alphaoffset", 
                      paste('Azimuth offset \\(\\alpha_{offset}\\) [deg]', sep=''),
                      value=din['alphaoffset','Default'],
                      min=  din['alphaoffset','Min'],
                      max=  din['alphaoffset','Max'],
                      step= din['alphaoffset','Step']),
          #centre axis offset - elevation
          sliderInput("betaoffset", 
                      paste('Elevation offset \\(\\beta_{offset}\\) [deg]', sep=''),
                      value=din['betaoffset','Default'],
                      min=  din['betaoffset','Min'],
                      max=  din['betaoffset','Max'],
                      step= din['betaoffset','Step'])
        )
      )
    ),
    br(),
    fluidRow(
      #plot root orienations - polar plot
      plotlyOutput("p_rootorientations2D")
    ), 
    br(),
    fluidRow(
      #plot root orienations - 3D plot
      plotlyOutput("p_rootorientations3D")
    ),
  ),
  
  ## ROOT PROPERTIES
  tabPanel("Root properties",
    fluidRow(
      column(6,
        wellPanel(
          #header
          h3("Root peak strength and strain"),
          #root tensile strength - power law - multiplier
          uiOutput("ui_at"),
          #root tensile strength - power law - power
          numericInput("bt", 
                       paste('Tensile strength power law - power \\(b_t\\) [-]', sep=''),
                       value=din['bt','Default']),
          
          #root tensile strain to failure  - power law - multiplier
          uiOutput("ui_aepsilon"),
          #root tensile strain to failure  - power law - power
          numericInput("bepsilon", 
                       paste('Tensile strain to failure power law - power \\(b_\\epsilon\\) [-]', sep=''),
                       value=din['bepsilon','Default'])
        ),
        wellPanel(
          #header
          h3("Root failure smoothing (Weibull)"),
          #Weibull distribution shape parameter
          numericInput("kappat", 
                       paste('Weibull tensile strength shape parameter \\(\\kappa_t\\) [-]', sep=''),
                       value=din['kappat','Default'])
        )          
      ),
      column(6, 
        wellPanel(
          #header
          h3("Root length"),
          #Root length - multiplier
          uiOutput("ui_aL"),
          #Root length - power
          numericInput("bL", 
                        paste('Root length power law - power \\(b_L\\) [-]', sep=''),
                        value=din['bL','Default'])
        ),
        wellPanel(
          #header
          h3("Root yield strength and strain"),
          #yield/ultimate strength ratio
          uiOutput("ui_trytru"),
          #yield/ultimate strain ratio
          uiOutput("ui_eryeru")
        )
      )
    ),
    br(),
    fluidRow(
      column(6, 
        #plot tensile strength as function of root diameter
        plotlyOutput("p_tru")
      ), 
      column(6,
        #plot root length as function of root diameter
        plotlyOutput("p_Lr")
      )
    ),
    br(),
    fluidRow(
      column(6,
        #plot root strain to failure as function of root diameter
        plotlyOutput("p_eru")
      ), 
      column(6,
        #plot root normalised stress-strain curve
        plotlyOutput("p_stressstrain")
      )
    )
  ),
  
  ## SOIL PROPERTIES 
  tabPanel("Soil properties",
    fluidRow(
      column(6,
        wellPanel(
          #header
          h3("Soil properties"),
          #soil cohesion
          uiOutput("ui_c"),
          #soil angle of internal friction
          
          numericInput("phi", 
                       paste("Soil angle of internal friction \\(\\phi'\\) [deg]", sep=''),
                       value=din['phi','Default'])
        ),
        wellPanel(
          #header
          h3("Shear zone properties"),
          #effective soil stress on shear plane
          uiOutput("ui_sign"),
          #initial shear zone thickness
          uiOutput("ui_h0"),
          #maximum shear zone thickness
          uiOutput("ui_hmax")
        )
      ),
      column(6, 
        wellPanel(
          #header
          h3("Root-soil interface"),  
          #soil-root interface friction
          uiOutput("ui_taui")
        ),
        wellPanel(
          #header
          h3("Shear displacement settings"),  
          #maximum shear displacement
          uiOutput("ui_umax"),
          #number of shear displacement steps
            numericInput("nstep", 
                         paste("Number of shear displacement steps \\(n_{step}\\) [-]", sep=''),
                         value=din['nstep','Default'])
        )
      )
    ),
    br(),
    fluidRow(
      column(6, 
        #plot Mohr-Coulomb failure criterion
        plotlyOutput("p_soilyieldcriterion")
      ), 
      column(6,
      )
    ),
  ),
  
  ## CALCULATE
  tabPanel("Calculate",
    ## Input and buttons
    fluidRow(
      column(4,
        wellPanel(
          #make action button to do calculation
          actionButton("buttonCalculate", "Start calculations")
        )
      ), 
      column(4, 
        wellPanel(          
          #create download button for input parameters
          downloadButton("DownloadInput", "Download Input")
        )
      ),
      column(4, 
        wellPanel(          
          #create download button for output parameters
          uiOutput("DownloadOutputButton")
        )
      )
    ),
    ## Plots      
    fluidRow(
      column(6, 
        #plot DRM root-reinforcement as function of shear displacement
        plotlyOutput("p_reinforcement")
      ), 
      column(6,
        #plot DRM shear zone thickness as function of shear displacement
        plotlyOutput("p_shearzonethickness")
      )
    ),
    br(),
    fluidRow(
      #plot root behaviour fractions as function of shear displacement
      plotlyOutput("p_behaviourfractions")
    )
  ),
  
  ## OTHER MODELS
  tabPanel("Comparison to existing models",
    fluidRow(
      column(6,
        wellPanel(
          #Wu/Waldron factor - input
          numericInput("wwmk", "Wu/Waldron orientation factor \\(k'\\) [-]", value=1.2)
        )
      ),
      column(6,
        wellPanel(
          #Fibre bundle load sharing factor - input
          numericInput("fbmfactor", "Fibre Bundle Model load sharing factor \\(a\\) [-]", value=0)
        )
      )
    ),
    br(),
    fluidRow(
      column(6, 
        #plot RBM+RBMw results - reinforcement as function of tensile strain
        plotlyOutput("p_rbm")
      ), 
      column(6,
        #plot FBM results - reinforcement as function of breaking root diameter, + WWM
        plotlyOutput("p_fbm")
      )
    ),
    br(),
    fluidRow(
      column(6, 
        #plot barplot of all peak reinforcements calculated with all models
        plotlyOutput("p_cru")
      ), 
      column(6,
      )
    )
  )
)



#####################
### DEFINE SERVER ###
#####################

# Define server 
server <- function(input, output) {

  ## Create unit system and default/min/max/step settings required for UI input widgets
  #load from external file
  du <- reactive({ 
    f_inputunitsystem(input$lengthunit_radio, input$stressunit_radio)
  })
  
  ## Create UI objects for input that is not dimensionless - Root diameters
  #root volume fraction
  output$ui_phir <- renderUI({
    numericInput("phir", 
                 withMathJax(paste('Total root volume fraction \\(\\phi_{rt}\\) [', du()['length','unit_user'], '^3/', du()['length','unit_user'] ,'^3]', sep='')),
                 value=din['phir','Default'],
                 min=  din['phir','Min'],
                 max=  din['phir','Max'])
  })
  #minimum root diameter
  output$ui_drmin <- renderUI({
    numericInput("drmin", 
                 withMathJax(paste('Minimum root diameter \\(d_{r,min}\\) [', du()['length','unit_user'] ,']', sep='')),
                 value=din['drmin','Default']/du()['length','factor'])
  })
  #maximum root diameter
  output$ui_drmax <- renderUI({
    numericInput("drmax", 
                 withMathJax(paste('Maximum root diameter \\(d_{r,max}\\) [', du()['length','unit_user'] ,']', sep='')),
                 value=din['drmax','Default']/du()['length','factor'])
  })

  ## Create UI objects for input that is not dimensionless - Root properties
  #Root tensile strength <at> 
  output$ui_at <- renderUI({
    numericInput("at", 
                 withMathJax(paste('Tensile strength power law - multiplier \\(a_t\\) [', du()['stress','unit_user'] ,']', sep='')),
                 value=din['at','Default']/du()['stress','factor'])
  })
  #Root tensile strain <aepsilon> 
  output$ui_aepsilon <- renderUI({
    numericInput("aepsilon", 
                 withMathJax(paste('Tensile strength power law - multiplier \\(a_t\\) [', du()['length','unit_user'], '/', du()['length','unit_user'] ,']', sep='')),
                 value=din['aepsilon','Default'])
  })
  #reference diameter
  output$ui_drref <- renderUI({
    numericInput("drref", 
                 withMathJax(paste('Root reference diameter \\(d_{r,ref}\\) [', du()['length','unit_user'] ,']', sep='')),
                 value=din['drref','Default']/du()['length','factor'])
  })
  #Root length - multiplier
  output$ui_aL <- renderUI({
    numericInput("aL", 
                 withMathJax(paste('Root length power law - multiplier \\(a_L\\) [', du()['length','unit_user'] ,']', sep='')),
                 value=din['aL','Default']/du()['length','factor'])
  })
  #yield/ultimate strength ratio
  output$ui_trytru <- renderUI({
    sliderInput("trytru", 
                withMathJax(paste('Ratio yield/ultimate tensile strength \\(t_{ry}/t_{ru}\\) [', du()['stress','unit_user'], '/', du()['stress','unit_user'] ,']', sep='')),
                value=din['trytru','Default'],
                min=  din['trytru','Min'],
                max=  din['trytru','Max'],
                step= din['trytru','Step'])
  })
  #yield/ultimate strain ratio
  output$ui_eryeru <- renderUI({
    sliderInput("eryeru", 
                withMathJax(paste('Ratio yield/ultimate tensile strain \\(\\epsilon_{ry}/\\epsilon_{ru}\\) [(', du()['length','unit_user'], '/', du()['length','unit_user'], ')/(', du()['length','unit_user'], '/', du()['length','unit_user'] ,')]', sep='')),
                value=din['eryeru','Default'],
                min=  din['eryeru','Min'],
                max=  din['eryeru','Max'],
                step= din['eryeru','Step'])
    
  })
  
  ## Create UI objects for input that is not dimensionless - Soil properties and analysis settings
  #soil cohesion
  output$ui_c <- renderUI({
    numericInput("c", 
                 withMathJax(paste('Soil cohesion \\(c\\) [', du()['stress','unit_user'] ,']', sep='')),
                 value=din['c','Default']/du()['stress','factor'])
  })
  #effective soil stress on shear plane
  output$ui_sign <- renderUI({
    numericInput("sign", 
                 withMathJax(paste("Normal effective soil stress on shear plane \\(\\sigma'_n\\) [", du()['stress','unit_user'] ,']', sep='')),
                 value=din['sign','Default']/du()['stress','factor'])
  })
  #initial shear zone thickness
  output$ui_h0 <- renderUI({
    numericInput("h0", 
                 withMathJax(paste("Shear zone thickness - initial \\(h_0\\) [", du()['length','unit_user'] ,']', sep='')),
                 value=din['h0','Default']/du()['length','factor'])
  })
  #maximum shear zone thickness
  output$ui_hmax <- renderUI({
    numericInput("hmax", 
                 withMathJax(paste("Shear zone thickness - maximum \\(h_{max}\\) [", du()['length','unit_user'] ,']', sep='')),
                 value=din['hmax','Default']/du()['length','factor'])
  })
  #soil-root interface friction
  output$ui_taui <- renderUI({
    numericInput("taui", 
                 withMathJax(paste("Root-soil interface frition \\(\\tau_i\\) [", du()['stress','unit_user'] ,']', sep='')),
                 value=din['taui','Default']/du()['stress','factor'])
  })
  #maximum shear displacement
  output$ui_umax <- renderUI({
    numericInput("umax", 
                 withMathJax(paste("Maximum shear displacement \\(u_{max}\\) [", du()['length','unit_user'] ,']', sep='')),
                 value=din['umax','Default']/du()['length','factor'])
  })

  ## Create dataframes with input parameters. All in SI units (radians, meters, Newtons, Pacals)
  #Root diameter input
  did <- reactive({
    #validate input
    validate(
      need(input$phir>=0,   "Root volume fractions need to be larger than zero"),
      need(input$phir<=1,   "Root volume fractions cannot be larger than 1 (100%)"),
      need(input$drmin>0,  "Minimum root diameter needs to be larger than 0"),
      need(input$drmax>=input$drmin, "Maximum root diameter needs to be larger than minimum root diameter"),
      need(input$drref>0,   "Reference root diameter needs to be larger than 0")
    )
    #dataframe with all input parameters
    data.frame(
      phir        = input$phir, 
      bphi        = input$bphi, 
      drmin       = input$drmin * du()['length','factor'],
      drmax       = input$drmax * du()['length','factor'], 
      nd          = input$nd,
      drref       = input$drref * du()['length','factor'])
  })
  #Root orientations
  dio <- reactive({
    data.frame(
      ndim        = input$ndim, 
      nori        = input$nori, 
      beta0max    = input$beta0max /180*pi,
      alphaoffset = input$alphaoffset /180*pi, 
      betaoffset  = input$betaoffset /180*pi)
  })
  #Root properties
  dip <- reactive({
    #validate input
    validate(
      need(input$at>0,      "Root tensile strength multiplier needs to be larger than 0"),
      need(input$aepsilon>0,"Root tensile strain to failure multiplier needs to be larger than 0"),
      need(input$aL>0,      "Root length coefficient needs to be larger than 0"),
      need(input$trytru>=0, "Root yield strength needs to be larger than 0"),
      need(input$trytru<=1, "Root yield strength needs to be smaller than ultimate strength"),
      need(input$eryeru>=0, "Root yield strain needs to be larger than 0"),
      need(input$eryeru<=1, "Root yield strain needs to be smaller than ultimate strain to failure"),
      need(!(input$eryeru==0 & input$trytru>0), "Root Young's modulus cannot be infinitely stiff (eryeru=0, trytru>0)"),
      need(!(input$eryeru==1 & input$trytru<1), "Root elastoplastic stiffness cannot be infinitely stiff (eryeru=1 (100%), trytru<1 (100%))"),
      need(!(input$eryeru<1 & input$trytru==1), "Root elastoplastic stiffness cannot be zero (eryeru<1 (100%), trytru==1 (100%))"),
      need(!(input$eryeru>0 & input$trytru==0), "Root Young's modulus cannot be zero (eryeru>0, trytru==0)"),      
      need(input$kappat>0,   "Weibull shape parameter needs to be larger than 0"),
      need(input$drref>0,   "Reference root diameter needs to be larger than 0")
    )
    #dataframe with all input parameters
    data.frame(
      at          = input$at * du()['stress','factor'], 
      bt          = input$bt, 
      aepsilon    = input$aepsilon, 
      bepsilon    = input$bepsilon,
      kappat      = input$kappat, 
      aL          = input$aL * du()['length','factor'], 
      bL          = input$bL, 
      trytru      = input$trytru, 
      eryeru      = input$eryeru,
      drref       = input$drref * du()['length','factor'])
  })
  #soil and analysis properties
  dis <- reactive({
    #validate input
    validate(
      #soil properties
      need(input$c>=0,    "Soil cohesion cannot be negative"),
      need(input$phi>=0,  "Root angle of internal friction cannot be negative"),
      need(input$sign>=0, "Normal effective stress in shear zone cannot be negative"),
      need(input$h0>=0,   "Initial shear zone thickness cannot be negative"),
      need(input$hmax>=input$h0, "Maximum shear zone thickness cannot be smaller than initial shear zone thickness"),
      need(input$taui>=0, "Root-soil interface friction cannot be negative"),
      #analysis settings
      need(input$umax>0, "Final shear displacements needs to be a positive number"),
      need(input$nstep>0, "Need a positive number of displacements steps"),
      need(input$nstep%%1==0, "Number of shear displacements steps needs to be a positive integer")
    )
    #dataframe with all input parameters
    data.frame(
      #soil properties
      c           = input$c * du()['stress','factor'], 
      phi         = input$phi /180*pi, 
      sign        = input$sign * du()['stress','factor'], 
      h0          = input$h0 * du()['length','factor'], 
      hmax        = input$hmax * du()['length','factor'], 
      taui        = input$taui * du()['stress','factor'],
      #analysis settings
      umax        = input$umax * du()['length','factor'], 
      nstep       = input$nstep)
  })    

  #Dataframe with all input parameters
  di <- reactive({
    cbind(did(), dio(), dip(), dis())
  })
  #Create root diameter classes
  dd <- reactive({
    f_rootDiameters(did())
  })
  #create all root orientations3
  do <- reactive({
    f_generateRootOrientations(dio())
  })
  #Create root properties - add to diameter classes defined earlier (dataframe <dd>)
  dp <- reactive({
    f_rootProperties(dd(), dip())
  })
  #combine all orientations and all root properties, in SI units
  dr <- reactive({
    f_CombineOrientationsRootproperties(do(), dp())
  })

  ## Create plots for input
  #diameter distribution
  output$p_diameters <- renderPlotly({plot_diameters(dd(), du())})
  #root tensile strength
  output$p_tru <- renderPlotly({plot_roottensilestrength(dp(), du())})
  #root tensile strain to failure
  output$p_eru <- renderPlotly({plot_roottensilestrain(dp(), du())})
  #root root length
  output$p_Lr <- renderPlotly({plot_rootlength(dp(), du())})
  #stress-strain curve
  output$p_stressstrain <- renderPlotly({plot_stressstraincurve(dp(), du())})
  #root orientaions 3D - preliminary calculations
  # - create all lines in unit sphere indicating root axes
  d3dl <- reactive({ f_rootpositionsline(f_polar2cartesian(do())) })
  # - create unit sphere
  d3dus <- reactive({
    if (input$ndim=='3') {
      f_coordssphere(beta0=c(input$beta0max/180*pi,pi-input$beta0max/180*pi), alphaoffset=input$alphaoffset/180*pi, betaoffset=input$betaoffset/180*pi)
    } else {
      f_coordssphere(beta0=c(0,pi), alphaoffset=input$alphaoffset/180*pi, betaoffset=input$betaoffset/180*pi)
    }
  })
  # - create orientation range
  d3dr <- reactive({
    if (input$ndim=='3') {
      f_coordssphere(beta0=c(0,input$beta0max/180*pi), alphaoffset=input$alphaoffset/180*pi, betaoffset=input$betaoffset/180*pi)    
    } else if (input$ndim=='2') {
      f_rootrange2D(beta0max=input$beta0max/180*pi, alphaoffset=input$alphaoffset/180*pi, betaoffset=input$betaoffset/180*pi)
    } else {
      1
    }
  })
  # - create grid lines for 3D orientations
  d3grid <- reactive({
    if (input$ndim=='3') {
      f_3Drootorientations_grid(input$nori, input$beta0max/180*pi, alphaoffset=input$alphaoffset/180*pi, betaoffset=input$betaoffset/180*pi)
    } else {
      1
    }
  })
  #root orientations 2D polar
  output$p_rootorientations2D <- renderPlotly({plot_rootorientations_2D(do(), dio(), d3grid(), input$ndim)})
  #root orientations 3D
  output$p_rootorientations3D <- renderPlotly({plot_rootorientations_3D(d3dl(), d3dus(), d3dr(), d3grid(), input$ndim)})
  #soil strength
  output$p_soilyieldcriterion <- renderPlotly({plot_soilyieldcriterion(dis(), du())})
  
  ## Calculate reinforcements using DRM
  #If button for calculation is pressed - create calculated values in SI units
  dout <- eventReactive(input$buttonCalculate, {
    ## Create progress bar
    #progress update function - is called in calculation function
    updateProgress <- function() {
      progress$inc(amount=1/dis()$nstep)
    }
    #create progress object in Shiny
    progress <- shiny::Progress$new()
    #set initial value
    progress$set(message = "Calculating root-reinforcements", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    ## run analysis
    f_runanalysis(dr(), di(), updateProgress)
  })
  # Split DRM output list in all data and summary data
  dout_all <- reactive({ dout()$all })
  dout_sum <- reactive({ dout()$sum })
  
  ## postprocessing results
  #analyse root fractions slipping, breaking etc
  dfrc <- reactive({
    f_fractions(dout_all(), dout_sum(), dr())
  })
  
  ## Create output plots - DRM
  #root-reinforcement
  output$p_reinforcement      <- renderPlotly({ plot_reinforcement(dout_sum(), du()) })
  #shear zone thickness
  output$p_shearzonethickness <- renderPlotly({ plot_shearzonethickness(dout_sum(), du()) })
  #fractions
  output$p_behaviourfractions <- renderPlotly({ plot_behaviourfractions(dfrc(), du()) })
  
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
      write.csv(f_dataframeoutputparameters(dfrc(), du()), file, row.names = FALSE)
    }
  )
  ## Download input
  #Create dataframe with input variables
  dinput_save <- reactive({ 
    f_dataframeinputparameters(input, dim(do())[1], du()) 
  })
  #provide download option - input
  output$DownloadInput <- downloadHandler(
    filename = function() {
      paste('Input_model','.csv', sep = "")
    },
    content = function(file) {
      write.csv(dinput_save(), file, row.names = FALSE)
    }
  )
  
  ## OTHER MODELS
  ## Calculations
  #Wu/Waldron
  dwwm <- reactive({ 
    f_WWM(dp(), input$wwmk) 
  })
  #Fibre bundle model
  dfbm <- reactive({ 
    f_FBM(dp(), input$fbmfactor, input$wwmk) 
  })
  #RBM
  drbm <- reactive({
    f_RBMw(dp(), input$wwmk, SuddenBreak=T)
  })
  #RBMw
  drbmw <- reactive({
    f_RBMw(dp(), input$wwmk, SuddenBreak=F)
  })
  ## Plots
  #Fibre bundle model
  output$p_fbm <- renderPlotly({plot_fibrebundlemodel(dfbm(), dwwm(), du()) })
  #RBMw
  output$p_rbm <- renderPlotly({plot_rootbundlemodel(drbm(), drbmw(), du()) }) 
  #all reinforcements
  output$p_cru <- renderPlotly({f_predictionsallbarplot(dwwm(), dfbm(), drbm(), drbmw(), dout_sum(), input$wwmk, du()) })
  
  ##Update input widgets also when they are hidden (Shiny only assigns/changes a value when the input is visible)
  ##otherwise, plotting some graphs without manually visiting all input tabs first will throw an error.
  #root diameter
  outputOptions(output, 'ui_phir', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_drmin', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_drmax', suspendWhenHidden=FALSE)
  #root properties
  outputOptions(output, 'ui_drref', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_at', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_aepsilon', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_aL',  suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_trytru', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_eryeru', suspendWhenHidden=FALSE)
  #soil properties
  outputOptions(output, 'ui_c', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_sign', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_taui', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_h0', suspendWhenHidden=FALSE)
  outputOptions(output, 'ui_hmax', suspendWhenHidden=FALSE)
  #analysis settings
  outputOptions(output, 'ui_umax', suspendWhenHidden=FALSE)
}


#################
### BUILD APP ###
#################

shinyApp(ui = ui, server = server)