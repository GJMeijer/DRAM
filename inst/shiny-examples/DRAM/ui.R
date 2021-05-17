#################
### DEFINE UI ###
#################

#create tab structure
ui <- navbarPage(
  ## DETAILS
  title = "Dundee Root Analytical Model",     #Application name, appears in top left corner
  position = "fixed-top",
  collapsible = TRUE,
  #theme=shinytheme("cerulean"),  #CSS theme used for formatting of the application. Theme loaded from the 'shinythemes' package

  ## INSTRUCTIONS
  tabPanel(
    "Notes",
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    #first part of rmarkdown
    includeMarkdown("notes/DRAM_notes_rmarkdown_pt1.Rmd"),
    #download buttons for files
    fluidRow(
      column(
        6,
        wellPanel(
          actionButton(
            inputId = 'ab1',
            label = "Link to journal publication",
            icon = icon("link"),
            onclick = "window.open('https://researchportal.bath.ac.uk/en/persons/gerrit-meijer', '_blank')"
          )
        )
      ),
      column(
        6,
        wellPanel(
          actionButton(
            inputId = 'ab2',
            label = "App manual",
            icon = icon("file-code"),
            onclick = "window.open('DRAM_manual_markdown.html', '_blank')"
          )
        )
      )
    ),
    #second part of rmarkdown
    includeMarkdown("notes/DRAM_notes_rmarkdown_pt2.Rmd"),
    #buttons for units
    fluidRow(
      column(
        2,
        wellPanel(
          #diameter/length
          radioButtons(
            "lengthunit_radio",
            'Unit for length, diameter and displacement',
            choices = list("mm" = 1, "cm" = 2, "m" = 3),
            selected = 1
          )
        )
      ),
      column(
        2,
        wellPanel(
          #root stress/strength
          radioButtons(
            "rootstressunit_radio",
            'Unit for root stress and strength',
            choices = list("Pa" = 1, "kPa" = 2, "MPa" = 3, "GPa" = 4),
            selected = 3
          )
        )
      ),
      column(
        2,
        wellPanel(
          #soil stress/strength
          radioButtons(
            "soilstressunit_radio",
            'Unit for soil stress and strength',
            choices = list("Pa" = 1, "kPa" = 2, "MPa" = 3, "GPa" = 4),
            selected = 2
          )
        )
      ),
      column(
        2,
        wellPanel(
          #root strain
          radioButtons(
            "rootstrainunit_radio",
            'Unit for root strain',
            choices = list("mm/mm" = 1, "%" = 2),
            selected = 1
          )
        )
      ),
      column(
        2,
        wellPanel(
          #root area ratio
          radioButtons(
            "rootarearatiounit_radio",
            'Unit for root area ratio',
            choices = list("mm2/mm2" = 1, "%" = 2),
            selected = 1
          )
        )
      ),
      column(
        2,
        wellPanel(
          #root reference diameter
          uiOutput("ui_dr0")
        )
      )
    ),
    #rmarkdown
    includeMarkdown("notes/DRAM_notes_rmarkdown_pt3.Rmd")
  ),

  ## ROOT VOLUME FRACTION AND DIAMETER
  tabPanel(
    "Root diameters",
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    #load mathjax for typesetting equations (has to be done only once)
    withMathJax(),
    #create input
    fluidRow(
      column(
        6,
        wellPanel(
          #header
          h3("Root volume fraction settings"),
          #root volume fraction
          uiOutput("ui_phirt"),
          #root distribution parameter
          numericInput(
            "betaphi",
            paste('Root volume distribution - power \\(\\beta_{\\phi}\\) [-]', sep=''),
            value = dp['betaphi','value_default'])
          )
        ),
      column(
        6,
        wellPanel(
          #header
          h3("Root diameter settings"),
          #root diameter range
          uiOutput("ui_drmin"),
          uiOutput("ui_drmax"),
          #number of discrete root diameters
          sliderInput(
            "nc",
            paste('Number of discrete diameter classes \\(n_{c}\\) [-]', sep=''),
            value = dp['nc','value_default'],
            min = dp['nc','value_min'],
            max = dp['nc','value_max'])
          )
        )
      ),
    br(),
    fluidRow(
      column(
        6,
        #plot distribution of root volume over root diameter classes
        plotly::plotlyOutput("p_diameters")
      ),
      column(
        6,
      )
    ),
  ),

  ## ROOT ORIENTATIONS
  tabPanel(
    "Root orientations",
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    fluidRow(
      column(
        6,
        wellPanel(
          #header
          h3("General orientation settings"),
          #number of root orientation dimensions
          radioButtons(
            "ndimension",
            'Root orientation dimensions \\(n_{dimension}\\) [-]',
            choices = list(
              "1-D" = 1,
              "2-D" = 2,
              "3-D" = 3
            ),
            selected = 3
          ),
          #requested approximate number of root orientations
          sliderInput(
            "norientation",
            paste('Requested number of discrete root orientations \\(n_{orientation}\\) [-]', sep=''),
            value = dp['norientation','value_default'],
            min = dp['norientation','value_min'],
            max = dp['norientation','value_max']
          )
        )
      ),
      column(
        6,
        wellPanel(
          #header
          h3("Root angle settings"),
          #max root elevation
          uiOutput("ui_beta0max"),
          #centre axis offset - azimuth
          uiOutput("ui_alphaoffset"),
          #centre axis offset - elevation
          uiOutput("ui_betaoffset")
        )
      )
    ),
    br(),
    fluidRow(
      #plot root orienations - polar plot
      plotly::plotlyOutput("p_rootorientations2D")
    ),
    br(),
    fluidRow(
      #plot root orienations - 3D plot
      plotly::plotlyOutput("p_rootorientations3D")
    ),
  ),

  ## ROOT PROPERTIES
  tabPanel(
    "Root properties",
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    fluidRow(
      column(
        6,
        wellPanel(
          #header
          h3("Root peak strength and strain"),
          #root tensile strength - power law - multiplier
          uiOutput("ui_tru0"),
          #root tensile strength - power law - power
          numericInput(
            "betat",
            paste('Tensile strength power law - power \\(\\beta_t\\) [-]', sep=''),
            value = dp['betat','value_default']
          ),
          #root tensile strain to failure  - power law - multiplier
          uiOutput("ui_epsru0"),
          #root tensile strain to failure  - power law - power
          numericInput(
            "betaeps",
            paste('Tensile strain to failure power law - power \\(\\beta_\\epsilon\\) [-]', sep=''),
            value = dp['betaeps','value_default']
          )
        ),
        wellPanel(
          #header
          h3("Root failure smoothing (Weibull)"),
          #Weibull distribution shape parameter
          numericInput(
            "kappat",
            paste('Weibull tensile strength shape parameter \\(\\kappa_t\\) [-]', sep=''),
            value = dp['kappat','value_default']
          )
        )
      ),
      column(
        6,
        wellPanel(
          #header
          h3("Root length"),
          #Root length - multiplier
          uiOutput("ui_Lr0"),
          #Root length - power
          numericInput(
            "betaL",
            paste('Root length power law - power \\(\\beta_L\\) [-]', sep=''),
            value = dp['betaL','value_default']
          )
        ),
        wellPanel(
          #header
          h3("Root yield strength and strain"),
          #yield/ultimate strength ratio
          uiOutput("ui_trytru"),
          #yield/ultimate strain ratio
          uiOutput("ui_epsryepsru")
        )
      )
    ),
    br(),
    fluidRow(
      column(
        6,
        #plot tensile strength as function of root diameter
        plotly::plotlyOutput("p_tensilestrength")
      ),
      column(
        6,
        #plot root length as function of root diameter
        plotly::plotlyOutput("p_rootlength")
      )
    ),
    br(),
    fluidRow(
      column(
        6,
        #plot root strain to failure as function of root diameter
        plotly::plotlyOutput("p_tensilestrain")
      ),
      column(
        6,
        #plot root normalised stress-strain curve
        plotly::plotlyOutput("p_stressstrain")
      )
    )
  ),

  ## SOIL PROPERTIES
  tabPanel(
    "Soil properties",
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    fluidRow(
      column(
        6,
        wellPanel(
          #header
          h3("Soil properties"),
          #soil cohesion
          uiOutput("ui_c"),
          #soil angle of internal friction
          uiOutput("ui_phi")
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
      column(
        6,
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
          uiOutput("ui_usmax"),
          #number of shear displacement steps
          numericInput(
            "nstep",
            paste("Number of shear displacement steps \\(n_{step}\\) [-]", sep=''),
            value = dp['nstep','value_default']
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(
        6,
        #plot Mohr-Coulomb failure criterion
        plotly::plotlyOutput("p_soilyieldcriterion")
      ),
      column(
        6,
      )
    ),
  ),

  ## CALCULATE
  tabPanel(
    "Calculate",
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    ## Input and buttons
    fluidRow(
      column(
        2,
        wellPanel(
          #make action button to do calculation
          actionButton(
            "buttonCalculate",
            HTML("Start<br>calculations")
          )
        )
      ),
      column(
        4,
        wellPanel(
          #create download button for input parameters
          downloadButton(
            "DownloadInput",
            HTML("Download input<br>parameters")
          ),
          #create download button for root properties and orientations
          downloadButton(
            "DownloadRootInput",
            HTML("Download all root<br>orientations and properties")
          )
        )
      ),
      column(
        3,
        wellPanel(
          #create download button for output parameters (summary)
          uiOutput("DownloadOutputButton")
        )
      ),
      column(
        3,
        wellPanel(
          #create download button for output parameters (per root)
          uiOutput("DownloadOutputAllButton")
        )
      )
    ),
    ## Plots
    fluidRow(
      column(
        6,
        #plot DRM root-reinforcement as function of shear displacement
        plotly::plotlyOutput("p_reinforcement")
      ),
      column(
        6,
        #plot DRM shear zone thickness as function of shear displacement
        plotly::plotlyOutput("p_shearzonethickness")
      )
    ),
    br(),
    fluidRow(
      #plot root behaviour fractions as function of shear displacement
      plotly::plotlyOutput("p_behaviourfractions")
    )
  ),

  ## OTHER MODELS
  tabPanel(
    "Comparison to existing models",
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    fluidRow(
      column(
        6,
        wellPanel(
          #header
          h3("Commonly used models"),
          #Wu/Waldron factor - input
          numericInput(
            "k",
            "Root orientation factor \\(k'\\) [-]",
            value = 1.2
          ),
          #Fibre bundle load sharing factor - input
          numericInput(
            "kappa",
            "Weibull survival function shape parameter \\(\\kappa\\) [-] (RBMw, FBMw, FBMcw)",
            value = 4
          ),
        )
      ),
      column(
        6,
        wellPanel(
          #header
          h3("Generic fibre bundle models (FBMc/FBMcw)"),
          #Load sharing paramete in FBMcw
          numericInput(
            "betaF",
            "Generic load sharing parameter \\(\\beta_F\\) [-] (FBMc, FBMcw)",
            value = 1
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(
        6,
        #plotly with peak reinforcements for existing models
        plotly::plotlyOutput("p_existingmodelsbarplot1")
      ),
      column(
        6,
        #plot FBM results - reinforcement as function of breaking root diameter, + WWM
        plotly::plotlyOutput("p_existingmodelsbarplot2")
        #tableOutput('table')
      )
    )
  )
)
##tableOutput('table'),
