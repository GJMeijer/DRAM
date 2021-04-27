#################
### DEFINE UI ###
#################

#create tab structure
ui <- navbarPage(
  ## DETAILS
  title="Dundee Root Analysis Model",     #Application name, appears in top left corner

  #theme=shinytheme("cerulean"),  #CSS theme used for formatting of the application. Theme loaded from the 'shinythemes' package

  ## INSTRUCTIONS
  tabPanel(
    "Notes",
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
            onclick = "window.open('https://discovery.dundee.ac.uk/en/persons/gerrit-meijer', '_blank')"
          )
        )
      ),
      column(
        6,
        wellPanel(
          actionButton(
            inputId = 'ab2',
            label = "Manual for web application",
            icon = icon("file-pdf"),
            onclick = "window.open('20191016_DundeeRootModel_ShinyManual.pdf', '_blank')"
          )
        )
      )
    ),
    #second part of rmarkdown
    includeMarkdown("notes/DRAM_notes_rmarkdown_pt2.Rmd"),
    #buttons for units
    fluidRow(
      column(
        3,
        wellPanel(
          #diameter/length
          radioButtons(
            "lengthunit_radio",
            'Unit for length',
            choices = list("mm" = 1, "cm" = 2, "m" = 3),
            selected = 1
          )
        )
      ),
      column(
        3,
        wellPanel(
          #root stress/strength
          radioButtons(
            "rootstressunit_radio",
            'Unit for root stress/strength',
            choices = list("Pa" = 1, "kPa" = 2, "MPa" = 3, "GPa" = 4),
            selected = 3
          )
        )
      ),
      column(
        3,
        wellPanel(
          #soil stress/strength
          radioButtons(
            "soilstressunit_radio",
            'Unit for soil stress/strength',
            choices = list("Pa" = 1, "kPa" = 2, "MPa" = 3, "GPa" = 4),
            selected = 2
          )
        )
      ),
      column(
        3,
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

  ## ROOT PROPERTIES
  tabPanel(
    "Root properties",
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
    fluidRow(
      column(
        6,
        wellPanel(
          #header
          h3("Soil properties"),
          #soil cohesion
          uiOutput("ui_c"),
          #soil angle of internal friction
          numericInput(
            "phi",
            paste("Soil angle of internal friction \\(\\phi'\\) [deg]", sep=''),
            value=din['phi','Default']
          )
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
          uiOutput("ui_umax"),
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
        plotlyOutput("p_soilyieldcriterion")
      ),
      column(
        6,
      )
    ),
  )
)
