# File containing functions required to plot certain things
# 28/05/2019 - G.J. Meijer


###############################################
### PLOT INPUT - ROOT DIAMETER DISTRIBUTION ###
###############################################

plot_diameters <- function(dop, diu, nsignif=4) {
  # FUNCTION to generate a plotly plot with all root diameter classes and root volume fraction assigned to each class
  # INPUT
  # - <dop>: dataframe containing all discrete root diameter classes. 
  #          units system used is input unit system. datafrae requires fields:
  #         - dr:          Average diameter of root in class (=0.5*(dr_classmax+dr_classmin))
  #         - dr_classmin: Maximum root diameter in class
  #         - dr_classmax: Minimum root diameter in class
  #         - phir:        Root volume fraction assigned to each class
  # - <diu>: dataframe containing unit system
  # OPTIONAL INPUT
  # - <nsignif>:           Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #units used
  u_x   <- diu['length','unit_user']
  u_y   <- paste(diu['length','unit_user'], '^3/', diu['length','unit_user'], '^3', sep='')
  #factors used
  fac_x <- diu['length','factor']
  fac_y <- 1
  
  #Create hover text - diameter range + total root fraction
  dop$HoverText <- with(dop, paste('Root diameter (average): ', signif(dr/fac_x,nsignif), ' [',u_x,']', 
                                  '<br>', 'Root diameter (range): ', signif(dr_classmin/fac_x,nsignif), '-', signif(dr_classmax/fac_x,nsignif), ' [',u_x,']', 
                                  '<br>', 'Root volume fraction in diameter class: ', signif(phir/fac_y,nsignif), ' [',u_y,']', 
                                  sep=''))
  #create plotly object
  p <- plot_ly(
    data = dop,
    x = ~dr/fac_x,
    y = ~phir/fac_y,
    type='scatter', 
    mode='lines+markers',
    text = ~HoverText,
    hoverinfo = 'text'
  )  %>% layout(
    title = 'Root diameters and volume distribution',
    xaxis = list(title=paste('Root diameter [',u_x,']',sep='')), 
    yaxis = list(title=paste('Root volume fraction in diameter class [',u_y,']',sep=''), rangemode='tozero')
  )
  #return
  return(p)
}


#####################################
### PLOT INPUT - ROOT ORIENTATION ###
#####################################

#2D polar plot or root orientations
plot_rootorientations_2D <- function(do, dio, d3grid, ndim, nsignif=4){
  # FUNCTION to generate a plotly plot with all discrete root orientations
  # INPUT
  # - <do>:     dataframe containing all discrete root orientations classes. 
  #             unit system used is in Requires fields
  #             - alpha0: azimuth angle [rad]
  #             - beta0:  elevation angle [rad]
  #             - weight: weight of each orientation class [-]
  # - <dio>:   dataframe containing input orientation properties, in SI units. Required fields
  #             - ndim: number of dimensions
  #             - beta0_max: max root angle [rad]
  #             - alpha_offset: azimuth offset [rad]
  #             - beta_offset: elevation offset [rad]
  # - <d3grid>: contains <x>,<y>,<z> coordinates of gridlines,when root orientations in 3D,
  # - <ndim>:   number of root orientation dimension ('1', '2' or '3', string value)
  # OPTIONAL INPUT
  # - <nsignif>:       Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #create boundary of orientations
  if (dio$ndim==1) {
    #1D --> single point
    db <- data.frame(alpha0=dio$alphaoffset, beta0=dio$betaoffset)
  } else if (dio$ndim==2) {
    #2D --> line
    temp <- f_rootangles(c(-pi,0), rep(dio$beta0max,2), dio$alphaoffset, dio$betaoffset)
    db <- data.frame(alpha0=temp$alpha0, beta0=temp$beta0)
  } else if (dio$ndim==3) {
    #3D --> plane
    n  <- 360
    db <- data.frame(alpha0=seq(-pi,pi,l=n), beta0=dio$beta0max)
    temp <- f_rootangles(db$alpha0, db$beta0, dio$alphaoffset,dio$betaoffset)
    db <- data.frame(alpha0=temp$alpha0, beta0=temp$beta0)
  }
  #create hover text
  do$HoverText <- with(do, paste('Azimuth angle: ', signif(alpha0*180/pi, nsignif), ' [deg]', 
                                 '<br>', 'Elevation angle: ', signif(beta0*180/pi, nsignif), ' [deg]',
                                 '<br>', 'Weight: ', signif(weight*100, nsignif), ' [%]', 
                                 sep=''))
  #create plotly object
  p <- plot_ly(
    data = db,
    type = 'scatterpolar',
    mode = 'lines',
    fill = 'toself',
    r = ~beta0*180/pi,
    theta = ~alpha0,
    thetaunit = 'radians',
    name = 'Orientation range'
  ) %>% add_trace(
    data = do,
    type = 'scatterpolar',
    r = ~beta0*180/pi,
    theta = ~alpha0,
    mode = 'markers',
    fill = 'none',
    thetaunit = 'radians',
    name = 'Discrete orientations used',
    text = ~HoverText,
    hoverinfo = 'text'
  ) %>% layout(
    title = 'Root orientations (polar)',
    polar=list(
      radialaxis =  list(title='Elevation angle [deg]'), #, range=c(0,90)
      angularaxis = list(title='Azimuth angle [deg]')
    )
  )
  #add 3d grid
  if (ndim=='3') { #3D
    p <- p %>% add_trace( #sphere - blue
      data = d3grid,
      r = ~beta0*180/pi, 
      theta = ~alpha0,
      type = 'scatterpolar',
      mode = 'lines',
      line = list(width = 1, color='#444444', opacity = 0.6),
      fill = NA,
      hoverinfo='skip',
      name='Orientation grid'
    )
  }
  #return
  return(p)
}

#3D orientation plot
plot_rootorientations_3D <- function(d3dl, d3dus, d3dr, d3grid, ndim, nsignif=4){
  # FUNCTION to generate a plotly 3D plot with all discrete root orientations
  # INPUT
  # - <d3dl>: dataframe with positions of root orientations in unit sphere. Requires fields
  #           - x: x-position
  #           - y: y-position
  #           - z: z-position
  #           position items in array are separatered by NA's to indicate seperate lines
  # - <d3dus>: list containing data for unit sphere mesh plotting. Contains dataframes
  #           - <coordinates>: dataframe with <x>, <y> and <z> coordinates
  #           - <vertices>: per face, contains <i>, <j> and <k> index of node indices characterising each trianglar face (python indexing)
  # - <d3dr>: information about root range to plot. Data varies depending of number of dimension of root orientation
  #           - if 1D, contains nothing, 
  #           - if 2D, contains line with <x>, <y>, <z> positions
  #           - if 3D, contains partial unit sphere mesh, see <d3dus>
  # - <d3grid>: data for grid lines for 3D root orientations
  #           - if 1D or 2D, contains nothing, 
  #           - if 3D, contains array for plotly line, with fields <x>,<y>,<z>
  # - <ndim>: number of dimensions of root orientations
  # OPTIONAL INPUT
  # - <nsignif>:       Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #hoverinfo
  nsignif <- 4
  d3dl$HoverText <- with(d3dl, paste('Azimuth: ', signif(alpha0*180/pi,nsignif), ' [deg]', 
                                     '<br>', 'Elevation: ', signif(beta0*180/pi,nsignif), ' [deg]', 
                                     '<br>', 'Weight: ', signif(weight*100,nsignif), ' [%]', 
                                     sep=''))
  #positions of shear displacement arrows
  arrowx  <- 0.4
  arrowz  <- 0.2
  arrowdz <- 0.1
  
  #create plotly object
  p <- plot_ly() %>% add_trace( #root orientations
    data = d3dl,
    x = ~x,
    y = ~y,
    z = ~z,
    type ='scatter3d',
    mode='lines+markers',
    text = ~HoverText,
    hoverinfo = 'text',
    name='Root orientations',
    marker = list(color='#ff7f0e'),
    line   = list(width = 3, color='#ff7f0e')
  ) %>% add_trace( #shear plane
    x = c(-1,1) * 1.2,
    y = c(-1,1) * 1.2,
    z = matrix(c(0,0,0,0),nrow=2),
    type = 'surface',
    opacity = 0.8,
    hoverinfo='skip',
    showscale = F,
    name='Shear plane'
  ) %>% add_trace( #shear plane arrows
    x = c(-arrowx, arrowx, arrowx-arrowdz, NA, arrowx, -arrowx, -arrowx+arrowdz),
    y = c(0,0,0, NA, 0,0,0),
    z = c(arrowz, arrowz, arrowz+arrowdz, NA, -arrowz, -arrowz, -arrowz-arrowdz),
    type='scatter3d',
    mode='lines',
    line = list(width = 6),
    name='Shear displacement direction'
  ) %>% add_trace( #unit sphere - gray
      data = d3dus,  
      x = ~coordinates$x, 
      y = ~coordinates$y, 
      z = ~coordinates$z, 
      i = ~vertices$i,
      j = ~vertices$j,
      k = ~vertices$k,
      opacity = 0.3,
      type = 'mesh3d',
      facecolor = rep('#7f7f7f', dim(d3dus$vertices)[1]),
      flatshading = T,
      hoverinfo='skip',
      name='Unit sphere'
  ) %>% layout(
    title = 'Root orientations (3D)',
    scene = list(
      xaxis  = list(title=paste('x', sep='')), 
      yaxis  = list(title=paste('y', sep=''), scaleanchor = "x"),
      zaxis  = list(title=paste('z', sep=''), scaleanchor = "x"),
      camera = list(eye=list(x=0, y=-2, z=0.5),
                    center=list(x=0, y=0, z=0))
    )
  )
  #add range
  if (ndim=='2') { #2D
    p <- p %>% add_trace(
      data = d3dr,
      x = ~x,
      y = ~y,
      z = ~z,
      type = 'scatter3d',
      mode = 'lines',
      line = list(width = 6, color='#1f77b4'),
      hoverinfo='skip',
      name='Orientation range',
      showlegend=F
    ) %>% add_trace(
      data = d3dr,
      x = ~-x,
      y = ~-y,
      z = ~-z,
      type = 'scatter3d',
      mode = 'lines',
      line = list(width = 6, color='#1f77b4'),
      hoverinfo='skip',
      name='Orientation range',
      showlegend=F
    )
  } else if (ndim=='3') { #3D
    p <- p %>% add_trace( #sphere - blue
      x = d3dr$coordinates$x, 
      y = d3dr$coordinates$y, 
      z = d3dr$coordinates$z, 
      i = d3dr$vertices$i,
      j = d3dr$vertices$j,
      k = d3dr$vertices$k,
      opacity = 0.6,
      type = 'mesh3d',
      facecolor = rep('#1f77b4', dim(d3dr$vertices)[1]),
      flatshading = T,
      hoverinfo='skip',
      name='Orientation range'
    ) %>% add_trace( #sphere - blue
      data = d3dr,
      x = -d3dr$coordinates$x, 
      y = -d3dr$coordinates$y, 
      z = -d3dr$coordinates$z, 
      i = d3dr$vertices$i,
      j = d3dr$vertices$j,
      k = d3dr$vertices$k,
      opacity = 0.6,
      type = 'mesh3d',
      facecolor = rep('#1f77b4', dim(d3dr$vertices)[1]),
      flatshading = T,
      hoverinfo='skip',
      name='Orientation range'
    ) %>% add_trace( #grid lines - top
      data = d3grid,
      x = ~x, 
      y = ~y, 
      z = ~z, 
      type = 'scatter3d',
      mode = 'lines',
      line = list(width = 2, color='#444444', opacity = 0.6),
      hoverinfo='skip',
      name='Orientation grid'
    ) %>% add_trace( #grid lines - top
      data = d3grid,
      x = ~-x, 
      y = ~-y, 
      z = ~-z, 
      type = 'scatter3d',
      mode = 'lines',
      line = list(width = 2, color='#444444', opacity = 0.6),
      hoverinfo='skip',
      name='Orientation grid',
      showlegend=F
    )
  }
  #return
  return(p)
}


##########################################
### PLOT INPUT - ROOT TENSILE STRENGTH ###
##########################################

#root tensile strength - root diameter
plot_roottensilestrength <- function(dop, diu, nsignif=4){
  # FUNCTION to generate a plotly plot with root tensile strength as function of root diameter
  # INPUT
  # - <dop>: dataframe containing all root diameter classes and properties. 
  #          values are expressed in the original unit system. Required fields:
  #         - dr:   average root diameter in class 
  #         - tru:  root tensile strength of average root in class 
  # - <diu>: dataframe containing unit system
  # OPTIONAL INPUT
  # - <nsignif>:    Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #units used
  u_x   <- diu['length','unit_user']
  u_y   <- diu['stress','unit_user']
  #factors used
  fac_x <- diu['length','factor']
  fac_y <- diu['stress','factor']
  
  #create hover text
  dop$HoverText <- with(dop, paste('Average root diameter in diameter class: ', signif(dr/fac_x, nsignif), ' [',u_x,']', 
                                   '<br>', 'Root tensile strength: ', signif(tru/fac_y, nsignif), ' [',u_y,']', 
                                   sep=''))
  #create plotly object
  p <- plot_ly(
    data=dop, 
    x=~dr/fac_x, 
    y=~tru/fac_y, 
    type='scatter', 
    mode='lines+markers',
    text = ~HoverText,
    hoverinfo = 'text'
  ) %>% layout(
    title = 'Root tensile strength',
    xaxis = list(title=paste('Root diameter [',u_x,']', sep='')), 
    yaxis = list(title=paste('Root tensile strength [',u_y,']', sep=''), rangemode='tozero')
  )
  #return plot
  return(p)
}


########################################
### PLOT INPUT - ROOT TENSILE STRAIN ###
########################################

#root tensile strain to failure - root diameter
plot_roottensilestrain <- function(dop, diu, nsignif=4){
  # FUNCTION to generate a plotly plot with root tensile strain to failue as function of root diameter
  # INPUT
  # - <dop>: dataframe containing all root diameter classes and properties. 
  #          values are expressed in the original unit system. Required fields:
  #          - dr:   average root diameter in class [L]
  #          - eru:  root tensile strain to failure of average root in class [F/L^2]
  # - <diu>: dataframe containing unit system
  # OPTIONAL INPUT
  # - <nsignif>:           Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #units used
  u_x   <- diu['length','unit_user']
  u_y   <- paste(diu['length','unit_user'], '/', diu['length','unit_user'], sep='')
  #factors used
  fac_x <- diu['length','factor']
  fac_y <- 1
  
  #create hover text
  dop$HoverText <- with(dop, paste('Average root diameter in diameter class: ', signif(dr/fac_x, nsignif), ' [',u_x,']', 
                                   '<br>', 'Root tensile strain to failure: ', signif(eru/fac_y, nsignif), ' [',u_y,']', 
                                   sep=''))
  #create plotly object
  p <- plot_ly(
    data=dop, 
    x=~dr/fac_x, 
    y=~eru/fac_y, 
    type='scatter', 
    mode='lines+markers',
    text = ~HoverText,
    hoverinfo = 'text'
  ) %>% layout(
    title = 'Root tensile strain to failure',
    xaxis = list(title=paste('Root diameter [',u_x,']', sep='')), 
    yaxis = list(title=paste('Root tensile strain to failure [',u_y,']', sep=''), rangemode='tozero')
  )
  #return plot
  return(p)
}


################################
### PLOT INPUT - ROOT LENGTH ###
################################

#root length - root diameter
plot_rootlength <- function(dop, diu, nsignif=4){
  # FUNCTION to generate a plotly plot with root length as function of root diameter
  # INPUT
  # - <dop>: dataframe containing all root diameter classes and properties. 
  #          values are expressed in the original unit system. Required fields:
  #          - dr:   average root diameter in class [L]
  #          - Lr:   root length of average root in class [F/L^2]
  # - <diu>: dataframe containing unit system
  # OPTIONAL INPUT
  # - <nsignif>:           Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #units used
  u_x   <- diu['length','unit_user']
  u_y   <- diu['length','unit_user']
  #factors used
  fac_x <- diu['length','factor']
  fac_y <- diu['length','factor']
  
  #create hover text
  dop$HoverText <- with(dop, paste('Average root diameter in diameter class: ', signif(dr/fac_x, nsignif), ' [',u_x,']', 
                                   '<br>', 'Root length: ', signif(Lr/fac_y, nsignif), ' [',u_y,']', 
                                   sep=''))
  #create plotly object
  p <- plot_ly(
    data=dop, 
    x=~dr/fac_x, 
    y=~Lr/fac_y, 
    type='scatter', 
    mode='lines+markers',
    text = ~HoverText,
    hoverinfo = 'text'
  ) %>% layout(
    title = 'Root length',
    xaxis = list(title=paste('Root diameter [',u_x,']', sep='')), 
    yaxis = list(title=paste('Root length [',u_y,']', sep=''), rangemode='tozero')
  )
  #return plot
  return(p)
}


#############################################
### PLOT INPUT - ROOT STRESS_STRAIN CURVE ###
#############################################

#normalised root stress-strain curve + fraction of roots intact as function of current tensile strain
plot_stressstraincurve <- function(dp, diu, nsignif=4){
  # FUNCTION to generate a plotly plot with root stress-strain behaviour and root fraction unbroken
  # INPUT
  # - <dp>: dataframe containing all root diameter classes and properties. 
  #         only the first row is used to plot an example plot
  #         dataframe required fields:
  #         - ery:    root yield strain [L/L]
  #         - eru:    root strain to failure [L/L]
  #         - try:    root yield strength [F/L^2]
  #         - tru:    root tensile strength [F/L^2]
  #         - kappat: Weibull shape parameter for distribution of root tensile strenght tru [-]
  # - <diu>: dataframe containing unit system
  # OPTIONAL INPUT
  # - <nsignif>:           Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #units used
  u_x    <- paste('(', diu['length','unit_user'], '/', diu['length','unit_user'], ')/(', diu['length','unit_user'], '/', diu['length','unit_user'], ')', sep='')
  u_y    <- paste(diu['stress','unit_user'], '/', diu['stress','unit_user'], sep='')
  u_y2   <- '-'
  #factors used
  fac_x  <- 1
  fac_y  <- 1
  fac_y2 <- 1
  
  #R machine precision - smallest step in R
  dd <- .Machine$double.eps
  #max epsilon - normalised over tensile strain to failure
  epshatmax <- 2
  #create stress-strain behaviour of single root, normalised by root tensile strength and strain to failure
  d1 <- data.frame(epshat = c(0, dp$ery[1]/dp$eru[1], 1, 1+dd, epshatmax),
                   trhat  = c(0, dp$try[1]/dp$tru[1], 1, 0, 0))
  #create hovertext
  d1$HoverText <- with(d1, paste('Strain (normalised): ', signif(epshat/fac_x, nsignif), ' [', u_x, ']', 
                       '<br>', 'Stress (normalised): ', signif(trhat/fac_y, nsignif), ' [', u_y, ']', 
                       sep=''))
  #create weibull data
  n    <- 501
  kap  <- dp$kappat[1]
  lam  <- 1 / gamma(1 + 1/kap)
  eps1 <- seq(0, dp$ery[1], l=n)
  eps2 <- seq(dp$ery[1], (epshatmax*dp$eru[1]), l=n)
  tr1  <- eps1 * dp$Ere[1]
  tr2  <- tail(tr1,1) + (eps2-dp$ery[1])*dp$Erep[1]
  d2   <- data.frame(epshat=c(eps1,eps2)/dp$eru[1], 
                     trihat=c(tr1,tr2)/dp$tru[1])
  d2$fbreak <- with(d2, exp(-((trihat/lam)^kap)))
  d2$trhat  <- with(d2, trihat*fbreak)
  #create hovertext
  d2$HoverText <- with(d2, paste('Strain (normalised): ', signif(epshat/fac_x, nsignif), ' [', u_x, ']', 
                                 '<br>', 'Root fraction intact: ', signif(fbreak/fac_y2, nsignif), ' [', u_y2, ']', 
                                 sep=''))
  #create plotly object
  p <- plot_ly(
    data=d1, 
    x=~epshat/fac_x, 
    y=~trhat/fac_y, 
    type='scatter', 
    mode='lines+markers',
    name='Root stress-strain behaviour',
    text = ~HoverText,
    hoverinfo = 'text'
  ) %>% add_trace(
    data=d2,
    x=~epshat/fac_x,
    y=~fbreak/fac_y2,  
    type='scatter', 
    mode='lines',
    name='Root fraction intact (Weibull)',
    yaxis = 'y2',
    text = ~HoverText,
    hoverinfo = 'text'
  ) %>% layout(
    title =  'Root tensile stress-strain behaviour',
    xaxis =  list(title=paste('Normalised root strain to failure [', u_x, ']', sep=''), range=c(0,epshatmax)/fac_x), 
    yaxis =  list(title=paste('Normalised root tensile strength [', u_y, ']', sep=''), range=c(0,1)/fac_y),
    yaxis2 = list(overlaying = "y", side = "right", title = paste('Root fraction intact [',u_y2,']',sep=''), range=c(0,1)/fac_y2)
  )
  #return plot
  return(p)
}


#########################################
### PLOT INPUT - SOIL FIELD CRITERION ###
#########################################

#Soil Mohr-Coulomb yield criterion
plot_soilyieldcriterion <- function(dos, diu, nsignif=4) {
  # FUNCTION to generate a plotly plot with soil behaviour (Mohr-Coulomb)
  # INPUT
  # - <dos>: dataframe containing all soil properties.
  #          Parameters are expressed in user-defined unit system. Required fields:
  #          - sign: soil effective normal stress in the shear zone [F/L^2]
  #          - c:    soil cohesion [F/L^2]
  #          - phi:  soil angle of internal friction [deg]
  # - <diu>: dataframe containing unit system
  # OPTIONAL INPUT
  # - <nsignif>:           Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #units used
  u_x   <- diu['stress','unit_user']
  u_y   <- diu['stress','unit_user']
  #factors used
  fac_x <- diu['stress','factor']
  fac_y <- diu['stress','factor']
  
  #soil angle of internal friction - force back into radians for calculations
  phisoil <- dos$phi * 180/pi
  
  #create dataframe with yield criterion
  dd <- data.frame(sign = seq(0, 1.25*dos$sign, l=501))
  dd$tau <- with(dd, dos$c + dd$sign*tan(phisoil))
  #Create hovertext
  dd$HoverText <- with(dd, paste('Normal effective stress: ', signif(sign/fac_x, nsignif), ' [',u_x,']', 
                                 '<br>', 'Soil shear strength: ', signif(tau/fac_y, nsignif), ' [',u_y,']', 
                                 sep=''))
  #create annotations layer for peak
  a <- list(
    x = with(dos, sign)/fac_x,
    y = with(dos, c+sign*tan(phisoil))/fac_y,
    text = paste('Normal effective stress in shear zone: ', signif(dos$sign/fac_x,nsignif), ' [',u_x,']', 
                 '<br>Soil shear strength in shear zone: ',  signif((dos$c + dos$sign*tan(phisoil))/fac_y,nsignif), ' [',u_y,']',
                 sep=''),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = 20,
    ay = -40
  )
  #create plotly object
  p <- plot_ly(
    data = dd, 
    x = ~sign/fac_x, 
    y = ~tau/fac_y, 
    type = 'scatter', 
    mode = 'lines',
    text = ~HoverText,
    hoverinfo = 'text'
  ) %>% layout(
    title = 'Soil shear strength',
    xaxis = list(title=paste('Normal effective stress [',u_x,']', sep='')), 
    yaxis = list(title=paste('Soil shear strength [',u_y,']', sep=''), rangemode='tozero'),
    annotations = a
  )
  #return plot object
  return(p)
}


########################################
### PLOT OUTPUT - ROOT REINFORCEMENT ###
########################################

#Dundee Root Model: reinforcement as function of shear displacement
plot_reinforcement <- function(dsum, diu, nsignif=4) {
  # FUNCTION to generate a plotly plot with total root-reinforcement as function of shear displacement
  # INPUT
  # - <dsum>: dataframe containing summary of analysis output. Required fields:
  #           - u:         shear displacement [L]
  #           - cr:        root-reinforcement [F/L^2]
  #           - WWMfactor: root-reinforcement normalised over the product of root tensile strength and root volume fraction [-]
  # OPTIONAL INPUT
  # - <nsignif>:           Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #units used
  u_x   <- diu['length','unit_user']
  u_y   <- diu['stress','unit_user']
  #factors used
  fac_x <- diu['length','factor']
  fac_y <- diu['stress','factor']
  
  #Create hover text - diameter range + total root fraction
  dsum$HoverText <- with(dsum, paste('Shear displacement: ', signif(u/fac_x, nsignif), ' [',u_x,']', 
                                 '<br>', 'Root-reinforcement: ', signif(cr/fac_y, nsignif), ' [',u_y,']', 
                                 '<br>', 'Equivalent WWM coefficient: ', signif(WWMfactor,nsignif), ' [-]', 
                                 sep=''))
  #create annotations layer for peak
  a <- list(
    x = dsum$u[which.max(dsum$cr)]/fac_x,
    y = max(dsum$cr)/fac_y,
    text = paste('Peak root-reinforcement: ', signif(max(dsum$cr)/fac_y,nsignif), ' [',u_y,']', 
                 '<br>Shear displacement: ',  signif(dsum$u[which.max(dsum$cr)]/fac_x,nsignif), ' [',u_x,']',
                 '<br>Equivalent WWM coefficient: ',     signif(dsum$WWMfactor[which.max(dsum$cr)],nsignif), ' [-]', 
                 sep=''),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = 20,
    ay = -40
  )
  #create plotly object
  p <- plot_ly(
    data = dsum, 
    x = ~u/fac_x, 
    y = ~cr/fac_y, 
    type = 'scatter', 
    mode = 'lines',
    text = ~HoverText,
    hoverinfo = 'text'
    ) %>% layout(
      title = 'Root-reinforcement',
      xaxis = list(title=paste('Shear displacement [',u_x,']', sep='')), 
      yaxis = list(title=paste('Root-reinforcement [',u_y,']', sep=''), rangemode='tozero'),
      annotations = a
    )
  #return plot object
  return(p)
}
  

##########################################
### PLOT OUTPUT - SHEAR ZONE THICKNESS ###
##########################################

#Dundee Root Model: shear zone thickness as function of shear displacement
plot_shearzonethickness <- function(dsum, diu, nsignif=4) {
  # FUNCTION to generate a plotly plot with total root-reinforcement as function of shear displacement
  # INPUT
  # - <dsum>: dataframe containing summary of analysis output. Required fields:
  #           - u:        shear displacement [L]
  #           - h:        shear zone thickness [L]
  # OPTIONAL INPUT
  # - <nsignif>:           Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #units used
  u_x   <- diu['length','unit_user']
  u_y   <- diu['length','unit_user']
  #factors used
  fac_x <- diu['length','factor']
  fac_y <- diu['length','factor']
  
  #Create hover text - diameter range + total root fraction
  dsum$HoverText <- with(dsum, paste('Shear displacement: ', signif(u/fac_x, nsignif), ' [',u_x,']', 
                                     '<br>', 'Shear zone thickness: ', signif(h/fac_y, nsignif), ' [',u_y,']', 
                                     sep=''))
  #create plotly object
  p <- plot_ly(
    data = dsum, 
    x = ~u/fac_x, 
    y = ~h/fac_y, 
    type = 'scatter', 
    mode = 'lines',
    text = ~HoverText,
    hoverinfo = 'text'
  ) %>% layout(
    title = 'Shear zone thickness',
    xaxis = list(title=paste('Shear displacement [',u_x,']', sep='')),
    yaxis = list(title=paste('Shear zone thickness [',u_y,']', sep=''), rangemode='tozero')
  )
  #return plot object
  return(p)
}


#################################################
### PLOT OUTPUT - FRACTIONS OF ROOT BEHAVIOUR ###
#################################################

#Dundee Root Model: type of root behaviour as function of shear displacement
plot_behaviourfractions <- function(dfrc, diu, nsignif=4) {
  # FUNCTION to generate a plotly plot with fractions of root behaviour (elastic, slipping etc) as function of shear displacement
  # INPUT
  # - <dfrc>: dataframe containing summary of analysis output. Required fields:
  #           - u:                              shear displacement [L]
  #           - Fraction_NotInTension:          Fraction of roots not in tension or completely within shear zone [L^3/L^3]
  #           - Fraction_AnchoredElastic:       Fraction of roots that behaves anchored, elastic [L^3/L^3]
  #           - Fraction_AnchoredElastoplastic: Fraction of roots that behaves anchored, elasto-plastic [L^3/L^3]
  #           - Fraction_SlippingElastic:       Fraction of roots that behaves slipping, elastic [L^3/L^3]
  #           - Fraction_SlippingElastoplastic: Fraction of roots that behaves slipping, elasto-plastic [L^3/L^3]
  #           - Fraction_Broken:                Fraction of roots that is broken [L^3/L^3]
  # OPTIONAL INPUT
  # - <nsignif>:           Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #units used
  u_x   <- diu['length','unit_user']
  u_y   <- paste(diu['length','unit_user'], '^3/', diu['length','unit_user'], '^3', sep='')
  #factors used
  fac_x <- diu['length','factor']
  fac_y <- 1
  
  #create some colors with RColorBrewer
  colo <- brewer.pal(6, "Set3")
  #create plotly object
  p <- plot_ly(data=dfrc, x=~u/fac_x, y=~Fraction_NotInTension/fac_y, name = 'Not in tension, or fully within shear zone', type='scatter', mode='none', stackgroup='one', fillcolor=colo[1]) %>% 
    add_trace(y=~Fraction_AnchoredElastic/fac_y,  name = 'Anchored, elastic', fillcolor=colo[2]) %>%         
    add_trace(y=~Fraction_AnchoredElastoplastic/fac_y, name = 'Anchored, elasto-plastic', fillcolor=colo[3]) %>%
    add_trace(y=~Fraction_SlippingElastic/fac_y,  name = 'Sliiping, elastic', fillcolor=colo[4]) %>%         
    add_trace(y=~Fraction_SlippingElastoplastic/fac_y, name = 'Slipping, elasto-plastic', fillcolor=colo[5]) %>%  
    add_trace(y=~Fraction_Broken/fac_y, name = 'Broken', fillcolor=colo[6]) %>%  
    layout(
    title = 'Root behaviour type',
    xaxis = list(title=paste('Shear displacement [',u_x,']', sep='')), 
    yaxis = list(title=paste('Fraction of total root volume [', u_y, ']', sep='')),
    hovermode = 'compare'
  )
  #return plot object
  return(p)
}


#################################
### FIBRE BUNDLE MODEL - PLOT ###
#################################

#Fibre bundle model, reinforcement as function of diameter of currently breaking root + WWM
plot_fibrebundlemodel <- function(dfbm, dwwm, diu, nsignif=4) {
  # FUNCTION to generate a plotly plot with Fibre Bundle Model and Wu/Waldron model predictions
  # INPUT
  # - <dfbm>: dataframe containing summary of FBM analysis
  #           - dr:           Diameter of currently breaking roots [L]
  #           - cr:           Root-reinforcement at moment of roots with dimaeter <dr> breaking [F/L^2]
  #           - FailureOrder: Order in which roots break (lower numbers break earlier) [-]
  # - <dwwm>: WWM root cohesion scalar value
  # - <diu>:  dataframe with unit settings
  # OPTIONAL INPUT
  # - <nsignif>:           Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #units used
  u_x   <- diu['length','unit_user']
  u_y   <- diu['stress','unit_user']
  #factors used
  fac_x <- diu['length','factor']
  fac_y <- diu['stress','factor']
  
  #Create hover text - diameter range + total root fraction
  dfbm$HoverText <- with(dfbm, paste('Diameter of currently breaking root: ', signif(dr/fac_x, nsignif), ' [', u_x, ']', 
                                     '<br>', 'Root-reinforcement (FBM): ', signif(cr/fac_y, nsignif), ' [', u_y, ']', 
                                     sep=''))
  
  #Create wwm dataframe
  ddwwm <- data.frame(dr=c(min(dfbm$dr), max(dfbm$dr)), cr=c(dwwm,dwwm))
  #Create hover text - diameter range + total root fraction
  ddwwm$HoverText <- with(ddwwm, paste('Root-reinforcement (WWM): ', signif(cr/fac_y, nsignif), ' [', u_y, ']', 
                                     sep=''))
  
  #sort dfbm by failure order
  dfbm <- dfbm[order(dfbm$FailureOrder),]
  #arrows: end point
  dfbm$dr_end <- c(dfbm$dr[-1], tail(dfbm$dr,1))
  dfbm$cr_end <- c(dfbm$cr[-1], tail(dfbm$cr,1))
  
  #create plotly object
  p <- plot_ly(
  ) %>% add_annotations( 
    data = dfbm,
    x = ~dr_end/fac_x,
    y = ~cr_end/fac_y,
    xref = "x", yref = "y",
    axref = "x", ayref = "y",
    text = "",
    showarrow = T,
    ax = ~dr/fac_x,
    ay = ~cr/fac_y,
    arrowcolor = '#1f77b4'
  ) %>% add_trace(
    data = dfbm, 
    x = ~dr/fac_x, 
    y = ~cr/fac_y, 
    type = 'scatter', 
    mode = 'markers',
    text = ~HoverText,
    hoverinfo = 'text',
    name = 'FBM'
  ) %>% add_trace(
    data = ddwwm,
    x = ~dr/fac_x, 
    y = ~cr/fac_y, 
    type = 'scatter', 
    mode = 'lines',
    text = ~HoverText,
    hoverinfo = 'text',
    name = 'WWM'
  ) %>% layout(
    title = 'Fibre bundle model + Wu/Waldron model',
    xaxis = list(title=paste('Diameter of currently breaking root [', u_x,']', sep='')), 
    yaxis = list(title=paste('Root-reinforcement [', u_y,']', sep=''), rangemode='tozero')
  ) %>% add_annotations(
    x = dfbm$dr[which.max(dfbm$cr)]/fac_x,
    y = max(dfbm$cr)/fac_y,
    text = paste('FBM: ',  signif(max(dfbm$cr)/fac_y,nsignif), ' [', u_y, ']',
                 sep=''),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = -20,
    ay = 40  
  ) %>% add_annotations(
    x = mean(ddwwm$dr)/fac_x,
    y = dwwm/fac_y,
    text = paste('WWM: ',  signif(dwwm/fac_y,nsignif), ' [', u_y, ']',
                 sep=''),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = 20,
    ay = -40
  )
  #return plot object
  return(p)
}


################################
### ROOT BUNDLE MODEL - PLOT ###
################################

#reinforcement according to RBM(w), as function of tensile strain
plot_rootbundlemodel <- function(drbm, drbmw, diu, nsignif=4) {
  # FUNCTION to generate a plotly plot with Root Bundle Model predictions
  # INPUT
  # - <drbm>:  dataframe containing summary of RBM analysis (sudden breakage)
  #            - eps:          Current tensile strain [L/L]
  #            - cr:           Current root-reinforcement  [F/L^2]
  # - <drbmw>: dataframe containing summary of RBMw analysis (weibull breakage)
  #            - eps:          Current tensile strain [L/L]
  #            - cr:           Current root-reinforcement  [F/L^2]
  # - <diu>:   dataframe with unit settings
  # OPTIONAL INPUT
  # - <nsignif>:           Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object
  
  #units used
  u_x   <- paste(diu['length','unit_user'], '/', diu['length','unit_user'], sep='')
  u_y   <- diu['stress','unit_user']
  #factors used
  fac_x <- 1
  fac_y <- diu['stress','factor']
  
  #create hover text - rmb
  drbm$HoverText <- with(drbm, paste('Tensile strain: ', signif(eps/fac_x, nsignif), ' [', u_x, ']', 
                                     '<br>', 'Root-reinforcement (RBM): ', signif(cr/fac_y, nsignif), ' [', u_y, ']', 
                                     sep=''))
  #create hover text - rbmw
  drbmw$HoverText <- with(drbmw, paste('Tensile strain: ', signif(eps/fac_x, nsignif), ' [', u_x, ']', 
                                     '<br>', 'Root-reinforcement (RBMw): ', signif(cr/fac_y, nsignif), ' [', u_y, ']', 
                                     sep=''))
  #create plotly object
  p <- plot_ly(
    data = drbm, 
    x = ~eps/fac_x, 
    y = ~cr/fac_y, 
    type = 'scatter', 
    mode = 'lines',
    text = ~HoverText,
    hoverinfo = 'text',
    name = 'RBM'
  ) %>% add_trace(
    data = drbmw,
    x = ~eps/fac_x, 
    y = ~cr/fac_y, 
    type = 'scatter', 
    mode = 'lines',
    text = ~HoverText,
    hoverinfo = 'text',
    name = 'RBMw'
  ) %>% layout(
    title = 'Root Bundle Models',
    xaxis = list(title=paste('Tensile strain [', u_x, ']', sep='')), 
    yaxis = list(title=paste('Root-reinforcement [', u_y, ']', sep=''), rangemode='tozero')
  ) %>% add_annotations(
    x = drbm$eps[which.max(drbm$cr)]/fac_x,
    y = max(drbm$cr)/fac_y,
    text = paste('RBM: ',  signif(max(drbm$cr)/fac_y,nsignif), ' [', u_y, ']',
                 sep=''),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = 20,
    ay = -40
  ) %>% add_annotations(
    x = drbmw$eps[which.max(drbmw$cr)]/fac_x,
    y = max(drbmw$cr)/fac_y,
    text = paste('RBMw: ',  signif(max(drbmw$cr)/fac_y,nsignif), ' [', u_y, ']',
                 sep=''),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = -20,
    ay = 40
  )
  #return plot object
  return(p)
}

##############################
### ALL ROOT MODELS - PLOT ###
##############################

#function to create bar plot with results from all root models
f_predictionsallbarplot <- function(dwwm, dfbm, drbm, drbmw, ddrm, k, diu, nsignif=4){
  # FUNCTION to plot all peak root-reinforcement results in a single bar plot
  # INPUT
  # - <dwwm>:  WWM root cohesion scalar value
  # - <dfbm>:  dataframe containing summary of FBM analysis
  #            - cr:           Root-reinforcement at moment of roots with dimaeter <dr> breaking [F/L^2]
  # - <drbm>:  dataframe containing summary of RBM analysis (sudden breakage)
  #            - cr:           Current root-reinforcement  [F/L^2]
  # - <drbmw>: dataframe containing summary of RBMw analysis (weibull breakage)
  #            - cr:           Current root-reinforcement  [F/L^2]
  # - <ddrm>: dataframe containing summary of Dundee root model
  #            - cr:           Current root-reinforcement  [F/L^2]
  # - <k>:    WWM model k factor
  # - <diu>:  dataframe with unit settings
  # OPTIONAL INPUT
  # - <nsignif>:           Number of significant digits in plotted labels
  # OUTPUT
  # - <p>: plotly plot object

  #units used
  u_y   <- diu['stress','unit_user']
  #factors used
  fac_y <- diu['stress','factor']

  #Create single dataframe with all predictions
  dall <- data.frame(Model=c('WWM','FBM','RBM','RBMw','DRAM'), 
                     cru  =c(dwwm, max(dfbm$cr), max(drbm$cr), max(drbmw$cr), max(ddrm$cr)))
  #WWM factor
  dall$WWMfactor <- dall$cru / dall$cru[dall$Model=='WWM'] * k
  #create hover text
  dall$HoverText <- with(dall, paste('Model: ', Model,  
                                     '<br>', 'Peak root-reinforcement: ', signif(cru/fac_y, nsignif), ' [', u_y, ']',
                                     '<br>', 'Equivalent WWM coefficient: ', signif(WWMfactor, nsignif), ' [-]', 
                                      sep=''))
  #create plotly object
  p <- plot_ly(
    data = dall,
    x = ~Model,
    y = ~cru/fac_y,
    type = "bar",
    text = ~HoverText,
    hoverinfo = 'text'
  ) %>% layout(
    title = 'Peak root-reinforcement (all models)',
    xaxis = list(title=paste('Model', sep='')), 
    yaxis = list(title=paste('Peak root-reinforcement [', u_y, ']', sep=''), rangemode='tozero')
  )
    
  #return
  return(p)
}


#######################################
### COORDINATE FOR (PARTIAL SPHERE) ###
#######################################

#function to generate a triangular mesh for a (partial) unit sphere
f_coordssphere <- function(alpha0=c(-pi,pi), beta0=c(0,pi), nalpha=24, nbeta=12, alphaoffset=0, betaoffset=0){
  # FUNCTION to create coordinates and vertices for plotting a (partial) sphere
  # OPTIONAL INPUT
  # - <alpha0>:      azimuth range of unit sphere [rad]
  # - <beta0>:       elevation range of unit sphere [rad]
  # - <nalpha>:      number of points for discretisation of azimuth range
  # - <nbeta>:       number of points for discretisation of elevation range
  # - <alphaoffset>: coordinate system azimuth offset
  # - <betaoffset>:  coordinate system elevation offset
  # OUTPUT
  # - list with two dataframes:
  #   - <coordinates>: <x>,<y>,<z> coordinates on unit sphere, and azimuth/elevation angles <alpha0> and <beta0>
  #   - <vertices>:    <i>,<j>,<k> list of indices of triangle points (python indexing, e.g. '0' is the first index)
  
  ## CREATE LOCATIONS OF NODES
  #create sphere locations
  ds   <- expand.grid(alpha0=seq(alpha0[1],alpha0[2],l=nalpha), beta0=seq(beta0[1],beta0[2],l=nbeta), KEEP.OUT.ATTRS=F)   #first index changes fastests
  #convert sphere with angle offsets
  ds <- f_rootangles(ds$alpha0,ds$beta0, alphaoffset,betaoffset)
  #get coordinates of sphere points
  ds$x <- with(ds, cos(alpha0)*sin(beta0))
  ds$y <- with(ds, sin(alpha0)*sin(beta0))
  ds$z <- with(ds, cos(beta0))
  
  ## CREATE VERTICES INDICES (python indexing, e.g. '0' is index of first point)
  #create vertices for first set of triangles
  dsv1 <- data.frame(
    i = rep(0:(nalpha-1),(nbeta-1)) + nalpha*rep(0:(nbeta-2),each=nalpha),
    j = rep(c(1:(nalpha-1),0),(nbeta-1)) + nalpha*rep(0:(nbeta-2),each=nalpha)
  )
  dsv1$k <- dsv1$j + nalpha
  #create vertices for second set of triangles
  dsv2 <- data.frame(
    i = rep(0:(nalpha-1),(nbeta-1)) + nalpha*rep(1:(nbeta-1),each=nalpha),
    j = rep(c(1:(nalpha-1),0),(nbeta-1)) + nalpha*rep(1:(nbeta-1),each=nalpha)
  )
  dsv2$k <- dsv2$i - nalpha
  #merge
  dsv <- rbind(dsv1,dsv2)
  
  ## RETURN
  return(list(coordinates=ds, vertices=dsv))
}


###############################
### MISCELLANEOUS FUNCTIONS ###
###############################

#function to get single line array based on list of root positions. Individual roots are separatered by NA to force plotly to plot seperate lines
f_rootpositionsline <- function(do) {
  # FUNCTION to create arrays for plotting root axes in unit sphere, in plotly
  #          coordinates of individual lines are separated using NA values 
  #          each line is characterised by a start and end point
  # INPUT
  # - <do>: dataframe with root orientations. should contain the fields:
  #         - alpha0: root azimuth angle [rad]
  #         - beta0:  root elevation angle [rad]
  #         - weight: relative weight of root orientation [-]
  # OUTPUT
  # - dataframe with arrays with fields:
  #   - x:      x-positions in unit sphere
  #   - y:      y-positions in unit sphere
  #   - z:      z-positions in unit sphere
  #   - alpha0: azimuth angle of every point [rad]
  #   - beta0:  elevation angle of every point [rad]
  #   - weight: relative weight of every root, specified at every point
  
  return(data.frame(
    x     =as.vector(t(cbind(do$x,-do$x,NA))),
    y     =as.vector(t(cbind(do$y,-do$y,NA))),
    z     =as.vector(t(cbind(do$z,-do$z,NA))),
    alpha0=as.vector(t(cbind(do$alpha0,-do$alpha0,NA))),
    beta0 =as.vector(t(cbind(do$beta0,-do$beta0,NA))),
    weight=as.vector(t(cbind(do$weight,do$weight,NA))) )
  )
}

#function to get positions in unit sphere for range of roots in 2D
f_rootrange2D <- function(beta0max, alphaoffset=0, betaoffset=0, n=25) {
  # FUNCTION to create a line on a unit sphere specifying the range of root orientations when these are specified in 2D.
  # INPUT
  # - <beta0max>     maximum elevation angle of root orientation range [rad]
  # - <alphaoffset>: coordinate system azimuth offset [rad]
  # - <betaoffset>:  coordinate system elevation offset [rad]
  # OPTIONAL INPUT
  # - <n>:           number of points on line
  # OUTPUT
  # - dataframe with the following fields
  #   - alpha0: azimuth angles
  #   - beta0:  elevation angles
  #   - x:      x-positions in unit sphere
  #   - y:      y-positions in unit sphere
  #   - z:      z-positions in unit sphere

  #create dataframe and discretize
  dr2  <- data.frame(alpha0=0, beta0=seq(-beta0max,beta0max,l=n))
  #convert to right coordinate system
  temp <- f_rootangles(dr2$alpha0, dr2$beta0, alphaoffset,betaoffset)
  dr2$alpha0 <- temp$alpha0
  dr2$beta0  <- temp$beta0
  #add positions
  dr2 <- f_polar2cartesian(dr2)
  #return
  return(dr2)
}

#function to get grid lines for 3D discrete root orientations - using grid method
f_3Drootorientations_grid <- function(nori_requested, beta0max, alphaoffset=0, betaoffset=0, np=50) {
  # FUNCTION to create grid lines belonging to root orientation mesh
  # INPUT:
  # - <nori_requested>: number of requested root orienations
  # - <beta0max>:       maximum elevation angle of spherical cap [rad]
  # OPTIONAL INPUT:
  # - <alphaoffset>:    azimuth offset of coordinate system [rad]
  # - <betaoffset>:     elevation offset of coordinate system [rad]
  # - <np>:             number of points per grid line
  # OUTPUT:
  # - data.frame with fields
  #   - <alpha0>: azimuth angle [rad]. Individual grid lines are separated by <NA> values
  #   - <beta0>:  elevation angle [rad]. Individual grid lines are separated by <NA> values
  
  #grid - get number of columns
  dnrc <- f_nrowscols(nori_requested)
  #temp - rows
  x1 <- cbind(matrix(rep(seq(-1,1,l=np),dnrc$nrow+1),nrow=dnrc$nrow+1,byrow=T), matrix(rep(NA,dnrc$nrow+1),ncol=1))
  y1 <- cbind(matrix(rep(seq(-1,1,l=dnrc$nrow+1),np),nrow=dnrc$nrow+1,byrow=F), matrix(rep(NA,dnrc$nrow+1),ncol=1))
  #temp - cols
  y2 <- cbind(matrix(rep(seq(-1,1,l=np),dnrc$ncol+1),nrow=dnrc$ncol+1,byrow=T), matrix(rep(NA,dnrc$ncol+1),ncol=1))
  x2 <- cbind(matrix(rep(seq(-1,1,l=dnrc$ncol+1),np),nrow=dnrc$ncol+1,byrow=F), matrix(rep(NA,dnrc$ncol+1),ncol=1))
  #grid coordinates
  d  <- data.frame(x=c(as.vector(t(x1)), as.vector(t(x2))),
                   y=c(as.vector(t(y1)), as.vector(t(y2))))
  #convert to azimuth/beta
  d <- f_convert2polar(d, beta0max)
  #rotate for any offsets
  temp <- with(d, f_rootangles(alpha0,beta0, alphaoffset,betaoffset))
  d$alpha0 <- temp$alpha0
  d$beta0  <- temp$beta0
  #positions
  d <- f_polar2cartesian(d)
  #return
  return(d[,c('alpha0','beta0','x','y','z')])
}

  