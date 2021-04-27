# File containing functions required to calculate root-reinforcement
# 27/08/2019 - G.J. Meijer

#uses plyr

##############################
### EXPAND GRID DATAFRAMES ###
##############################

## FUNCTION TO EXPAND THE ROWS OF MULTIPLE DATAFRAME - GET ALL COMBINATIONS OF ROWS
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))


###############################
### FUNCTIONS - ROOT ANGLES ###
###############################

## FUNCTIONS for find orientations of roots in shear plane coordinate system
f_rootangles <- function(a0d,b0d, af,bf){
  # FUNCTION to calculate initial root angles by transforming angles in rotated coordinate system back to
  #          angles in shear plane coordinate system
  # INPUT
  # - a0d: Azimuth angle in rotated coordinate system (vector) [rad]
  # - b0d: Elevation angle in rotated coordinate system (vector) [rad]
  # - af:  Azimuth angle between shear plane coordinate system and rotated coordinate system (scalar) [rad],   
  #        usually indicated with parameter <alphaoffset> throughout model
  # - bf:  Elevation angle between shear plane coordinate system and rotated coordinate system (scalar) [rad]
  #        usually indicated with parameter <betaoffset> throughout model
  # OUTPUT
  # - list containing two fields
  #   - <alpha0>: array of azimuth angles, in shear plane coordinate system
  #   - <beta0>:  array of elevation angles, in shear plane coordinate system
  #
  # Coordinate transformation applied:
  #   [ux]   [cos(a0)*sin(b0)]   [cos(af), -sin(af), 0]   [ cos(bf), 0, sin(bf)]   [cos(a0d)*sin(a0d)]
  #   [us] = [sin(a0)*sin(b0]] = [sin(af),  cos(af), 0] * [ 0,       1, 0      ] * [sin(a0d)*sin(a0d)]
  #   [uz]   [        cos(b0)]   [0      ,  0      , 1]   [-sin(bf), 0, cos(bf)]   [         cos(a0d)]
  
  #calculate coordinate transformation: 
  ux <- cos(af)*cos(a0d)*cos(bf)*sin(b0d) - sin(af)*sin(a0d)*sin(b0d) + cos(af)*cos(b0d)*sin(bf)
  uy <- cos(af)*sin(a0d)*sin(b0d) + cos(b0d)*sin(af)*sin(bf) + cos(a0d)*cos(bf)*sin(af)*sin(b0d)
  uz <- cos(bf)*cos(b0d) - cos(a0d)*sin(bf)*sin(b0d)  
  
  #calculate angles
  b0         <- acos(uz)      # in range [0 <= beta0 <= pi]
  a0         <- atan2(uy, ux) # R function for atan(y/x), in range [-pi <= alpha0 < pi]
  
  #return
  return(data.frame(alpha0=a0, beta0=b0))
}


######################################
### FUNCTION - CREATE ORIENTATIONS ###
######################################


#function to convert polar positions to 3D cartesian (unit circle)
f_polar2cartesian <- function(d){
  # FUNCTION to convert polar positions to cartesian positions on unit circle
  # INPUT
  # <d>: dataframe, containing the following fields
  #      - <alpha0>: azimuth angle [rad]
  #      - <beta0>:  elevation angle [rad]
  # OUTPUT:
  # - function adds fields to dataframe:
  #   - <x>: x-position 
  #   - <y>: y-position
  #   - <z>: z-position
  
  #convert
  d$x <- with(d, cos(alpha0)*sin(beta0))
  d$y <- with(d, sin(alpha0)*sin(beta0))
  d$z <- with(d, cos(beta0))
  #return
  return(d)
}

#sinus integral function
f_Si <- function(x,n=5){
  # FUNCTION to approximate the sin-integral function:
  #     Si(x) = int sin(t)/t tx   from t=0 to t=x
  #   this function can be approximated by series expansion
  #     Si(x) = sum_{k=1}^{infty} (-1)^(k-1) * x^(2k-1) / ((2k-1)*(2k-1)!)
  #   (see http://mathworld.wolfram.com/SineIntegral.html)
  #   for the domain 0<=x<=pi/2, the maximum incurred error (at x=pi/2) by changing <infty> sum limit to <n> is
  #     n=1: 14.59%
  #     n=2: -1.12%
  #     n=3: 0.0475%
  #     n=4: -0.00128%
  #     n=5: 2.36e-5%
  # INPUT
  # - <x>: vector with x-values
  # OPTIONAL INPUT
  # - <n>: number of series in series expansion, max(k)
  # OUTPUT
  # - vectot with integrals, same length as input <x>
  
  #get matrix with x values (rows) for every k-value used (columns)
  X  <- matrix(rep(x,n), ncol=n)
  #get matrix with k values (columns) for every x-value used (rows)
  k  <- matrix(rep(seq(1,n),each=length(x)), ncol=n)
  #calculate, sum over rows and return vector
  return(rowSums((-1)^(k-1) * X^(2*k-1) / ((2*k-1)*factorial(2*k-1))))
}

#function to select number of rows and colums in grid (if 3D-root orientations used)
f_nrowscols <- function(nori_requested){
  # FUNCTION to generate number of rows and colums in a rectangular grid
  # INPUT:
  # - <nori_requested>: number of requested root orientations
  # OUTPUT:
  # - dataframe with fields:
  #   - <nrow>: number of rows
  #   - <ncol>: number of columns
  
  #grid - get number of columns
  ncol <- ceiling(sqrt(nori_requested))
  #grid - number of rows (always equal or smaller than number of columns)
  nrow <- ceiling(nori_requested/ncol)
  #return
  return(data.frame(ncol=ncol, nrow=nrow))
}

#function to convert <x,y> rectangular mesh grid to polar coordinate system
f_convert2polar <- function(d, beta0max){
  # FUNCTION to convert positions in <x,y> grid to polar coordinate system. Domain of <x><y> is:
  #            -1 <= x <= 1
  #            -1 <= y <= 1
  #          these cartesian coordinates are converted to polar coordinates in the following fashion:
  #          - x=0, y=0 concides with the <z> axis, i.e. beta=0
  #          - on the the domain x>0, -x<=y<=x:
  #            - beta0  = x*beta0max (i.e. elevation angle ~ x-coordinate)
  #            - alpha0 = pi/4 * y/x (i.e. azimuth ~ arc length around square grid)
  # INPUT
  # - <d>: dataframe, with the following fields
  #        - x: x-position in cartesian system (-1<=x<=1) 
  #        - y: y-position in cartesian system (-1<=y<=1)
  # - <beta0max>: maximum elevation angle of spherical cap
  # OUTPUT
  # - added fields to dataframe <d>
  #   - <alpha0>: azimuth angle in radians
  #   - <beta0>:  elevation angle in radians
  
  #declare memory space
  d$alpha0 <- 0
  d$beta0  <- 0
  #azimuth angle - first quadrant (right: x>0 & -x<=y<=x)
  d$alpha0[d$x>0 & abs(d$y)< abs(d$x) & !is.na(d$x)] <- with(d[d$x>0 & abs(d$y)< abs(d$x) & !is.na(d$x),], pi/4*y/x)
  #azimuth angle - second quadrant (top: y>=0 & -y<=x<=y)
  d$alpha0[d$y>0 & abs(d$x)<=abs(d$y) & !is.na(d$x)] <- with(d[d$y>0 & abs(d$x)<=abs(d$y) & !is.na(d$x),], pi/2 - pi/4*x/y)
  #azimuth angle - third quarter (left: x<0 & -x<=y<=x)
  d$alpha0[d$x<0 & abs(d$y)< abs(d$x) & !is.na(d$x)] <- with(d[d$x<0 & abs(d$y)< abs(d$x) & !is.na(d$x),], pi + pi/4*y/x)
  #azimuth angle - fourth quadrant (bottom: y<=0 & -y<=x<=y)
  d$alpha0[d$y<0 & abs(d$x)<=abs(d$y) & !is.na(d$x)] <- with(d[d$y<0 & abs(d$x)<=abs(d$y) & !is.na(d$x),], -pi/2 - pi/4*x/y)
  #azimuth angles - set within range [-pi <= alpha0 <= pi]
  d$alpha0 <- d$alpha0%%(2*pi) - pi
  #elevation angle <beta0>
  d$beta0 <- with(d, beta0max*pmax(abs(y),abs(x)))
  #return
  return(d)
}

#function to get 3D discrete root orientations - using grid method
f_3Drootorientations <- function(nori_requested, beta0max) {
  # FUNCTION to get 3D orientations of roots, (relatively) uniformly distributed over a spherical cap
  #          a rectangular grid is stretched over the spherical cap. Mid points of grid cells form 
  #          discrete root orientations. Per grid cell on the spherical cap, the area is calculated which
  #          is then transformed into a weighting function (relative area of each cell with respect to total
  #          area)
  # INPUT
  # - <nori-requested>: requested number of root orientations
  # - <beta0max>:       max. elevation angle of spherical cap
  
  #grid - get number of columns
  dnrc <- f_nrowscols(nori_requested)
  #create dataframe with positions of each node (in rectangular coordinates, -1<=x<=1, -1<=y<=1)
  d <- expand.grid(x = seq(0.5,(dnrc$ncol-0.5))/(dnrc$ncol)*2-1,
                   y = seq(0.5,(dnrc$nrow-0.5))/(dnrc$nrow)*2-1, 
                   KEEP.OUT.ATTRS=F)
  #x-boundaries of each grid cell
  d$x1 <- with(d, x-1/(dnrc$ncol))
  d$x2 <- with(d, x+1/(dnrc$ncol))
  #y-boundaries of each grid cell
  d$y1 <- with(d, y-1/(dnrc$nrow))
  d$y2 <- with(d, y+1/(dnrc$nrow))
  
  #do calculations for one quarter only (top right quarter only, x>0 & y>0)
  dp <- d[d$x>=0 & d$y>=0,]
  #round lower limits to zero
  dp$x1[dp$x1<0] <- 0
  dp$y1[dp$y1<0] <- 0
  #check intersection y=x line with <y1> and <y2>, and limits between <x1> and <x2>
  dp$xy1 <- with(dp, pmin(pmax(y1,x1),x2))
  dp$xy2 <- with(dp, pmin(pmax(y2,x1),x2))
  #get area functions
  f_intxbox <- function(beta0max,x1,x2,y1,y2) {
    #Integral of area in domain:
    #  x1 <= x <= x2
    #  y1 <= y <= y2
    #  abs(x) >= abs(y)
    return(pi/4*beta0max*(y2-y1)*(f_Si(beta0max*x2)-f_Si(beta0max*x1)))
  }
  f_intxtri <- function(beta0max,x1,x2,y1) {
    #Integral of area in domain
    #  x1 <= x <= x2
    #  y1 <= y <= x
    #  abs(x) >= abs(y)
    return(pi/4*(cos(beta0max*x1)-cos(beta0max*x2)) - pi/4*beta0max*y1*(f_Si(beta0max*x2)-f_Si(beta0max*x1)))
  }
  f_intytri <- function(beta0max,x1,x2,y2) {
    #Integral of area in domain
    #  x1  <= x <= x2
    #  x   <= y <= y2
    #  abs(x) >= abs(y)
    return(pi/4*beta0max*f_Si(beta0max*y2)*(x2-x1) - pi/4*beta0max*
             ((x2*f_Si(beta0max*x2) + cos(beta0max*x2)/beta0max) - 
                (x1*f_Si(beta0max*x1) + cos(beta0max*x1)/beta0max)) )
  }
  #total area
  dp$weight <- with(dp, (f_intxbox(beta0max,xy2,x2,y1,y2) + 
                         f_intxbox(beta0max,y1,y2,x1,xy1) +
                         f_intxtri(beta0max,xy1,xy2,y1) +
                         f_intytri(beta0max,xy1,xy2,y2)) )
  #if grid cell on symmetry axis - multiply
  dp$weight[dp$x==0] <- 2*dp$weight[dp$x==0]
  dp$weight[dp$y==0] <- 2*dp$weight[dp$y==0]
  #copy and add second quadrant
  dp_temp1        <- dp[dp$x>0,]
  dp_temp1$x      <- -dp_temp1$x
  dpall      <- rbind(dp,dp_temp1)
  #copy and add third and fourth quadrant
  dp_temp2   <- dpall[dpall$y > 0,]
  dp_temp2$y <- -dp_temp2$y
  dpall      <- rbind(dpall, dp_temp2)
  #normalise weight over cap area
  dpall$weight <- dpall$weight / (2*pi*(1-cos(beta0max)))
  #add azimuth and elevation angles
  dpall <- f_convert2polar(dpall, beta0max)
  #return
  return(dpall[,c('alpha0','beta0','weight')])
}

#generate root orientations
f_generateRootOrientations <- function(dio) {
  # FUNCTION to generate uniformly distributed root orientations. Uniform distribution of root orientations is approximated
  #          using a finite amount of orientations. These are equally spread over the selected range, in a coordinate system 
  #          offset by azimuth <alpha0_offset> and elevation <beta0_offset> with respect to the shear plane coordinate system
  # INPUT
  # - dio: dataframe with input variables in SI system, containing the following fields
  #       - ndimension:    number of dimensions in root orientation [integer]
  #                        1 = single orienation
  #                        2 = uniform root distribution in x-z plane. i.e. all root azimuth angles = 0
  #                        3 = uniform root distribution in full 3D, using on Lebedev quadrature approximation
  #       - norientations: (minimum) number of discrete orientations required in bundle [integer]
  #                        function will minimize the number of discrete orientations ensuring at least <nmin> orientations are present
  #       - beta0max:     Maximum elevation angle in the bundle [rad]
  #       - alpha_offset: Azimuth offset to apply to orientations [rad]
  #       - beta_offset:  Elevation offset to apply to orientations [rad]
  # OUTPUT
  # - list with output, containing the following fields
  #                  - alpha0: azimuth angle [rad]
  #                  - beta0:  Elevation angle [rad]
  #                  - weight: relative weight of each orientation (total sum of weights amounts to 1) [-]
  
  #check for dimension
  if (dio$ndim=='1' | dio$beta0max==0 | dio$nori==1) {
    #1D --> single orientation 
    do <- data.frame(alpha0 = 0,
                     beta0 = 0,
                     weight = 1)
  } else if (dio$ndim == '2') {
    #2D --> fan 
    do <- with(dio, data.frame(alpha0 = c(rep(pi,floor(nori/2)), rep(0,(nori-floor(nori/2)))),
                               beta0 = abs((1/(nori)*(1:nori) - 0.5/nori)*2*beta0max - beta0max), 
                               weight = 1/nori))
  } else if (dio$ndim == '3') {
    #3D --> mesh approach
    do <- f_3Drootorientations(dio$nori, dio$beta0max)
  } else {
    #error message
    stop('INPUT: unknown number of distribution dimensions given')
  }
  
  #adjust angles for central angle offset
  temp <- with(dio, f_rootangles(do$alpha0, do$beta0, alphaoffset, betaoffset))
  #assign to output
  do$alpha0 <- temp$alpha0
  do$beta0  <- temp$beta0
  
  #return
  return(do)
}


##################################################
### ROOT PROPERTIES AND DIAMETER DISTRIBUTIONS ###
##################################################

#create root diameter classes
f_rootDiameters <- function(did){
  # FUNCTION to generate a dataframe containing all root diameters/length and associated biomechanical properties
  # INPUT
  # - di:  dataframe with input variables in SI system, containing the following fields
  #       - <phir>:       Total root volume fraction [m^3/m^3]
  #       - <zetar>:      distribution parameter governing the spread of root volume over all diameters [-]. Defined as:
  #                         zetar = (phir(drmax)-phir(drmin)) / (phir(drmax)+phir(drmin))
  #       - <drmin>:      smallest root diameter in bundle [m]
  #       - <drmax>:      largest root diameter in bundle [m]
  #       - <nd>:         Number of discrete root orientations [integer]
  #       - <drref>:      root reference diameter
  # OUTPUT
  # - dataframe with root diameters (dr) and volume fractions per diameter class (phir)

  #Root diameters, quantities
  if (did$drmin==did$drmax){
    #if drmin==drmax, reduce number of discrete root diameters to 1
    dd <- with(did, data.frame(dr_classmin=drmin,
                                 dr_classmax=drmax,
                                 dr         =drmin,
                                 phir       =phir))
  } else {
    #create range of diameters
    dd <- with(did, data.frame(dr_classmin=((0:(nd-1))/nd) * (drmax-drmin) + drmin,
                                 dr_classmax=((1:nd)    /nd) * (drmax-drmin) + drmin))
    #average diameter in class
    dd$dr     <- with(dd, 0.5*(dr_classmin + dr_classmax))
    #calculate root fractions per diameter - power law
    if (did$bphi==-1){
      #catch exception for integral: bphi=-1
      dd$phir <- with(dd, did$phir * log(dr_classmax/dr_classmin) / log(did$drmax/did$drmin))                     
    } else {
      #all other values for bphi
      aphi    <- with(did, phir*(1+bphi)*drref^bphi / (drmax^(1+bphi)-drmin^(1+bphi)) )
      dd$phir <- with(dd, aphi/(1+did$bphi)*did$drref^(-did$bphi) * (dr_classmax^(1+did$bphi)-dr_classmin^(1+did$bphi)))
    }
  }  
  #return dataframe
  return(dd)
}

#create root properties
f_rootProperties <- function(dd, dip, drref) {
  # FUNCTION to generate a dataframe containing all root diameters/length and associated biomechanical properties
  # INPUT
  # - dd:  dataframe with root diameter classes, should contain field <dr>
  # - dip: dataframe with input variables in SI system, containing the following fields
  #       - <aL>:         Root length property, as function of root diameter <dr> (multiplicator) [m]
  #                         Lr(dr) = aL * (dr/drref)^bL
  #       - <bL>:         Root length property (power) [-]
  #       - <at>:         Root tensile strength, as function of root diameter <dr> (multiplicator) [Pa]
  #                         tru(dr) = at * (dr/drref)^bt
  #       - <bt>:         Root tensile strength (power) [-]
  #       - <aepsilon>:   Root tensile strain to failure, as function of root diameter <dr> (multiplicator) [m/m]
  #                         eru(dr) = aepsilon * (dr/drref)^bepsilon
  #       - <bepsilon>:   Root tensile strain to failure (power) [-]
  #       - <trytru>:     Root yield strain/tensile strenght ratio [Pa/Pa]
  #       - <eryeru>:     Root yield strain/tensile strain to failure ratio [(m/m)/(m/m)]
  #       - <kappat>:     Root tensile strength weibull distribution shape parameter [-]
  #       - <drref>:      root reference diameter
  # OUTPUT
  # - dataframe with root properties added
  
  #get biomechanical properties, lenght etc.
  dd$Cr     <- with(dd, pi*dr)                                        # Root circumference [m]
  dd$Ar     <- with(dd, pi/4*dr^2)                                    # Root cross-sectional area [m^2]
  dd$Lr     <- with(dd, dip$aL * (dr/dip$drref)^dip$bL)               # Root length [m]
  dd$tru    <- with(dd, dip$at * (dr/dip$drref)^dip$bt)               # Root tensile strength [Pa]
  dd$try    <- with(dd, tru * dip$trytru)                             # Root yield strength [Pa]
  dd$eru    <- with(dd, dip$aepsilon * (dr/dip$drref)^dip$bepsilon)   # Root tensile strain to failure [m/m]
  dd$ery    <- with(dd, eru * dip$eryeru)                             # Root yield strain [m/m]
  dd$Ere    <- with(dd, ifelse(ery==0, 
                                   tru/eru,
                                   try/ery))                              # Root Young's modulus [Pa]
  dd$Erep   <- with(dd, ifelse(ery==eru,
                                   tru/eru,
                                   (tru-try)/(eru-ery)))                  # Root Elasto-plastic stiffness [Pa]
  dd$kappat <- dip$kappat                                                # Weibull shape parameter [-]
  #return
  return(dd)
}


########################################################
### FUNCTIONS - ALL ORIENTATIONS AND ROOT PROPERTIES ###
########################################################

## FUNCTIONS for get all cmbinations of seperately defined orientations and root properties
f_CombineOrientationsRootproperties <- function(do, dr) {
  # FUNCTION to generate a dataframe containing all root diameters/length and associated biomechanical properties
  # INPUT
  # - <do>: Dataframe with all orientations and weights of all orientations
  # - <dr>: Dataframe with all bundles of roots
  # OUTPUT
  # - <df>: Dataframe with all roots that need calculated. Combines all orientations with all sets of root properties
  
  #combine all orientations with all diameters
  df <- expand.grid.df(do, dr)
  #adjust <phir> for orientation weight
  df$phir <- with(df, phir*weight)
  #assign unique identifier
  df$RootID <- seq(1,dim(df)[1])
  #only keep specific columns
  col_names_keep <- c('RootID','phir','alpha0','beta0', 'Cr','Ar','Lr', 'try','tru','Ere','Erep','kappat')
  #return
  return(df[,col_names_keep])
}

#######################
### UNIT CONVERSION ###
#######################

f_inputunitsystem <- function(lengthunit_radio, stressunit_radio){
  # FUNCTION to create conversion factors and strings for units for every model input
  # INPUT
  # - lengthunit_radio: radio button input for length unit
  # - stressunit_radio: radio button input for root stress/strength/stiffness unit
  # OUTPUT
  # - list with two fields: <length> and <stress>. Each contains a dataframe with the fields
  #   - unit_SI:   String containing the SI unit, e.g. "Pa"
  #   - unit_user: String containing the input unit used by the user, e.g. "MPa"
  #   - factor:    Multiplication factor required to get user-defined unit system to SI
  
  #Create empty list
  ddef <- data.frame(factor=rep(1,2), unit_user=rep(NA,2), unit_SI=rep(NA,2), row.names=c('length','stress'))
  #Length scale
  ddef['length','unit_SI'] <- 'm'
  if (lengthunit_radio==1) {
    ddef['length','factor']    <- 1e-3
    ddef['length','unit_user'] <- 'mm'
  } else if (lengthunit_radio==2) {
    ddef['length','factor']    <- 1e-2
    ddef['length','unit_user'] <- 'cm'
  } else if (lengthunit_radio==3) {
    ddef['length','factor']    <- 1
    ddef['length','unit_user'] <- 'm'
  }
  #Stress scale
  ddef['stress','unit_SI'] <- 'Pa'
  if (stressunit_radio==1) {
    ddef['stress','factor']    <- 1
    ddef['stress','unit_user'] <- 'Pa'
  } else if (stressunit_radio==2) {
    ddef['stress','factor']    <- 1e3
    ddef['stress','unit_user'] <- 'kPa'
  } else if (stressunit_radio==3) {
    ddef['stress','factor']    <- 1e6
    ddef['stress','unit_user'] <- 'MPa'
  } else if (stressunit_radio==4) {
    ddef['stress','factor']    <- 1e9
    ddef['stress','unit_user'] <- 'GPa'
  }
  #return
  return(ddef)
}