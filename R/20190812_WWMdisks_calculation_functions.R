# File containing functions required to calculate root-reinforcement
# 28/05/2019 - G.J. Meijer


####################################
### FUNCTIONS - EQUATION SOLVING ###
####################################

## FUNCTIONS for finding the root of polynomial equations

# SOLVE CUBIC EQUATION - ROOT SOLVING
f_solve_p3 <- function(a,b,c,d){
  # FUNCTION that returns the largest real root of the cubic function:
  #   a*x^3 + b*x^2 + c*x^2 + d == 0 
  # with the following assumptions that follow from the problem definition:
  # - a,b,c >= 0
  # - d     <= 0
  # because of these assumptions, there always exists a real, positive root
  #
  # INPUT
  # - a,b,c,d: arrays of parameters
  # OUTPUT
  # - x:       array with largest real root of the above cubic equation
  
  #max length of array of input parameters
  len     <- max(length(a), length(b), length(c), length(d))
  #construct empty output vector
  x       <- numeric(len) 
  #new parameters - so that: x^3 + e*x^2 + f*x + g = 0
  #also ensures all arrays have the same length
  e       <- rep_len(b/a, len)
  f       <- rep_len(c/a, len)
  g       <- rep_len(d/a, len)
  #temporary values
  Q       <- (e^2 - 3*f)/9
  R       <- (2*e^3 - 9*e*f+27*g)/54
  ind     <- R^2 < Q^3    #if true, 3 real roots exist, if false, only one real root exists
  #three real roots - calculate largest value
  theta   <- acos(R[ind] / sqrt(Q[ind]^3))
  x[ind]  <- -2*sqrt(Q[ind])*cos((theta+2*pi)/3) - e[ind]/3
  #one real root
  A       <- -sign(R[!ind])*(abs(R[!ind]) + sqrt(R[!ind]^2-Q[!ind]^3))^(1/3)
  B       <- Q[!ind]/A
  x[!ind] <- (A+B) - e[!ind]/3
  #x=0 solution (when d==0)
  x[d==0] <- 0
  #return array of solutions
  return(x)
}

# SOLVE QUADRATIC EQUATION - ROOT SOLVING
f_solve_p2 <- function(a,b,c){
  # FUNCTION that returns the largest real root of the quadratic function:
  #   a*x^2 + b*x^2 + c == 0 
  # with the following assumptions that follow from the problem definition:
  #   a < 0
  # INPUT
  # - a,b,c:   arrays of parameters
  # OUTPUT
  # - x:       array with largest real root of the above quadratic equation

  #max length of array of input parameters
  len     <- max(length(a), length(b), length(c))
  #ensure all arrays have the same length
  a       <- rep_len(a, len)
  b       <- rep_len(b, len)
  c       <- rep_len(c, len)
  #Discriminant
  D       <- b^2 - 4*a*c
  #calculate root (get largest root)
  x       <- (-b - sqrt(D)) / (2*a)
  #return array of solutions
  return(x)
}


############################
### FUNCTIONS - GEOMETRY ###
############################

## FUNCTIONS for calculating the orientations of the deformed roots
#  INPUT
#  - alpha0: undisplaced root azimuth
#  - beta0:  undisplaced root elevation
#  - u:      current shear displacement
#  - h:      current shear zone thickness
#  OUTPUTS
#  - alpha:  displaced root azimuth
#  - beta:   displaced root elevation
#  NOTES
#  - Coordinate system (right-handed)
#    - x: points in direction of shear displacement
#    - y: coordinate on shear plane, perpendicular to direction of shear displacement
#    - z: points in direction perpendicular to shear displacement.
#         positive direction in direction of positive soil shear strains
#  - azimuth:   Angle between projected root orientation on x-y plane, and the x-axis
#  - elevation: Angle between root orientation and z-axis

# FUNCTION TO CALCULATE COSINE OF DISPLACED AZIMUTH
f_cosalpha <- function(alpha0,beta0,u,h){
  xi_x     <- u + h*cos(alpha0)*tan(beta0)
  xi_y     <- h*sin(alpha0)*tan(beta0)
  cosalpha <- xi_x / sqrt(xi_x^2 + xi_y^2)
  return(cosalpha)
}
# FUNCTION TO CALCULATE SINE OF DISPLACED ELEVATION
f_sinbeta <- function(alpha0,beta0,u,h){
  xi_x     <- u + h*cos(alpha0)*tan(beta0)
  xi_y     <- h*sin(alpha0)*tan(beta0)
  sinbeta  <- sqrt(xi_x^2 + xi_y^2) / sqrt(xi_x^2 + xi_y^2 + h^2)
  return(sinbeta)
}
# FUNCTION TO CALCULATE COSINE OF DISPLACED ELEVATION
f_cosbeta <- function(alpha0,beta0,u,h){
  xi_x     <- u + h*cos(alpha0)*tan(beta0)
  xi_y     <- h*sin(alpha0)*tan(beta0)
  cosbeta  <- h / sqrt(xi_x^2 + xi_y^2 + h^2)
  return(cosbeta)
}


#################################
### FUNCTIONS - ROOT STRESSES ###
#################################

## FUNCTIONS for calculating tensile stresses in roots
#  INPUT
#  - Cr:        Root circumference
#  - Ar:        Root cross-sectional area
#  - Lr:        Root length (shear plane assumed in middle)
#  - try:       Root yield stress (transition elastic --> elastoplastic behaviour)
#  - tru:       Root tensile strength
#  - Ere:       Root elastic tensile stiffness (i.e. when tr<=try)
#  - Erep:      Root elasto-plastic tensile stiffness (i.e. when tr>try)
#  - kappat:    Weibull distribution shape parameter for root tensile strength <tru>
#  - taui:      Root-soil interface friction
#  - cosalpha:  cosine of displaced root azimuth angle
#  - cosalpha0: cosine of undisplaced root azimuth angle
#  - cosbeta:   cosine of displaced root elevation angle
#  - cosbeta0:  cosine of undisplaced root elevation angle

### STRESS CALCULATION FUNCTIONS

## CALCULATE ROOT TENSILE STRAIN BASED ON ROOT TENSILE STRESS
f_epsr <- function(tr, try,Ere,Erep){
  #assume linear elastic
  epsr <- tr / Ere
  #indices in <tr> that go elasto-plastic
  ind <- tr>try
  #alter for elasto-plastic roots
  epsr[ind] <- try[ind] / Ere[ind] + (tr[ind]-try[ind]) / Erep[ind]
  #return
  return(epsr)
}

## ROOT SLIPPAGE STRESS - LINEAR ELASTIC ROOTS
f_trslip_linear <- function(Cr,Ar,Lr, Ere, taui, h,cosbeta) {
  #quadric polynomial terms
  xi2 <- -1/Ere
  xi1 <- Lr/2 * taui*Cr/Ar * 1/Ere - 1 
  xi0 <- 1/2 * taui*Cr/Ar * (Lr - h/cosbeta) 
  #solve quadratic equation
  trslip <- f_solve_p2(xi2, xi1, xi0)
  #return
  return(trslip)
}

## ROOT SLIPPAGE STRESS - BILINEAR ELASTIC ROOTS
f_trslip_bilinear <- function(Cr,Ar,Lr, try,Ere,Erep, taui, h,cosbeta) {
  #intermediate variables
  zeta <- try/Erep - try/Ere
  #quadric polynomial terms
  xi2 <- -1/Erep
  xi1 <- Lr/2 * taui*Cr/Ar * 1/Erep - 1 + zeta
  xi0 <- 1/2 * taui*Cr/Ar * (Lr*(1-zeta) - h/cosbeta) 
  #solve quadratic equation
  trslip <- f_solve_p2(xi2, xi1, xi0)
  #return
  return(trslip)
}

## ROOT ANCHORED STRESS - LINEAR ELASTIC ROOTS
f_tranch_linear <- function(Cr,Ar, Ere, taui, h,cosbeta,cosbeta0) {
  #cubic polynomial terms
  xi3 <- Ar/(taui*Cr) * 1/(Ere^2)
  xi2 <- Ar/(taui*Cr) * 1/Ere
  xi1 <- h/cosbeta0 * 1/Ere
  xi0 <- h/cosbeta0 - h/cosbeta
  #solve cubic equation
  tranch <- f_solve_p3(xi3, xi2, xi1, xi0)
  #return
  return(tranch)
}

## ROOT ANCHORED STRESS - BILINEAR ELASTIC ROOTS
f_tranch_bilinear <- function(Cr,Ar, try,Ere,Erep, taui, h,cosbeta,cosbeta0) {
  #intermediate variables
  zeta <- try/Erep - try/Ere
  #cubic polynomial terms
  xi3 <- Ar/(taui*Cr) * 1/(Erep^2)
  xi2 <- Ar/(taui*Cr) * 1/Erep * (1 - 3*zeta)
  xi1 <- h/cosbeta0 * 1/Erep + zeta*Ar/(taui*Cr) * (try/Erep - 2*(1-zeta))
  xi0 <- h/cosbeta0 - h/cosbeta + zeta * (Ar/(taui*Cr)*try*(1-zeta) - h/cosbeta0)
  #solve cubic equation
  tranch <- f_solve_p3(xi3, xi2, xi1, xi0)
  #return
  return(tranch)
}


### COMBINING SOLUTIONS FOR CASES

## CALCULATE CURRENT ROOT TENSILE STRESS - COMBINING ANCHORED/SLIPPING (NO BREAKAGE)
f_tr_unbroken <- function(Cr,Ar,Lr, try,Ere,Erep, taui,h,cosbeta,cosbeta0){
  # OUTPUT
  # List with two fields
  # - <tr_unbroken>: Root tensile stress (in unbroken root)
  # - <flag>         Flag indicating which solution was used
  #                  - 0: not in tension
  #                  - 1: Anchored - elastic
  #                  - 2: Anchored - elasto-plastic
  #                  - 3: Slipping - elastic
  #                  - 4: Slipping - elasto-plastic
  
  #tensile stress - initiate vector and fill with zeros (only for roots in tension)
  tr_unbroken <- numeric(length(try))
  #solution flag - initiate vector and fill with zeros (only for roots in tension)
  flag        <- numeric(length(try))
  #switch, is root in tension and not completely in shear zone? (1=yes, 0=no)
  s1 <- (cosbeta < cosbeta0) & (Lr*cosbeta > h)  
  #only do calculations when at least some roots are in tension
  if (any(s1)) {
    #1: ANCHORED SOLUTION
    #calculate tensile stress - linear elastic
    tra    <- f_tranch_linear(Cr[s1],Ar[s1], Ere[s1], taui, h,cosbeta[s1],cosbeta0[s1])
    flag_a <- rep(1, length(tra))
    #check if tensile stress exceeds yield stress
    s2 <- tra >= try[s1]
    #calculate tensile stress - bi-linear elastic
    if (any(s2)) {
      tra[s2]    <- f_tranch_bilinear(Cr[s1][s2],Ar[s1][s2], try[s1][s2],Ere[s1][s2],Erep[s1][s2], taui, h,cosbeta[s1][s2],cosbeta0[s1][s2])
      flag_a[s2] <- 2
    }
      
    #2: LINEAR ELASTIC, SLIPPING SOLUTION
    #calculate tensile stress - linear elastic
    trs    <- f_trslip_linear(Cr[s1],Ar[s1],Lr[s1], Ere[s1], taui, h,cosbeta[s1])
    flag_s <- rep(3, length(trs))
    #check if tensile stress exceeds yield stress
    s2 <- trs >= try[s1]
    #calculate tensile stress - bi-linear elastic
    if (any(s2)) {
      trs[s2]    <- f_trslip_bilinear(Cr[s1][s2],Ar[s1][s2],Lr[s1][s2], try[s1][s2],Ere[s1][s2],Erep[s1][s2], taui, h,cosbeta[s1][s2])
      flag_s[s2] <- 4
    }
      
    #3: TAKE MINIMUM
    tr_unbroken[s1]   <- pmin(tra, trs)
    flag[s1]          <- flag_a
    flag[s1][trs<tra] <- flag_s[trs<tra]
  }
  #return
  return(list(tr_unbroken=tr_unbroken, flag=flag))
}
 
## CALCULATE BREAKAGE PARAMETER
f_fbreak <- function(tr_unbroken, tru, kappat, fbreak_prev){
  #weibull scale parameter
  lambdat    <- tru / gamma(1+1/kappat)
  #root breakage parameter
  fbreak     <- exp(-(tr_unbroken/lambdat)^kappat)
  #return minimum of current and previous breakage parameter (to ensure roots do not 'unbreak' again)
  return(pmin(fbreak, fbreak_prev))
}


########################
### FUNCTIONS - SOIL ###
########################

## FUNCTIONS for calculating soil stresses, soil stability and reinforcements
#  INPUT
#  - c:    soil cohesion (Morh-Coulomb)
#  - phi:  soil friction angle (Morh-Coulomb)
#  - sign: normal stress acting on the shear plane (exclusing any root effects, positive in compression)
#  - other parameters (related to roots) explained in previous functions

## FUNCTION TO CALCULATE FALLOW SOIL SHEAR STRENGTH
f_tauf <- function(c, phi, sign){
  #return soil shear strength
  return(c + sign*tan(phi))
}

## FUNCTION TO CALCULATE DESTABILISING ROOT STRESS
f_taur <- function(tr,phir, phi, cosalpha,sinbeta,cosbeta){
  #soil volume fraction
  phis <- 1-sum(phir)
  #destabilising stress
  taur <- 1/phis * sum(phir*tr*(cosalpha*sinbeta - cosbeta*tan(phi)))
  #return destabilising stress roots apply on soil just outside shear zone
  return(taur)
}

## YIELD FUNCTION FOR STABILITY OF SOIL OUTSIDE THE CURRENT SHEAR ZONE
f_stab <- function(h,u, c,phi,sign,taui, Cr,Ar,Lr,phir,alpha0,beta0, try,tru,Ere,Erep,kappat,fbreak_prev) {
  #deformed root orientations
  cosalpha <- f_cosalpha(alpha0,beta0,u,h)
  sinbeta  <- f_sinbeta (alpha0,beta0,u,h)
  cosbeta  <- f_cosbeta (alpha0,beta0,u,h)
  #root tensile strength (no breakage included yet)
  tr_unbroken <- f_tr_unbroken(Cr,Ar,Lr, try,Ere,Erep, taui,h,cosbeta,cos(beta0))$tr_unbroken
  #breakage parameter
  fbreak   <- f_fbreak(tr_unbroken,tru,kappat,fbreak_prev)
  #fallow soil strength
  tauf     <- f_tauf(c, phi, sign)
  #shear load applied by roots on soil
  taur     <- f_taur(fbreak*tr_unbroken, phir, phi, cosalpha,sinbeta,cosbeta)
  #return yield criterion (negative=stable, positive=unstable)
  return(taur - tauf)
}

## CALCULATE ROOT_REINFORCEMENT PER ROOT
f_cr <- function(phi, tr,phir, cosalpha,sinbeta,cosbeta) {
  #reinforcement per root
  cr <- tr*phir*(cosalpha*sinbeta + cosbeta*tan(phi))
  #return root-reinforcement
  return(cr)
}


#############################
### FUNCTIONS - NUMERICAL ###
#############################

## FUNCTION for root-finding, based on Newton-Raphson approach
f_rootsolve_newtonraphson <- function(func, x0, tolerance= .Machine$double.eps^0.2, step_size=1e-6, max_iter=1e3, ...) {
  # FUNCTIONS for root-finding, based on Newton-Raphson approach
  #           i.e. find solution for <x> satisfying: func(x)=0  
  # INPUT
  # - func:      function name
  # - x0:        initial guess (try to be as close as possible for fast convergence)
  # - tolerance: termination criterion.
  #              solution accepted when: |func(x)| <= tolerance
  # - step_size: little step size used to calculate the derivative required for Newton-Raphson solving
  #              df/dx = (f(x+step_size)-f(x)) / step_size
  # - max_iter:  solving terminates when number of iteration steps exceeds <max_iter>
  
  # NOTES
  # - might not be the best for this problem, since the derivative of root-reinforcement, and therefore
  #   stability, is not necessarily a continuous function! Therefore spurious results may be obtained
  #   zero-finding using the bisecion method, although slower, is more reliable
    
  #iteration counts
  n   <- 1
  #calculate values for initial guess
  x   <- x0
  f1  <- func(x, ...)
  #loop until function value within <tolerace> of zero
  while (abs(f1)>tolerance & n<=max_iter) {
    f2 <- func(x + step_size, ...)
    x  <- x - f1 / ((f2-f1)/step_size)
    f1 <- func(x, ...)
    n  <- n + 1
  }
  #Display warning message of max. number of iterations exceeded
  if (n > max_iter){
    warning('Newton-Raphson root finding: Maximum number of iterations exceeded!')
  }
  #return
  return(x)
}

## FUNCTION for root-finding, based bisection approach
f_rootsolve_bisection <- function(func, lims, tolerance=.Machine$double.eps^0.5, max_iter=1e3, ...){
  # FUNCTIONS for root-finding, based on bisection approach
  #           i.e. find solution for <x> satisfying: func(x)=0  
  # INPUT
  # - func:      function name
  # - lims:       2-value array with left and right limits (function eval have to be opposite signs)
  # - tolerance: termination criterion.
  #              solution accepted when half of bisection domain <  <= tolerance
  # - max_iter:  solving terminates when number of iteration steps exceeds <max_iter>
  #
  # NOTES
  # - assumptions made are that the function is ever increasing or decreasing over the domain <lims>
  #   and that the function values at both domain ends are of the opposite sign. If the latter is not
  #   satisfied, the right limit is returned (lims[2])
  
  #bisection method - starting values 
  a <- lims[1]            #left side of domain
  b <- lims[2]            #right side of domain
  #function value at starting point
  fa <- func(a, ...)
  fb <- func(b, ...)
  #check if <fa> and <fb> are not of opposite sign
  if (fb*fa >= 0){
    #not of opposite sign, return <b>
    return(b)
  } else {
    #opposite sign - find zero by bisection
    c <- (a+b)/2           #midpoint of doimain - initial
    fc <- func(c, ...)     #function value at midpoint - initial
    n <- 1                 #iteration counter
    #bisection - loop
    while (fc!=0 & (b-a)/2 > tolerance) {
      #current midpoint
      c  <- (a+b)/2
      #function value at midpoint
      fc <- func(c, ...)
      #shrink domain
      if (sign(fc) == sign(func(a, ...))) {
        a <- c
      } else {
        b <- c
      }
      #update iteration counter
      n <- n + 1
    }
    #Display warning message of max. number of iterations exceeded
    if (n > max_iter){
      warning('Newton-Raphson root finding: Maximum number of iterations exceeded!')
    }
    #return
    return(c)
  }
}


############################
### FUNCTIONS - ANALYSIS ###
############################

f_runanalysis <- function(dr, di, updateProgress=NULL) {
  # FUNCTION to run an analysis using the various input parameters specified
  # INPUT
  # - <dr> dataframe with all root parameters/orientations
  #        vector fields required (i.e. dataframe may have multiple rows for multiple roots)
  #        - RootID: Unique root identifier
  #        - phir:   root volume fraction
  #        - alpha0: initial orientation - azimuth angle
  #        - beta0:  initial orientation - elevation angle
  #        - Cr:     root circumference length
  #        - Ar:     root cross-sectional area
  #        - Lr:     root length
  #        - try:    root yield strength
  #        - tru:    root tensile strength
  #        - Ere:    root elastic stiffness
  #        - Erep:   root elasto-plastic stiffness
  #        - kappat: root bundle breakage parameter
  # - <di> dataframe with all input parameters
  #        scalar fields required (i.e. dataframe with single row)
  #        - c:    soil cohesion
  #        - phi:  soil angle of internal friction
  #        - sign: normal effective soil stress on shear plane
  #        - taui: root-soil interface friction 
  #        - h0:   initial shear zone thickness 
  #        - hmax: maximum shear zone thickness
  #        - umax:  maximum shear displacement
  #        - nstep: number of steps
  # OUTPIT
  # - analysis outputs a list containing two dataframes
  #   - <sum>: summary of calculated results, contains fields
  #            - StepID:    number of displacement step
  #            - u:         current shear displacement
  #            - h:         current shear zone thickness
  #            - cr:        current root-reinforcement
  #            - WWMfactor: current equivalent WWM factor to achieve <cr> result
  #   - <all>: data per root, per displacement step. contains fields:
  #            - RootID:    root identified, corresponds with ID in dataframe <dr>
  #            - StepID:    number of displacement step, to link data to <sum>
  #            - tri:       current tensile stress in intact root
  #            - tr:        current tensile stress in root (after applying breakage parameter)
  #            - cr:        current root-reinforcement for this root
  #            - WWMfactor: current equivalent WWM factor to achieve <cr> result, for this root
  #            - flag:      integer explaining current behaviour type:
  #                         - 0: root not in tension or completely within shear zone
  #                         - 1: anchored, elastic
  #                         - 2: anchored, elastoplastic
  #                         - 3: slipping, elastic
  #                         - 4: slipping, elastoplastic

  #exit tolerance for bisection criterion (used for updating shear zone thickness, when this thickness is increasing)
  bisection_tolerance <- 1e-6 * di$umax
  
  #Create data frame for summary output
  dos <- data.frame(StepID    = seq(0,di$nstep), 
                    u         = seq(0,di$umax,l=di$nstep+1), 
                    h         = di$h0,
                    cr        = 0, 
                    WWMfactor = 0)

  #Create dataframe for output per root
  dor <- expand.grid(RootID    = dr$RootID, 
                     StepID    = dos$StepID,
                     tri       = 0, 
                     tr        = 0, 
                     cr        = 0,
                     WWMfactor = 0,
                     flag      = 0,
                     fbreak    = 1)
  
  #initial root breakage parameter
  dr$fbreak <- 1
  
  #loop through displacement steps
  for (j in 2:dim(dos)[1]) {  
    #check if shear zone is can increase in thickness
    if (dos$h[j-1] < di$hmax){
      #if allowed to increase - check stability of soil outwith current shear zone
      stab <- f_stab(dos$h[j-1],dos$u[j], di$c,di$phi,di$sign,di$taui, 
                     dr$Cr,dr$Ar,dr$Lr,dr$phir,dr$alpha0,dr$beta0, 
                     dr$try,dr$tru,dr$Ere,dr$Erep,dr$kappat,dr$fbreak)
      #increase <h> if not stable
      if (stab > 0) {
        #root solve yield function to find new shear zone thickness
        dos$h[j] <- f_rootsolve_bisection(f_stab, c(dos$h[j-1],di$hmax),  #bisection 
                                           u=dos$u[j], c=di$c,phi=di$phi,sign=di$sign,taui=di$taui,
                                           Cr=dr$Cr,Ar=dr$Ar,Lr=dr$Lr,phir=dr$phir,
                                           alpha0=dr$alpha0,beta0=dr$beta0, 
                                           try=dr$try,tru=dr$tru,Ere=dr$Ere,Erep=dr$Erep,kappat=dr$kappat,fbreak_prev=dr$fbreak,
                                           tolerance=bisection_tolerance)
      } else {
        #Shear plane edge stable, keep <h> thickness constant
        dos$h[j] <- dos$h[j-1]
      }
    } else {
      #max shear zone thickness reached, keep shear zone thickness from last step
      dos$h[j] <- dos$h[j-1]
    }
    
    #calculate displaced root orientations
    cosalpha     <- f_cosalpha(dr$alpha0,dr$beta0,dos$u[j],dos$h[j])
    sinbeta      <- f_sinbeta (dr$alpha0,dr$beta0,dos$u[j],dos$h[j])
    cosbeta      <- f_cosbeta (dr$alpha0,dr$beta0,dos$u[j],dos$h[j])
    #indices in <dor> to assign to
    ind          <- dor$StepID==dos$StepID[j]
    #calculate root tensile stresses (unbroken)
    temp          <- f_tr_unbroken(dr$Cr,dr$Ar,dr$Lr, dr$try,dr$Ere,dr$Erep, di$taui,dos$h[j],cosbeta,cos(dr$beta0))
    dor$tri[ind]  <- temp$tr_unbroken
    dor$flag[ind] <- temp$flag
    #calculate root breakage parameter
    dr$fbreak    <- f_fbreak(dor$tri[ind],dr$tru,dr$kappat,dr$fbreak)
    dor$fbreak[ind] <- dr$fbreak
    #calculate tensile stress (include broken)
    dor$tr[ind]  <- dor$tri[ind] * dr$fbreak
    #calculate reinforcement per root
    dor$cr[ind]  <- f_cr(di$phi, dor$tr[ind], dr$phir, cosalpha,sinbeta,cosbeta)
    #calculate reinforcement as fraction of total root tensile strength
    dor$WWMfactor[ind]  <- dor$cr[ind] / sum(dr$phir * dr$tru)
    
    #calculate total reinforcement
    dos$cr[j]    <- sum(dor$cr[ind])
    #Calculate WWM factor: k=cr/sum(phir*tru)
    dos$WWMfactor[j]    <- dos$cr[j] / sum(dr$phir * dr$tru)
    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      updateProgress()
    }
  }
  
  #return
  return(list(sum=dos, all=dor))
}

######################################################
### CALCULATE FRACTIONS OF ANCHORED/SLIPPING ROOTS ###
######################################################

#function to calculate fractions of root behaviour
f_fractions <- function(dall, dsum, dr) {
  # FUNCTION to calculate fractions of each root behaviour type (elastic, slipping etc)
  # INPUT
  # - <dall>: dataframe with calculation results: data for every step, for every root
  # - <dsum>: dataframe with calculation results: data for every step (summary)
  # - <dr>:   dataframe with all root properties, per root diameter
  # OUTPUT
  # - adds fields to summary data <dsum>:
  #     - <Fraction_NotInTension>:          root not in tension or completely within shear zone
  #     - <Fraction_AnchoredElastic>:       anchored, elastic
  #     - <Fraction_AnchoredElastoplastic>: anchored, elastoplastic
  #     - <Fraction_SlippingElastic>:       slipping, elastic
  #     - <Fraction_SlippingElastoplastic>: slipping, elastoplastic
  
  #get root volume fractions per root
  dall <- merge(dall[,c('RootID','StepID','fbreak','flag')], dr[,c('RootID','phir')], by='RootID', all=T)
  #get shear displacements per step
  dall <- merge(dall, dsum[c('StepID','u')], by='StepID', all=T)
  #complete all entries
  dall <- complete(dall, RootID, u, flag=seq(0,4), fill=list(phir=0, fbreak=0))
  #summarize -> create long data for root volume fraction in each category
  #dfrc <- ddply(dall, .(u,flag), summarize, phir=sum(phir*fbreak,na.rm=T)) 
  dfrc <- dall %>% group_by(u,flag) %>% summarise(phir=sum(phir*fbreak,na.rm=T)) 
  #if sum(phir)=0 --> replace resulting NA's by zeros
  dfrc$phir[is.na(dfrc$phir)] <- 0
  #root volume fraction: relative (sums up to 1)
  dfrc$phirrel <- dfrc$phir / sum(dr$phir)
  #transform flags into factors
  dfrc$flag <- as.factor(dfrc$flag)
  #change flag factor levels
  levels(dfrc$flag) <- c('Fraction_NotInTension',
                         'Fraction_AnchoredElastic',
                         'Fraction_AnchoredElastoplastic',
                         'Fraction_SlippingElastic',
                         'Fraction_SlippingElastoplastic')
  #convert long data format of <dfrc> into wide data
  dfrcw <- dfrc[,c('u','flag','phirrel')] %>% 
    spread(key=flag, value=phirrel) 
  #add broken category
  dfrcw$Fraction_Broken <- as.double(with(dfrcw, 1 - Fraction_NotInTension - Fraction_AnchoredElastic - Fraction_AnchoredElastoplastic - Fraction_SlippingElastic - Fraction_SlippingElastoplastic))
  #merge with dsum
  dout <- merge(dsum, dfrcw, by='u')
  #return
  return(dout)  
}
