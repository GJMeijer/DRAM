# File containing functions required to calculate root-reinforcement using existing models
# 28/05/2019 - G.J. Meijer


########################
### WU/WALDRON MODEL ###
########################

#Wu/waldron model
f_WWM <- function(dp, k=1.2) {
  # FUNCTION to calculate peak root-reinforcement using the Wu/Waldron model
  # INPUT
  # - <dp>: dataframe with all root diameters and properties. Required fields
  #           - tru:  Root tensile strengths
  #           - phir: Root volume fractions per root diameter class
  # - <k>:  Wu/Waldron multiplication constant
  # OUTPUT
  # - <cr>: scalar containing peak root-reinforcement [F/L^2]
  
  #return reinforcement
  return(with(dp, k*sum(phir * tru)))
}


##########################
### FIBRE BUNDLE MODEL ###
##########################

#fibre bundle model
f_FBM <- function(dp, fac=1, k=1.2){
  # FUNCTION to calculate root-reinforcement using the Fibre Bundle Model
  # INPUT
  # - <dp>:   dataframe with all root diameters and properties. Required fields
  #           - tru:  Root tensile strengths
  #           - dr:   Root diameters
  #           - phir: Root volume fractions per root diameter class
  # - <fac>:  Fibre bundle model load sharing factor
  #           Load is shared according to root diameter <dr> proportional to <dr^fac>
  # - <k>:    Wu/Waldron factor
  # OUTPUT
  # - <dats>: dataframe with fibre model results. Contains
  #           - dr:           Diameter of currently breaking roots
  #           - cr:           Total root-reinforcement at the point when roots with <dr> breaks
  #           - FailureOrder: Order in which roots break (lower numbers break first)
  
  #number of roots
  n   <- dim(dp)[1]
  #sorting order for failure order
  dp$FailureOrder <- with(dp, order(tru*dr^(2-fac)))
  #expand dataframe. For every root diameter, create an entry for all root diameters
  #                  This is than used to calculate the force in each root at the moment
  #                  of breakage in any root
  dat2 <- expand.grid.df(dp, dp)
  #initialise tensile stress in each root
  dat2$tr    <- 0
  #tensile force in each root at moment of breakage of each root (only calculate for roots still intact)
  # - FailureOrder.x & dr.x & phir.x: correspond to root that currently breaks
  # - FailureOrder.y & dr.y & phir.y: correspond to root that stress need calculated for (previously: 1)
  dat2$tr[dat2$FailureOrder.y>=dat2$FailureOrder.x] <- with(dat2[dat2$FailureOrder.y>=dat2$FailureOrder.x,], tru.x*dr.x^(2-fac) / dr.y^(2-fac))
  #contribution to root-reinforcement ('cohesion')
  dat2$cr    <- with(dat2, k*phir.y*tr)
  #summarize - sum reinforcement per breakage step
  dats <- dat2 %>% group_by(dr.x,FailureOrder.x) %>% summarize(cr=sum(cr)) 
  #Change column names
  colnames(dats) <- c('dr','FailureOrder','cr')
  #return dataframe with root-reinforcement at every root breakage moment
  return(as.data.frame(dats))
}


#########################
### ROOT BUNDLE MODEL ###
#########################

#root bundle models (schwarz)
f_RBMw <- function(dp, k=1.2, SuddenBreak=F) {
  # FUNCTION to calculate root-reinforcement using Root Bundle Model (Schwarz et al)
  # INPUT
  # - <dp>:   dataframe with all root diameters and properties. Required fields:
  #           - tru:    Root tensile strength
  #           - try:    Root yield strength
  #           - eru:    Root tensile strain to failure
  #           - eru:    Root yield strain
  #           - Ere:    Root elastic stiffness (Young's modulus)
  #           - Erep:   Root elasto-plastic stiffness
  #           - kappat: Root Weibull shape parameter describing distribution of root tensile strength per root class
  #           - phir:   Root volume fraction per root class
  # OPTIONAL INPUT
  # - <k>:           Wu/Waldron factor
  # - <SuddenBreak>: If True, weibull distribution function for failure is overruled by Heaviside (sudden root breakage, original RBM)
  #                  If False, weibull distribution is used (RBMw)
  # OUTPUT
  # - <dats>: dataframe with fibre model results. Contains
  #           - eps:          Tensile strain
  #           - cr:           Total root-reinforcement

  #number of displacement steps
  n <- 1001
  #strain steps
  deps <- data.frame(eps = seq(0, 1.5*max(dp$eru), l=n))
  #Expand data.frame - all strain steps for all roots
  dd <- expand.grid.df(dp, deps)
  #calculate tension in each root - elastic (no breakage)
  dd$tr <- with(dd, ifelse(eps<=(try/Ere), (Ere*eps), (try+(eps-try/Ere)*Erep)))
  #calculate breakage parameter
  if (SuddenBreak == T){
    #sudden breakage
    dd$fbreak <- with(dd, ifelse(tr>tru, 0, 1))
  } else {
    #Weibull scale parameter
    dd$lambdat <- with(dd, tru/gamma(1+1/kappat))
    #Weibull intact fraction
    dd$fbreak <- with(dd, exp(-(tr/lambdat)^kappat))
  }
  #average tensile strength
  dd$tr <- with(dd, tr*fbreak)
  #calculate reinforcement per root
  dd$cr <- with(dd, k*tr*phir)
  
  #summarise per strain step
  dats <- dd %>% group_by(eps) %>% summarize(cr=sum(cr)) 
  #return
  return(dats)
}
