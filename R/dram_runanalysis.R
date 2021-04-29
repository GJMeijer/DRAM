#' DRAM calculation function
#'
#' @description
#' Function that takes all root properties/orientations and calculates
#' root reinforcements at every displacement step
#' @param da dataframe with root properties / orientations
#' @param ds dataframe with soil properties
#' @param updateProgress shiny object to update calculation progress in
#'   shiny UI
#' @return a
#' @examples
#' da <- data.frame(rootID=1, phir=0.01, alpha0=0, beta0=0, Cr=pi, Ar=pi/4, Lr=100, try=10e6, tru=20e6, Ere=100e6, Erep=50e6, kappat=5)
#' ds <- data.frame(c=1e3, phi=0.5, sign=10e3, taui=1e3, h0=1, hmax=50, usmax=50, nstep=100)
#' dram_runanalysis(da, ds)
#' @export

dram_runanalysis <- function(da, ds, updateProgress = NULL) {
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

  ## generate all combinations of root properties and orientations

  #Create data frame for summary output
  dos <- data.frame(
    stepID = seq(0,ds$nstep),
    us = seq(0, ds$usmax, l = ds$nstep + 1),
    h = ds$h0,
    cr = 0
  )
  #Create dataframe for output per root
  dor <- expand.grid(
    rootID = da$rootID,
    stepID = dos$stepID,
    tr = 0,
    fb = 0,
    cr = 0,
    flag = 0
  )
  #initial root breakage parameter
  da$fb <- 1
  #loop through displacement steps
  for (j in 2:nrow(dos)) {
    #check if shear zone is can increase in thickness
    if (dos$h[j-1] < ds$hmax){
      #if allowed to increase - check stability of soil outwith current shear zone
      stab <- shearzone_stabilitycriterion(
        dos$h[j-1], dos$us[j],
        ds$c, ds$phi, ds$sign, ds$taui,
        da$Cr, da$Ar, da$Lr, da$phir,
        da$alpha0, da$beta0,
        da$try, da$tru,
        da$Ere, da$Erep,
        kappat = da$kappat,
        fb0 = da$fb
      )
      #increase <h> if not stable
      if (stab > 0) {
        #check if stable at the maximum value of the shear zone
        stab_max <- shearzone_stabilitycriterion(
          ds$hmax, dos$us[j],
          ds$c, ds$phi, ds$sign, ds$taui,
          da$Cr, da$Ar, da$Lr, da$phir,
          da$alpha0, da$beta0,
          da$try, da$tru,
          da$Ere, da$Erep,
          kappat = da$kappat,
          fb0 = da$fb
        )
        if (stab_max >= 0){
          #also unstable at maximum shear zone thickness -> set thickness to max thickness
          dos$h[j] <- ds$hmax
        } else {
          #solve to find shear zone thickness at which stable
          sol <- uniroot(
            shearzone_stabilitycriterion,
            lower = dos$h[j-1],
            upper = ds$hmax,
            us = dos$us[j],
            c = ds$c, phi = ds$phi, sign = ds$sign, taui = ds$taui,
            Cr = da$Cr, Ar = da$Ar, Lr = da$Lr, phir = da$phir,
            alpha0 = da$alpha0, beta0 = da$beta0,
            try = da$try, tru = da$tru,
            Ere = da$Ere, Erep = da$Erep,
            kappat = da$kappat,
            fb0 = da$fb
          )
          dos$h[j] <- sol$root
        }
      } else {
        #Shear plane edge stable, keep <h> thickness constant
        dos$h[j] <- dos$h[j-1]
      }
    } else {
      #max shear zone thickness reached, keep shear zone thickness from last step
      dos$h[j] <- dos$h[j-1]
    }
    #calculate displaced root orientations
    da$cosalpha <- calc_cosalpha(da$alpha0, da$beta0, dos$us[j], dos$h[j])
    da$sinbeta <- calc_sinbeta(da$alpha0, da$beta0, dos$us[j], dos$h[j])
    da$cosbeta <- calc_cosbeta(da$alpha0, da$beta0, dos$us[j], dos$h[j])
    #calculate tensile stresses at current <us> and <h>
    dtr <- tensilestress_unbroken(
      da$Cr, da$Ar, da$Lr,
      da$try, da$Ere, da$Erep, ds$taui,
      dos$h[j], da$cosbeta, cos(da$beta0)
    )
    #update breakage parameter
    da$fb <- calc_breakageparameter(dtr$tr, da$tru, kappat = da$kappat, fb0 = da$fb)
    #calculate root reinforcement
    dtr$cr <- calc_rootreinforcement(
      ds$phi, dtr$tr*da$fb, da$phir,
      da$cosalpha, da$sinbeta, da$cosbeta
    )
    #indices in <dor> to assign to
    ind <- (dor$stepID == dos$stepID[j])
    #calculate root tensile stresses (unbroken)
    dor[ind,c('tr','cr','flag')] <- dtr[,c('tr','cr','flag')]
    dor[ind,'fb'] <- da$fb
    #calculate total reinforcement
    dos$cr[j] <- sum(dtr$cr)
    #update progress in shiny UI
    if (is.function(updateProgress)) {
      updateProgress()
    }
  }
  #return
  return(list(sum=dos, all=dor))
}
