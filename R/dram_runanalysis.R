#' DRAM calculation function
#'
#' @description
#' Function that takes all root properties/orientations and calculates
#' root reinforcements at every displacement step
#' @param da dataframe with root properties and orientations. This should
#'   contain fields for:
#'   the total root area ratio (`phir`),
#'   initial orientation azimuth (`alpha0`) [in radians],
#'   initial orientation elevation (`beta0`) [in radians],
#'   root circumference (`Cr`),
#'   root cross-sectional area (`Ar`),
#'   root length (`Lr`),
#'   root yield strength (`try`),
#'   root tensile strength (`tru`),
#'   root elastic stiffness (`Ere`),
#'   root elasto-plastic stiffness (`Erp`),
#'   Weibull breakage parameter (`kappa`)
#' @param ds dataframe with soil properties. This should contain
#'   fields for:
#'   soil cohesion (`c`),
#'   soil angle of internal friction (`phi`) [rad],
#'   normal effective soil stress on shear plane (`sign`),
#'   root-soil interface friction (`taui`),
#'   initial shear zone thickness (`h0`),
#'   maximum shear zone thickness (`hmax`),
#'   maximum shear displacement (`usmax`),
#'   number of steps  (`nstep`)
#' @param updateProgress shiny object to update calculation progress in
#'   shiny UI
#' @return List with two dataframes:
#'
#'   a) `sum`, which contains summary data for each step.
#'   This contains fields:
#'   step identifier (`stepID`),
#'   shear displacement (`u`),
#'   shear zone thickness (`h`),
#'   root reinforcement (`cr`);
#'
#'   b) `all`, which contains summary data for each step and root.
#'   This contains fields for:
#'   step identifier (`stepID`),
#'   root identified (`rootID`),
#'   root tensile strain assuming no root breakage (`tr`),
#'   breakage parameter/current fraction of roots intact (`fb`),
#'   root reinforcement per root (`cr`),
#'   a flag indicating the type of root behaviour (`flag`):
#'   `flag=0` = root not in tension,
#'   `flag=1` = anchored elastic behaviour,
#'   `flag=2` = anchored elastoplastic behaviour
#'   `flag=3` = slipping elastic behaviour,root not in tension,
#'   `flag=4` = slipping elastoplastic behaviour.
#'
#' @examples
#' da <- data.frame(
#'   rootID = 1, phir = 0.01, alpha0 = 0, beta0 = 0, Cr=pi, Ar = pi/4,
#'   Lr = 100, try = 10e6, tru = 20e6, Ere = 100e6, Erp = 50e6,
#'   kappat = 5
#' )
#' ds <- data.frame(
#'   c = 1e3, phi = 0.5, sign = 10e3, taui = 1e3,
#'   h0 = 1, hmax = 50, usmax = 50, nstep = 100
#' )
#' dram_runanalysis(da, ds)
#' @export

dram_runanalysis <- function(da, ds, updateProgress = NULL) {
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
    fb = 1,
    cr = 0,
    flag = 0,
    KEEP.OUT.ATTRS = FALSE
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
        da$Ere, da$Erp,
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
          da$Ere, da$Erp,
          kappat = da$kappat,
          fb0 = da$fb
        )
        if (stab_max >= 0){
          #also unstable at maximum shear zone thickness -> set thickness to max thickness
          dos$h[j] <- ds$hmax
        } else {
          #solve to find shear zone thickness at which stable
          sol <- stats::uniroot(
            shearzone_stabilitycriterion,
            lower = dos$h[j-1],
            upper = ds$hmax,
            us = dos$us[j],
            c = ds$c, phi = ds$phi, sign = ds$sign, taui = ds$taui,
            Cr = da$Cr, Ar = da$Ar, Lr = da$Lr, phir = da$phir,
            alpha0 = da$alpha0, beta0 = da$beta0,
            try = da$try, tru = da$tru,
            Ere = da$Ere, Erp = da$Erp,
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
      da$try, da$Ere, da$Erp, ds$taui,
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
