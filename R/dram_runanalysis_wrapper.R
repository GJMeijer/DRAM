#' DRAM calculation wrapper function
#'
#' @description
#' Function that takes all input parameters and generates various outputs
#'
#' @param drmin mimimum root diameter [L]
#' @param drmax maximum root diameter [L]
#' @param phirt total root area ratio [L^2/L^2]
#' @param betaphi root area ratio power coefficient
#' @param nc number of discrete root diameter classes [-]
#' @param ndimension number of dimensions of uniform root orientation [-]
#'   should be equal to `1`, `2` or `3` for 1-D, 2-D or 3-D
#' @param norientation number of discrete root orienations requested [-]
#' @param beta0max maximum elevation of root orientations [rad]
#' @param alphaoffset azimuth offset for initial root orientations [rad]
#' @param betaoffset elevation offset for initial root orientations [rad]
#' @param tru0 tensile strength of reference root [F/L^2]
#' @param betat tensile strength power law coefficient [-]
#' @param epsru0 tensile strain to failure of reference root [L/L]
#' @param betaeps tensile strain to failure power law coefficient [-]
#' @param Lr0 root length of reference root [L]
#' @param betaL root length power law coefficient [-]
#' @param trytru ratio of tensile yield strength and ultimate strength [-]
#' @param epsryepsru ratio of tensile yield strain and ultimate strain [-]
#' @param kappat Weibull shape parameter for root survival function based
#'   on root tensile stress [-]
#' @param c soil (apparent) cohesion [F/L^2]
#' @param phi soil angle of internal friction [rad]
#' @param sign normal soil stress acting on the shear plane [F/L^2]
#' @param taui root-soil interface shear resistance [F/L^2]
#' @param h0 initial shear zone thickness [L]
#' @param hmax maximum shear zone thickness [L]
#' @param usmax maximum soil shear displacement [L]
#' @param nstep number of discrete shear displacement steps [-]
#' @param dr0 reference root diameter [L]
#' @return List with three dataframes:
#'
#'   a) `root` which contains all root properties and orientations.
#'   This contains fields:
#'   a unique identifying integer assigned to each combination
#'   of root properties (`rootID`),
#'   the minimum diameter of the root class (`drmin`),
#'   the maximum diameter of the root class (`drmax`),
#'   the average diameter in the root class (`dr`),
#'   root length (`Lr`),
#'   root tensile strength (`tru`),
#'   root yield strength (`try`),
#'   root tensile strain to failure (`epsru`),
#'   root tensile strain to yield (`epsry`),
#'   Weibull shape parameter for survival function (`kappat`),
#'   root elastic tensile stiffness (`Ere`),
#'   root elastoplastic tensile stiffness (`Erp`),
#'   azimuth angle of initial root orientation (`alpha0`),
#'   elevation angle of initial root orientation (`beta0`),
#'   root area ratio assigned to root class (`phir`),
#'
#'   b) `sum`, which contains summary data for each step.
#'   This contains fields:
#'   step identifier (`stepID`),
#'   shear displacement (`u`),
#'   shear zone thickness (`h`),
#'   root reinforcement (`cr`);
#'
#'   c) `all`, which contains summary data for each step and root.
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
#' dr0 <- 0.001  #[m]
#' drmin <- 0.002  #[m]
#' drmax <- 0.010  #[m]
#' nc <- 10  #[-]
#' phirt <- 0.002  #[m3/m3]
#' betaphi <- -1  #[-]
#' Lr0 <- 0.50  #[m]
#' betaL <- 0  #[-]
#' tru0 <- 10e6  #[Pa]
#' betat <- -0.5  #[-]
#' trytru <- 0.5  #[Pa/Pa]
#' epsru0 <- 0.2  #[m/m]
#' betaeps <- 0  #[-]
#' epsryepsru <- 0.1  #[(m/m)/(m/m)]
#' kappat <- 3  #[-]
#' ndimension <- 3  #[-]
#' norientation <- 20  #[-]
#' beta0max <- pi/4  #[rad]
#' alphaoffset <- 0  #[rad]
#' betaoffset <- 0  #[rad]
#' c <- 1e3  #[Pa]
#' phi <- 30*pi/180  #[rad]
#' sign <- 5e3  #[Pa]
#' taui <- 2e3  #[Pa]
#' h0 <- 0.002  #[m]
#' hmax <- 0.030  #[m]
#' usmax <- 0.1  #[m]
#' nstep <- 100  #[-]
#'
#' dram_runanalysis_wrapper(
#'   drmin, drmax, nc, phirt, betaphi,
#'   Lr0, betaL, tru0, betat, trytru, epsru0, betaeps, epsryepsru, kappat,
#'   ndimension, norientation, beta0max, alphaoffset, betaoffset,
#'   c, phi, sign, taui, h0, hmax,
#'   usmax, nstep, dr0 = dr0
#' )
#' @export

dram_runanalysis_wrapper <- function(
  drmin, drmax, nc, phirt, betaphi,
  Lr0, betaL, tru0, betat, trytru, epsru0, betaeps, epsryepsru, kappat,
  ndimension, norientation, beta0max, alphaoffset, betaoffset,
  c, phi, sign, taui, h0, hmax,
  usmax, nstep,
  dr0 = 1
) {
  #create root diameter classes
  dd <- discretise_rootdiameters(drmin, drmax, nc, phirt, betaphi)
  #add root properties to classes
  dr <- create_rootproperties(
    dd,
    Lr0, betaL,
    tru0, betat, trytru,
    epsru0, betaeps, epsryepsru,
    kappat,
    dr0 = dr0
  )
  #create root orientations
  do <- orientations_initial(
    ndimension, norientation,
    phirt, beta0max,
    alphaoffset = 0, betaoffset = 0
  )
  #create all combinations of roots: properties and orientations
  da <- create_allorientationsproperties(dr, do)
  #create dataframe with soil properties
  ds <- data.frame(
    c = c, phi = phi, sign = sign, taui = taui,
    h0 = h0, hmax = hmax,
    usmax = usmax, nstep = nstep
  )
  #run analysis
  dout <- dram_runanalysis(da, ds)
  #add dataframe with root orientations/properties to the output
  dout$root <- dr
  #return all results
  return(dout)
}
