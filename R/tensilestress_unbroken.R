#' Current tensile stress in a unbroken root
#'
#' @description
#' Function calculates the tensile stress in a root where is crosses the shear
#' zone in the soil. It assumes that the root is still unbroken. This function
#' will detect whether the bit of root in the shear zone behaves elastic or
#' elasto-plastic, and whether the root slips or remains anchored.
#' The root is assumed to be straight, unbranched, have two freely supported
#' ends and has the middle of the root located in the shear zone.
#' 
#' @param Cr root circumference (array)
#' @param Ar root cross-sectional area (array)
#' @param Lr root length (array)
#' @paray try root yield strength (array)
#' @param Ere root elastic stiffness (array)
#' @param Erep root elasto-plastic stiffness (array)
#' @param taui root-soil interface shear stress (scalar)
#' @param h thickness of the shear plane (scalar)
#' @param cosbeta cosine of the elevation angle of the current orientation
#'   of the root within the shear zone (array)
#' @param cosbeta0 cosine of the elevation angle of the initial orientation
#'   of the root within the shear zone (array)
#' @return data.frame with two fields. `tr` = tensile stress in middle of 
#'   root under the stated assumptions, `flag` = the type of root behaviour
#'   (0 = not in tension; 1 = anchored, elastic; 2 = anchored, elasto-plastic;
#'   3 = slipping, elastic; 4 = slipping, elasto-plastic 
#' @examples
#' tensilestress_unbroken(pi, pi/4, 200, 2, 100, 80, 0.002, 30, 0.8, 0.9)
#' @export

tensilestress_unbroken <- function(Cr, Ar, Lr, try, Ere, Erep, taui, h, cosbeta, cosbeta0){
  #maximum vector length (in case not all vectors
  n <- max(c(length(Cr), length(Ar), length(Lr), length(Ere), length(Erep), length(cosbeta), length(cosbeta0)))
  #tensile stress - initiate vector and fill with zeros (only for roots in tension)
  tr_unbroken <- numeric(n)
  #solution flag - initiate vector and fill with zeros (only for roots in tension)
  flag <- numeric(n)
  #switch, is root in tension and not completely in shear zone? (1=yes, 0=no)
  s1 <- (cosbeta < cosbeta0) & ((Lr*cosbeta) > h)  
  #only do calculations when at least some roots are in tension
  if (any(s1)) {
    #1: ANCHORED SOLUTION
    #calculate tensile stress - linear elastic
    tra    <- tensilestress_anchored_elastic(Cr[s1],Ar[s1], Ere[s1], taui, h,cosbeta[s1],cosbeta0[s1])
    flag_a <- rep(1, length(tra))
    #check if tensile stress exceeds yield stress
    s2 <- (tra >= try[s1])
    #calculate tensile stress - bi-linear elastic
    if (any(s2)) {
      tra[s2]    <- tensilestress_anchored_elastoplastic(Cr[s1][s2],Ar[s1][s2], try[s1][s2],Ere[s1][s2],Erep[s1][s2], taui, h,cosbeta[s1][s2],cosbeta0[s1][s2])
      flag_a[s2] <- 2
    }
    
    #2: LINEAR ELASTIC, SLIPPING SOLUTION
    #calculate tensile stress - linear elastic
    trs    <- tensilestress_slip_elastic(Cr[s1],Ar[s1],Lr[s1], Ere[s1], taui, h,cosbeta[s1])
    flag_s <- rep(3, length(trs))
    #check if tensile stress exceeds yield stress
    s2 <- (trs >= try[s1])
    #calculate tensile stress - bi-linear elastic
    if (any(s2)) {
      trs[s2]    <- tensilestress_slip_elastoplastic(Cr[s1][s2],Ar[s1][s2],Lr[s1][s2], try[s1][s2],Ere[s1][s2],Erep[s1][s2], taui, h,cosbeta[s1][s2])
      flag_s[s2] <- 4
    }
    
    #3: TAKE MINIMUM
    tr_unbroken[s1]   <- pmin(tra, trs)
    flag[s1]          <- flag_a
    flag[s1][trs<tra] <- flag_s[trs<tra]
  }
  #return
  return(
    data.frame(
      tr_unbroken = tr_unbroken, 
      flag = flag
    )
  )
}