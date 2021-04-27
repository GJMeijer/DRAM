#' Create discrete initial root orientations
#'
#' @description
#' Wrapper function to create a series of discrete, initial root orientations.
#' This function assumed a uniform distribution of roots in 1-D, (single
#' point), 2-D (orientations on the same arc) or 3-D (orientations on a
#' spherical grid).
#'
#' First, the range of orientations is discretised into a number of discrete
#' orientations. Each orientation is assigned a weight, indicating the
#' fraction of all roots that is represented by that orientation.
#'
#' Secondly, the root area ratio is calculated for each orientation, since
#' roots that are less perpendicular to the shear plane contribute less
#' towards the root area ratio.
#'
#' @param ndimension The number of dimensions to consider. `1` for 1D root
#'   orientations (single orientation), `2` for 2-D orienations (orientation
#'   all located on the same arc) or `3` for 3-D orientations (orientation
#'   distributed on a spherical cap).
#' @param norientation The number of requested discrete root orienations.
#'   if `ndimension==3` (3-D root orientation), the number of returned
#'   orientations may be larger due to the discretisation method chosen.
#'   See documentation for `initial_orientations_sphericalcap` for more
#'   details
#' @param phirt total root area ratio on the shear plane (if `input_volume`)
#'   is set to `TRUE`, this input is interpreted as the root volume ratio
#'   instead. Input in terms of a fraction
#' @param betamax the maximum (limiting) elevation angle for all root
#'   orientations
#' @param alphaoffset azimuth of the (rotated) coordinate system used for initial
#'   definition of orientations.
#' @param betaoffset elevation of the (rotated) coordinate system used for initial
#'   definition of orientations.
#' @param input_volume if `TRUE`, the total root input `phirt` is interpreted
#'   as the total root volume ratio in the soil volume.
#'   If `FALSE` (default), the input is assumed to be the root area ratio
#'   on the shear plane.
#' @return dataframe with the initial azimuth (`alpha`), the initial
#'   elevation (`beta`), the relative 'weight' of each orientation, i.e.
#'   the fraction of root volume associated with each orientation
#'   (`weight`), and the root area ratio assigned to each orientation
#'   (`phir`)
#' @examples
#' initial_orientations(1, 10, 0.01, pi/4)
#' initial_orientations(2, 10, 0.01, pi/4)
#' initial_orientations(3, 10, 0.01, pi/4)
#' initial_orientations(3, 10, 0.01, pi/4, input_volume = TRUE)
#' initial_orientations(3, 10, 0.01, pi/4, alphaoffset = 0.1, betaoffset = 0.1)
#' @export

#generate root orientations
initial_orientations <- function(ndimension, norientation, phirt, betamax, alphaoffset = 0, betaoffset = 0, input_volume = FALSE){
  #do different things depending on dimension
  if ((ndimension==1) | (betamax==0) | (norientation==1)){
    #1-D --> single orientation
    do <- data.frame(
      alpha = 0,
      betaoffset = 0,
      weight = 1
    )
  } else if (ndimension==2) {
    #2-D --> arc
    do <- initial_orientations_arc(betamax, norientation)
  } else if (ndimension==3){
    #3-D --> spherical cap
    do <- initial_orientations_sphericalcap(betamax, norientation)
  } else {
    #error message
    stop('INPUT: unknown number of distribution dimensions given')
  }
  #offset coordinate system (if required)
  if (!((alphaoffset==0) & (betaoffset==0))) {
    do <- transform_orientations(do, alphaoffset=alphaoffset, betaoffset=betaoffset)
  }
  #calculate root area ratio per orientation
  if (input_volume == FALSE){
    #input is total root area ratio
    do$phir <- phirt * do$weight*cos(do$beta) / sum(do$weight*cos(do$beta))
  } else {
    #input is total root volume ratio
    do$phir <- phirt * do$weight*cos(do$beta)
  }
  #return
  return(do)
}
