#' Transform polar coordinates to different system of polar coordinates
#'
#' @description
#' function to calculate initial root angles by transforming angles in a
#' rotated coordinate system back to a coordinate system of interest.
#'
#' The orientation of the initial coordinate system is descrbined in terms
#' of the system of interest by an azimuth angle `alphaoffset` and an elevation
#' angle `betaoffset`.
#'
#' @param do a dataframe with orienations. Should contain fields for the
#'   azimuth (`alpha0`) and the elevation (`beta0`).
#' @param alphaoffset The azimuth rotation required to obtain the intial coordinate
#'   system in terms of the coordinate system to be transformed to
#' @param betaoffset The elevation rotation required to obtain the intial coordinate
#'   system in terms of the coordinate system to be transformed to
#' @return a dataframe witht the same fields as input `do`, but with updated
#'   values for the azimuth (`alpha0`) and elevation angle (`beta0`)
#' @examples
#' do <- data.frame(
#'   alpha0 = c(-1, 0, 1, 1),
#'   beta0 = c(1, 1, 1.5, 1.2),
#'   weight = c(0.25, 0.25, 0.25, 0.25)
#' )
#' polar2polar(do, alphaoffset = 0.1, betaoffset = 0.1)
#' @export

## FUNCTIONS for find orientations of roots in shear plane coordinate system
polar2polar <- function(do, alphaoffset = 0, betaoffset = 0){
  # Coordinate transformation applied:
  #   [ux]   [cos(a0)*sin(b0)]   [cos(af), -sin(af), 0]   [ cos(bf), 0, sin(bf)]   [cos(a0d)*sin(a0d)]
  #   [us] = [sin(a0)*sin(b0]] = [sin(af),  cos(af), 0] * [ 0,       1, 0      ] * [sin(a0d)*sin(a0d)]
  #   [uz]   [        cos(b0)]   [0      ,  0      , 1]   [-sin(bf), 0, cos(bf)]   [         cos(a0d)]
  # where
  # - a0d: azimuth in rotated system
  # - b0d: elevation in rotated system
  # - a0: azimuth in rotated system
  # - b0: elevation in rotated system
  # - af: rotation azimuth
  # - bf: rotation elevation

  #calculate coordinate transformation:
  ux <- with(do,
             cos(alphaoffset)*cos(alpha0)*cos(betaoffset)*sin(beta0) -
               sin(alphaoffset)*sin(alpha0)*sin(beta0) +
               cos(alphaoffset)*cos(beta0)*sin(betaoffset))
  uy <- with(do,
             cos(alphaoffset)*sin(alpha0)*sin(beta0) +
               cos(beta0)*sin(alphaoffset)*sin(betaoffset) +
               cos(alpha0)*cos(betaoffset)*sin(alphaoffset)*sin(beta0))
  uz <- with(do,
             cos(betaoffset)*cos(beta0) -
               cos(alpha0)*sin(betaoffset)*sin(beta0))
  #calculate elevation angles (in range [0 <= beta <= pi])
  do$beta0 <- acos(uz)
  #calculate azimuth angles (in range [-pi <= alphaoffset < pi])
  do$alpha0 <- atan2(uy, ux)
  #return
  return(do)
}
