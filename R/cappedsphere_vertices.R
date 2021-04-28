#' Get coordinates and vertices of a capped sphere
#'
#' @description
#' Function to generate dataframes containing the coordinates on a unit
#' sphere and the vertices required to draw a unit semi-sphere
#'
#' @param alpha0 array with upper and lower azimuth limits
#' @param beta0 array with upper and lower elevation limits
#' @param nalpha number of points to discretise alpha
#' @param nbeta number of points to discretise beta
#' @param alphaoffset azimuth offset of the apex of the capped sphere
#' @param betaoffset elevation offset of the apex of the capped sphere
#' @return list with two dataframes, one for coordinates (fields
#'   `x`, `y` and `z`), and one with vertices (fields `i`, `j`,
#'   `k`, referring to the row of associated coordiantes)
#' @examples
#' cappedsphere_vertices()
#' cappedsphere_vertices(beta0 = c(0, pi/8))
#' @export

#function to generate a triangular mesh for a (partial) unit sphere
cappedsphere_vertices <- function(alpha0 = c(-pi, pi), beta0 = c(0, pi), nalpha = 24, nbeta = 12, alphaoffset = 0, betaoffset = 0){
  ## CREATE LOCATIONS OF NODES
  #create sphere locations (first index changes fastest)
  ds <- expand.grid(
    alpha0 = seq(alpha0[1], alpha0[2], l=nalpha),
    beta0 = seq(beta0[1], beta0[2], l=nbeta),
    KEEP.OUT.ATTRS = FALSE
  )
  #convert sphere with angle offsets
  if (!((alphaoffset==0) & (betaoffset==0))){
    ds <- polar2polar(ds, alphaoffset = alphaoffset, betaoffset = betaoffset)
  }
  #get coordinates in cartesian form
  dp <- polar2cartesian(ds$alpha0, ds$beta0)
  ## CREATE VERTICES INDICES (python indexing, e.g. '0' is index of first point)
  #create vertices for first set of triangles
  dv1 <- data.frame(
    i = rep(0:(nalpha-1),(nbeta-1)) + nalpha*rep(0:(nbeta-2),each=nalpha),
    j = rep(c(1:(nalpha-1),0),(nbeta-1)) + nalpha*rep(0:(nbeta-2),each=nalpha)
  )
  dv1$k <- dv1$j + nalpha
  #create vertices for second set of triangles
  dv2 <- data.frame(
    i = rep(0:(nalpha-1),(nbeta-1)) + nalpha*rep(1:(nbeta-1),each=nalpha),
    j = rep(c(1:(nalpha-1),0),(nbeta-1)) + nalpha*rep(1:(nbeta-1),each=nalpha)
  )
  dv2$k <- dv2$i - nalpha
  #merge
  dv <- rbind(dv1,dv2)
  ## RETURN
  return(list(coordinates = dp, vertices = dv))
}
