#' Create initial root orientations on a 2D range
#'
#' @description
#' Determines discrete number of root orientations that approximate a
#' continuous distribution of initial root orienations in a 2D-plane.
#'
#' The azimuth is assumed zero, and the elevations uniformly distributed
#' along an elevation range spanning from `-beta0max` to `beta0max`.
#'
#' @param beta0max the maximum elevation angle describing the spherical cap
#' @param n number of discrete orientation requested
#' @return dataframe with the initial azimuth (`alpha0`), the initial
#'   elevation (`beta0`) and the relative weight that should be assigned to
#'   each orientation (`weight`).
#' @examples
#' orientations_initial_arc(pi/4, 6)
#' @export

orientations_initial_arc <- function(beta0max, n){
  #create all orientations
  do <- data.frame(
    alpha0 = 0,
    beta0 = beta0max*seq((-1+1/n), (1-1/n), l=n),
    weight = 1/n
  )
  #make sure all elevations are positive
  do$alpha0[do$beta0<0] <- -pi
  do$beta0[do$beta0<0] <- -do$beta0[do$beta0<0]
  #return
  return(do)
}
