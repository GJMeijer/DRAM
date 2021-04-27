#' Create initial root orientations on a 2D range
#'
#' @description
#' Determines discrete number of root orientations that approximate a
#' continuous distribution of initial root orienations in a 2D-plane.
#'
#' The azimuth is assumed zero, and the elevations uniformly distributed
#' along an elevation range spanning from `-betamax` to `betamax`.
#'
#' @param betamax the maximum elevation angle describing the spherical cap
#' @param n number of discrete orientation requested
#' @return dataframe with the initial azimuth (`alpha0`), the initial
#'   elevation (`beta0`) and the relative weight that should be assigned to
#'   each orientation (`weight`).
#' @examples
#' orientations_initial_arc(pi/4, 6)
#' @export

orientations_initial_arc <- function(betamax, n){
  return(
    data.frame(
      alpha = 0,
      beta = betamax*seq((-1+1/n), (1-1/n), l=n),
      weight = 1/n
    )
  )
}
