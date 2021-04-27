#' Converts spherical to cartesian coordinates
#'
#' @description
#' Transform spherical coordinates, defined by an azimuth and elevation
#' angle, to 3D Cartesian coordinates on a unit circle.
#'
#' @param alpha azimuth angles (numerical array)
#' @param beta elevation angles (numerical array)
#' @param data dataframe with fields `alpha` and `beta`. This dataframe
#'   is used if alpha and beta not specifically specified. Results for
#'   cartesian coordinates are calculated and added to the dataframe
#' @return a dataframe with cartesian coordinates. Contains fields for
#'   x (`x`), y (`y`) and z (`z`) positions on the unit circle. In `data`
#'   is specified, these fields are added to the input dataframe and that
#'   dataframe is returned
#' @examples
#' polar2cartesian(alpha = c(0,1,2), beta = c(1,1,1))
#' polar2cartesian(data = data.frame(alpha = c(0,1,2), beta = c(1,1,1)))
#' @export

#function to convert polar positions to 3D cartesian (unit circle)
polar2cartesian <- function(alpha = NULL, beta = NULL, data = NULL){
  if (is.null(data)){
    return(
      data.frame(
        x = cos(alpha)*sin(beta),
        y = sin(alpha)*sin(beta),
        z = cos(beta)
      )
    )
  } else {
    data$x <- cos(data$alpha) * sin(data$beta)
    data$y <- sin(data$alpha) * sin(data$beta)
    data$z <- cos(data$beta)
    return(data)
  }
}
