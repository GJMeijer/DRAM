#' Compare two numeric vectors
#'
#' @description
#' This is a safe way of comparing if two vectors of floating
#' point numbers are (pairwise) equal. This is safer than
#' using `==``, because it has a built in tolerance. This function
#' is based on `dplyr::near()`.
#'
#' @param x,y Numeric vectors to compare
#' @param tol tolerance of comparison (optional)
#' @return a logical array (same size as input arrays) with
#'   element-wise comparison of `x` and `y``
#' @examples
#' sqrt(2)^2 == 2
#' is_near(sqrt(2)^2, 2)
#' @export

is_near <- function(x, y, tol = .Machine$double.eps^0.5 ){
  return(abs(x - y) < tol)
}
