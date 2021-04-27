#' Returns the largest real root of a quadratic function
#'
#' @description
#' This function returns the largest root of the cubic function:
#'   `a*x^2 + b*x^2 + c == 0`
#' with the following assumptions that follow from the mobilisation of 
#' root stresses:
#'   `a < 0`
#' because of these assumptions, there always exists a real, positive root
#'
#' @param a quadratic coefficient (numeric array)
#' @param b linear coefficient (numeric array)
#' @param c constant coefficient (numeric array)
#' @return largest solution to the quadratic equation (numeric array)
#' @examples
#' solve_polynomial_quadratic(-2,6,8)
#' @export

# SOLVE QUADRATIC EQUATION - ROOT SOLVING
solve_polynomial_quadratic <- function(a,b,c){
  #max length of array of input parameters
  len <- max(length(a), length(b), length(c))
  #ensure all arrays have the same length
  a <- rep_len(a, len)
  b <- rep_len(b, len)
  c <- rep_len(c, len)
  #Discriminant
  D <- b^2 - 4*a*c
  #calculate root (get largest root)
  x <- (-b - sqrt(D)) / (2*a)
  #return array of solutions
  return(x)
}