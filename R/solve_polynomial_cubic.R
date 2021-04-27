#' Returns the largest real root of a cubic function
#'
#' @description
#' This function returns the largest real root of the cubic function:
#'   `a*x^3 + b*x^2 + c*x^2 + d == 0`
#' with the following assumptions that follow from the mobilisation of 
#' root stresses:
#'   `a,b,c >= 0` and `d <= 0`
#' because of these assumptions, there always exists a real, positive root
#'
#' @param a cubic coefficient (numeric array)
#' @param b quadratic coefficient (numeric array)
#' @param c linear coefficient (numeric array)
#' @param d constant coefficient (numeric array)
#' @return positive, real solution to the cubic equation (numeric array)
#' @examples
#' solve_polynomial_cubic(1,2,3,-4)
#' @export

# SOLVE CUBIC EQUATION - ROOT SOLVING
solve_polynomial_cubic <- function(a,b,c,d){
  #max length of array of input parameters
  len <- max(length(a), length(b), length(c), length(d))
  #construct empty output vector
  x <- numeric(len) 
  #new parameters - so that: x^3 + e*x^2 + f*x + g = 0
  #also ensures all arrays have the same length
  e <- rep_len(b/a, len)
  f <- rep_len(c/a, len)
  g <- rep_len(d/a, len)
  #temporary values
  Q <- (e^2 - 3*f)/9
  R <- (2*e^3 - 9*e*f+27*g)/54
  ind <- R^2 < Q^3    #if true, 3 real roots exist, if false, only one real root exists
  #three real roots - calculate largest value
  theta <- acos(R[ind] / sqrt(Q[ind]^3))
  x[ind] <- -2*sqrt(Q[ind])*cos((theta+2*pi)/3) - e[ind]/3
  #one real root
  A <- -sign(R[!ind])*(abs(R[!ind]) + sqrt(R[!ind]^2-Q[!ind]^3))^(1/3)
  B <- Q[!ind]/A
  x[!ind] <- (A+B) - e[!ind]/3
  #x=0 solution (when d==0)
  x[d==0] <- 0
  #return array of solutions
  return(x)
}