#' Tensile strain in roots based on stress
#'
#' @description
#' Function to calculate the tensile strain in a root with a bilinear
#' stress-strain behaviour (elasto-plastic). 
#' 
#' @param tr root tensile stress (array)
#' @param try root yield stress (array or scalar)
#' @param Ere root elastic stiffness (array or scalar)
#' @param Erep root elasto-plastic stiffness (array or scalar)
#' @return tensile strain (array)
#' @examples
#' tensilestrain_unbroken(c(1,2,3,4), 2.5, 20, 10)
#' @export

tensilestrain_unbroken <- function(tr, try, Ere, Erep){
  #assume linear elastic
  epsr <- tr/Ere
  #indices in <tr> that go elasto-plastic
  ind <- (tr>try)
  #alter for elasto-plastic roots
  epsr[ind] <- (try/Ere+(tr-try)/Erep)[ind]
  #return
  return(epsr)
}
