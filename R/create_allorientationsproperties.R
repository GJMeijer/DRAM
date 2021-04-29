#' Create dataframe with all root properties and orientations
#'
#' @description
#' Function that creates dataframe with all root properties and orientations,
#' combining input already defined input for properties and orientations
#'
#' @param dr dataframe with root properties
#' @param do dataframe with orientation properties
#' @return dataframe with all combinations of root properties and orientations
#'   required in DRAM
#' @examples
#' dr <- data.frame(
#'   phir = c(1, 2, 1),
#'   tru = c(2, 3, 4),
#'   dr = c(1, 2, 3)
#' )
#' do <- data.frame(
#'   alpha0 = c(0, 0, 1),
#'   beta0 = c(0, 1, 1),
#'   phir = c(1, 1.5, 1.5)
#' )
#' create_allorientationsproperties(dr, do)
#' @export

create_allorientationsproperties <- function(dr, do){
  #create all combinations
  da <- expand_grid_df(dr, do)
  #total root area ratio
  phirt <- sum(dr$phir)
  #create field for root area ratio (in bath <dr> and <do>, the total root area
  #ratio sums to <phirt>, so when combining the result needs to be corrected)
  da$phir <- da$phir.x * da$phir.y / phirt
  #column names for output
  colnames_out <- colnames(da)[!colnames(da)%in%c('phir.x','phir.y')]
  #return
  return(da[,colnames_out])
}
