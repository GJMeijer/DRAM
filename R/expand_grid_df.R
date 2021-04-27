#' Expand.grid for dataframes
#' 
#' @description
#' Equivalent of base-R `expand.grid` for dataframes. Each combination of 
#' rows in the dataframes is outputted
#' 
#' @param ... dataframes 
#' @return dataframe all combinations of rows, merged
#' @examples
#' d1 <- data.frame(a = c(1,2,3), b = c(2,3,4))
#' d2 <- data.frame(c = c(5,6), d = c(7,8))
#' expand_grid_df(d1, d2)
#' @export

expand_grid_df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
