#' Generate dataframe with all (summary) output parameters, values and units
#'
#' @description
#' Function that takes the (summary) output from a DRAM analysis and creates
#' a dataframe
#'
#' @param dsum dataframe with output (in SI units)
#' @param du dataframe with unit conventions
#' @return dataframe with all properties
#' @export

datasave_outputparameters <- function(dsum, du = NULL){
  #convert all units back to user-defined units
  if (!is.null(du)){
    #find fields that can be converted
    coltoconvert <- colnames(dsum)[colnames(dsum)%in%rownames(du)]
    #convert
    dsum[,coltoconvert] <- mapply(`/`, dsum[,coltoconvert], du[coltoconvert,'unit_factor'], SIMPLIFY = FALSE)
    #add SI units to column names
    colnew <- paste(coltoconvert, ' [', du[coltoconvert,'unit_user'], ']', sep='')
    #assign new column names
    colnames(dsum)[colnames(dsum)%in%rownames(du)] <- colnew
    #return
    return(dsum)
  } else {
    return(dsum)
  }
}
