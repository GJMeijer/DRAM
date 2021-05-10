#' Postprocess DRAM results to get fractions of behaviour type
#'
#' @description
#' Function that analyses the DRAM results and determines for each displacement
#' step what fraction of the root area ratio corresponds with the various types
#' of root behaviour (not in tension, anchored elastic, slipping elastoplastic
#' etc.). The results are added to the `dsum` dataframe that is inputted.
#'
#' @param dsum dataframe with DRAM output per step. Should contain fields the
#'   displacement step identifier `stepID`.
#' @param dall dataframe with DRAM outpput per step and root. Should contain
#'   fields for the displacement step identifier (`stepID`), root identifier
#'   (`rootID`), breakage parameter (`fb`) and the flag indicating the
#'   type of root behaviour (`flag`)
#' @param da dataframe with root properties and orientation. Should contain
#'   fields for the root identfier (`rootID`) and the root area ratio per
#'   root (`phir`)
#' @return dataframe `dsum` with added fields for the fraction of the root
#'   area ratio belonging to broken roots (`fraction_broken`),
#'   roots not in tension (`fraction_notintension`),
#'   anchored elastic roots (`fraction_anchoredelastic`),
#'   anchored elastoplastic roots (`fraction_anchoredelastoplastic`),
#'   slipping elastic roots (`fraction_slipelastic`) and
#'   slipping elastoplastic roots (`fraction_slipelastoplastic`)
#' @examples
#' dsum <- data.frame(
#'   stepID = c(1, 2)
#' )
#' dall <- data.frame(
#'   stepID = c(1, 1, 2, 2),
#'   rootID = c(1, 2, 1, 2),
#'   fb = c(1, 1, 0.9, 0.8),
#'   flag = c(0, 0, 1, 3)
#' )
#' da <- data.frame(
#'   rootID = c(1, 2),
#'   phir = c(0.01, 0.02)
#' )
#' postprocess_roottypes(dsum, dall, da)
#' @export

postprocess_roottypes <- function(dsum, dall, da){
  #fraction of roots
  da$frac <- da$phir / sum(da$phir)
  #get root volume fractions per root
  dall2 <- merge(
    dall[, c('rootID', 'stepID', 'fb', 'flag')],
    da[, c('rootID', 'frac')],
    by = 'rootID', all = T
  )
  #rar of intact roots
  dall2$frac_intact <- dall2$fb * dall2$frac
  #initiate output in <dsum>
  dsum$fraction_notintension <- NA
  dsum$fraction_anchoredelastic <- NA
  dsum$fraction_anchoredelastoplastic <- NA
  dsum$fraction_slipelastic <- NA
  dsum$fraction_slipelastoplastic <- NA
  #loop through all displacement steps
  for (i in 1:nrow(dsum)){
    dsum$fraction_notintension[i] <- sum(dall2$frac_intact[(dall2$stepID==dsum$stepID[i]) & (dall2$flag==0)])
    dsum$fraction_anchoredelastic[i] <- sum(dall2$frac_intact[(dall2$stepID==dsum$stepID[i]) & (dall2$flag==1)])
    dsum$fraction_anchoredelastoplastic[i] <- sum(dall2$frac_intact[(dall2$stepID==dsum$stepID[i]) & (dall2$flag==2)])
    dsum$fraction_slipelastic[i] <- sum(dall2$frac_intact[(dall2$stepID==dsum$stepID[i]) & (dall2$flag==3)])
    dsum$fraction_slipelastoplastic[i] <- sum(dall2$frac_intact[(dall2$stepID==dsum$stepID[i]) & (dall2$flag==4)])
  }
  #fraction of broken roots
  dsum$fraction_broken <- (1 - dsum$fraction_notintension - dsum$fraction_anchoredelastic -
    dsum$fraction_anchoredelastoplastic - dsum$fraction_slipelastic - dsum$fraction_slipelastoplastic)
  #return
  return(dsum)
}
