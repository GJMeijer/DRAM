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
