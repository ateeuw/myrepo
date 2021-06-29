split_and_merge_by_doc <- function(clms, sheet){
  mergedat <- sheet[,1] %>% distinct()
  for(i in clms){
    clmname <- colnames(sheet)[i]
    print(clmname)
    splitdat <- sheet[,c(1,i)]
    splitdat <- splitdat[!is.na(splitdat[,2]),]
    splitdat <- splitdat %>% distinct()
    mergedat <- merge(mergedat, splitdat)
  }
  
  return(mergedat)
  
}
