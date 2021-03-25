#This function goes through a given dictionary and add the class a code belongs to in a new column

dict_classification <- function(sheet, dct, clm, class_clm){
  sheet[, class_clm] <- as.character(sheet[, class_clm])
  #sheet[i, n_clm] <- as.numeric(sheet[i, n_clm])
  for(i in 1:nrow(sheet)){
    print(i)
    for(j in names(dct)){
      print(j)
      if((sheet[i,clm] %in% dct[[j]])){
        sheet[i, class_clm] <- j
        #sheet[i, n_clm] <- length(dct[[j]])
      }
    }
  }
  return(sheet)
}
