#This function replaces all spaces in column names with _

away_spaces <- function(sheet){
  
  #remove spaces from column names
  for(i in 1:length(colnames(sheet))){
    colnames(sheet)[i] <- gsub(" ",
                               replacement = "_",
                               x = colnames(sheet)[i])
  }
  
  return(sheet)
  
}