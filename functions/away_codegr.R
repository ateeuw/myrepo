#This function removes the provided text signifying which code group the code belongs to to the string of that code
# Example: "Spatial & temporal - representation: chessboard" will be shortened to "chessboard"

away_codegr <- function(sheet, codegr){
  
  #remove <code group> from column names
  for(i in 1:length(colnames(sheet))){
    colnames(sheet)[i] <- gsub(codegr, 
                               replacement = "", 
                               x = colnames(sheet)[i])
  }
  
  return(sheet)
}