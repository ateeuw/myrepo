#This function removes the provided text signifying how grounded a code is

away_gr <- function(sheet){
  
  #remove Gr=<number> from column names
  for(i in 1:length(colnames(sheet))){
    colnames(sheet)[i] <- gsub("Gr=[0-9]+", 
                               replacement = "", 
                               x = colnames(sheet)[i])
  }
  
  #remove Gr=<number> from first column
  for(i in 1:length(rownames(sheet))){
    sheet[i,1] <- gsub("Gr=[0-9]+", 
                       replacement = "", 
                       x = sheet[i,1])
  }
  
  return(sheet)
}