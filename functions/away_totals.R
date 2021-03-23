#This function removes the rows and columns that give totals

away_totals <- function(sheet){
  
  # remove "Totals" column
  sheet <- sheet[,-which(colnames(sheet) == "Totals")]
  
  #remove "Totals" row
  sheet <- sheet[-which(sheet[,1] == "Totals"),]
}