# function to split double codes within one quotation

split_and_add <- function(sheet, clm, clm_range){
  
  code_name <- colnames(sheet)[clm]
  
  for(a in unique(sheet$Document)){
    adata <- sheet[sheet$Document==a,]
    
    for(i in unique(adata$ID)){
      idata <- adata[adata$ID==i,]
      mtype <- unique(idata[,clm])[!is.na(unique(idata[,clm]))]
      
      if(length(mtype)==0){
        print(paste0("warning: in this quotation, no '", code_name, "'code has been attached"))
        print(paste("Document = ", a))
        print(paste("Quotation = ", i))
        print("")
      }else if(length(mtype)==1){
        sheet[sheet$ID == i,clm] <- mtype
      }else{
        sheet[sheet$ID == i,clm] <- mtype[1]
        for(m in 2:length(mtype)){
          to_add <- sheet[sheet$ID == i,]
          to_add[,clm] <- mtype[m]
          sheet <- rbind(sheet, to_add)
        }
      }
    }
    
  }
  

  
}



spatial <- quotes_wide
spatial_codes <- c("spatial & temporal - country", "spatial & temporal - ref scale", "spatial & temporal - representation", 
                   "spatial & temporal - spatial extent", "spatial & temporal - spatial resolution", "spatial & temporal - temporal extent",
                   "spatial & temporal - temporal resolution")
keep <- which(colnames(spatial) %in% c(spatial_codes, "ID", "name_id", "Document"))
spatial <- spatial[,keep]
empty_rows <- which(rowSums(is.na(spatial[,4:ncol(spatial)]))==length(4:ncol(spatial)))
spatial <- spatial[-empty_rows,]

test <- split_and_add(sheet = spatial, clm = 4)
