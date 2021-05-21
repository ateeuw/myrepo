make_cooc_doc <- function(sheet, codegr1, codegr2){
  dat <- sheet[sheet$code_group %in% c(codegr1, codegr2),]
  dat <- dat[!is.na(dat$code_group),]
  dat <- dat[!is.na(dat$name),]
  
  dat_sum <- dat %>% 
    group_by(Document, code_group, name) %>%
    count(Document, code_group, name)
  
  dat_sum <- dat_sum[,-which(colnames(dat_sum)=="n")]
  dat_sum <- pivot_wider(data = dat_sum, id_cols = Document, names_from = code_group, values_from = name, values_fn = list)
  
  dat_cooc <- dat_sum[1,]
  dat_cooc[,1:3] <- ""
  
  for(i in 1:nrow(dat_sum)){
    doms <- unlist(dat_sum[i,which(colnames(dat_sum)==codegr1)])
    if(is.null(doms)){next
    }else{
      typs <- unlist(dat_sum[i,colnames(dat_sum)==codegr2])
      if(is.null(typs)){next
      }else{
        for(j in 1:length(doms)){
          for(k in 1:length(typs)){
            newrow <- dat_cooc[1,]
            newrow[,colnames(newrow)==codegr1] <- doms[j]
            newrow[,colnames(newrow)==codegr2] <- typs[k]
            newrow[,colnames(newrow)=="Document"] <- dat_sum[i,1]
            dat_cooc <- rbind(dat_cooc, newrow)
          }
        }
      } 
    }
  }
  dat_cooc <- dat_cooc[-1,] 
  
  return(dat_cooc)
}



