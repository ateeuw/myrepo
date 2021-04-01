level2_summ <- function(level1code, level2code, dat_long){ #as strings
  dat <- dat_long[dat_long$code_group %in% c(level1code, level2code),]
  dat <- dat[!is.na(dat$code_group),]
  dat <- dat %>% spread(code_group, name)
  Acol <- which(colnames(dat) == level1code)
  Bcol <- which(colnames(dat) == level2code)
  colnames(dat)[c(Acol,Bcol)] <- c("codeA", "codeB")
  A <- dat[,which(colnames(dat) %in% c("codeA", "ID", "Document"))]
  B <- dat[,which(colnames(dat) %in% c("codeB", "ID"))]
  dat <- merge(A, B, by = "ID")
  dat <- na.omit(dat)
  dat <- dat %>% group_by(Document, codeA, codeB) %>%
    count(Document, codeA, codeB)
  dat$n <- 1
  
  dat_sum <- dat %>% 
    group_by(codeB) %>%
    count(codeB)
  
  dat_sum <- dat_sum %>%
    arrange(desc(n))
  
  dat_sum$codeB <- factor(dat_sum$codeB, levels = rev(unique(dat_sum$codeB)))
  
  return(dat_sum)
  
}
