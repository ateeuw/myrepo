level2_class_summ <- function(level1code, level2code, dat_long, classdct){ #as strings
  #
  source("./functions/dict_classification.R")
  dat_long$class <- ""
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
  
  dat$class <- ""
  Bcol  = which(colnames(dat) == "codeB")
  classcol = which(colnames(dat) == "class")
  dat <- dict_classification(sheet = dat, dct = classdct, clm = Bcol, class_clm = classcol)
  
  dat_sum <- dat %>% 
    group_by(class) %>%
    count(class)
  
  dat_sum <- dat_sum %>%
    arrange(desc(n))
  
  dat_sum$class <- factor(dat_sum$class, levels = rev(unique(dat_sum$class)))
  
  return(dat_sum)
  
}
