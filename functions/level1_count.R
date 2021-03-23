# function summarising presence of codes within papers

level1_count <- function(sheet, code_gr){
  data_sum <- sheet %>% 
    group_by(name, Document) %>%
    count(name)
  
  data_sum$n <- 1
  
  data_sum <- data_sum %>% 
    group_by(name) %>%
    count(name)
  
  data_sum <- data_sum %>%
    arrange(desc(n))
  
  return(data_sum)
  
}