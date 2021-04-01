# function summarising presence of codes within papers

level2_count <- function(sheet){
  data_sum <- sheet %>% 
    group_by(Document, name, class) %>%
    count(Document, name, class)
  
  data_sum$n <- 1
  
  data_sum <- data_sum %>% 
    group_by(class) %>%
    count(class)
  
  data_sum <- data_sum %>%
    arrange(desc(n))
  
  return(data_sum)
  
}
