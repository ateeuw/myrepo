reverse_dict <- function(dict){
  rev_dict <- list()
  for(i in 1:length(dict)){
  key <- names(dict)[i]
  values <- dict[[key]]
  for(j in values){
    rev_dict[[j]] <- key
    }
  }
  return(rev_dict)
}
