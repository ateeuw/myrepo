# overview of papers

rm(list = ls())

###################### load libraries ###################### 
library("readxl") #for reading excel files
###################### load libraries ###################### 

dataloc <- "../papers list both.xlsx"
dataloc2 <- "../20210422_rayyan_export.xlsx"

###################### load data ###################### 
papers <- read_excel(dataloc, sheet = "-duplicates")
rayyan <- read_excel(dataloc2, sheet = "articles")
###################### load data ###################### 

too_old <- which(papers$Year<2000)
youngpapers <- papers[-too_old,]
too_young <- which(papers$Year>2020)

dup <- grepl("duplicate", rayyan$notes)
rayyannodup <- rayyan[!dup,]

empty <- is.na(rayyannodup$title)
rayyannona <- rayyannodup[!empty,]
