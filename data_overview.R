# Here I created a structured, comprehensive, and clean dataframe that I can use to view and analyse my data

rm(list = ls()) #start with a clean environment


# libraries
library("readxl") #for reading data
library("dplyr") #for piping
library("tidyr") #for data processing
library("countrycode") #for aggregating countries into regions
library("maps") #to get a list of all countries
library("formattable") #for making ncie tables #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
library("kableExtra") #for making nice tables #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
library("xlsx") #for saving data

# functions
source("./functions/split_and_add_by_ID.R")
source("./functions/split_and_add_by_doc.R")
source("./functions/dict_classification.R")
source("./functions/split_and_merge_by_doc.R")
source("./functions/level1_count.R")
source("./functions/reverse_dict.R")

# dictionaries
source("./dictionaries/timpl_class.R")
source("./dictionaries/comm_class.R")
source("./dictionaries/NOTA_class.R") 
source("./dictionaries/NOTA_subclass.R")#dictionary linking governance measures to NATO subclasses
source("./dictionaries/goals_class.R")
source("./dictionaries/FSi_class.R")

# Load data ###################
datadir <- "../Atlas_export_sheets"
quotes <- read_excel(paste0(datadir, "/", "all_quotes.xlsx")) # read_excel("C:/Users/MeyerMA/OneDrive - Universiteit Twente/Paper/Review paper/Import_R_case_studies.xlsx")
# Load data ###################


# Pre-process data ########################
quotes <- quotes[quotes$`Document Groups` == "!Read for Lit review - eligible",] #ignore codes attached in papers ineligible for literature review

# make quotes into long format (one quote per row, but still in the column "name")
quotes_long <- quotes[1,]
quotes_long$code <- NA
quotes_long[,] <- NA

for(i in 1:nrow(quotes)){
  codes <- quotes$Codes[i]
  codes_vec <- unlist(strsplit(codes, "\r\n"))
  for(j in codes_vec){
    new_row <- quotes[i,]
    new_row$code <- j
    quotes_long <- rbind(quotes_long, new_row)
  }
}

quotes_long <- quotes_long[-1,]

quotes_long$code_group <- ""
quotes_long$name <- ""

for(i in 1:nrow(quotes_long)){
  code <- quotes_long$code[i]
  code_vec <- unlist(strsplit(code, ": "))
  quotes_long$code_group[i] <- code_vec[1]
  quotes_long$name[i] <- code_vec[2]
}

n_studies <- as.numeric(as.character(length(unique(quotes_long$Document))))

quotes_long <- quotes_long[!is.na(quotes_long$name),]

rm(list = c("quotes", "new_row", "code", "codes_vec", "code_vec", "codes", "i", "j"))

quotes_long$name_id <- 1:nrow(quotes_long)

# Fix spatial & temporal - extent and resolution
quotes_long$code_group[which(quotes_long$code_group == "spatial & temporal - spatial extent [m2]")] <- "spatial & temporal - spatial extent"
quotes_long$code_group[which(quotes_long$code_group == "spatial & temporal - spatial resolution [m2]")] <- "spatial & temporal - spatial resolution"
quotes_long$code_group[which(quotes_long$code_group == "spatial & temporal - temporal extent [d]")] <- "spatial & temporal - temporal extent"
quotes_long$code_group[which(quotes_long$code_group == "spatial & temporal - temporal resolution [d]")] <- "spatial & temporal - temporal resolution"

# Fix countries
# identify papers that represent all countries
to_fix <- which(quotes_long$name == "all countries fix in R")

#get list of all countries
x <- map("world", plot=FALSE)
str(x)
x$names

countries <- unique(gsub("\\:.*","",x$names))
countries

# add country not in list:
countries <- c(countries, "Dominican republic")

#some of the countries extracted from the function above are not really countries (but e.g. sovereign states), and therefore removed
no_country <- c("Ascension Island", "Azores", "Barbuda", "Bonaire", "Canary Islands", "Chagos Archipelago", "Christmas Island",
                "Cocos Islands", "Falkland Islands", "Guadeloupe", "Heard Island", "Madeira Islands", "Martinique", "Mayotte",
                "Micronesia", "Reunion", "Saba", "Saint Kitts" , "Saint Vincent", "Siachen Glacier", "Sint Eustatius", "Sint Maarten",
                "South Georgia", "South Sandwich Islands", "Trinidad", "Vatican", "Virgin Islands, US")

countries <- countries[-which(countries %in% no_country)]

#add row for each country, with the rest of the information remaining the same
for(i in to_fix){
  rowi <- quotes_long[i,]
  for(j in countries){
    rowj <- rowi
    rowj$name <- j
    quotes_long <- rbind(quotes_long, rowj)
  }
}

#remove rows with "all countries fix in R" identifier
quotes_long <- quotes_long[-to_fix,]

#some countries are not recognized by the map function used later on, therefore fixed here
quotes_long$name[quotes_long$name == "Antigua"] <- "Antigua and Barbuda"
quotes_long$name[quotes_long$name == "Côte d'Ivoire"] <- "Cote d'Ivoire"
quotes_long$name[quotes_long$name == "Grenadines"] <- "Saint Vincent and the Grenadines"
quotes_long$name[quotes_long$name == "Nevis"] <- "Saint Kitts and Nevis"
quotes_long$name[quotes_long$name == "Tobago"] <- "Trinidad and Tobago"
quotes_long$name[quotes_long$name == "Eswatini"] <- "Swaziland"

# Aggregate countries into larger regions (continent and 7 and 23 regions as defined by the World Bank for Development evaluation)
for(i in which(quotes_long$code_group == "spatial & temporal - country")){
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  
  rowi_cont <- rowi; rowi_r7 <- rowi; rowi_r23 <- rowi 
  
  rowi_cont$name <- countrycode(sourcevar = dati, origin = "country.name", destination = "continent")
  rowi_r7$name <- countrycode(sourcevar = dati, origin = "country.name", destination = "region")
  rowi_r23$name <- countrycode(sourcevar = dati, origin = "country.name", destination = "region23")
  
  rowi_cont$code_group <- "spatial & temporal - continent"; rowi_r7 <- "spatial & temporal - WBD7 region"; rowi_r23 <- "spatial & temporal - WBD23 region" 
  
  quotes_long <- rbind(quotes_long, rowi_cont)
  quotes_long <- rbind(quotes_long, rowi_r7)
  quotes_long <- rbind(quotes_long, rowi_r23)
  
}

# Get type and subtype of governance measures
quotes_long$class <- ""
quotes_long <- dict_classification(sheet = quotes_long, dct = undir_gov, clm = 16, class_clm = 18)
colnames(quotes_long)[18] <- "per measure - measure undirected"
unique(quotes_long$`per measure - measure undirected`)
quotes_long$class <- ""
quotes_long <- dict_classification(sheet = quotes_long, dct = NOTA_subclass, clm = 18, class_clm = 19)
colnames(quotes_long)[19] <- "per measure - NATO subclass"
unique(quotes_long$`per measure - NATO subclass`)
quotes_long$class <- ""
quotes_long <- dict_classification(sheet = quotes_long, dct = NOTA_class, clm = 19, class_clm = 20)
colnames(quotes_long)[20] <- "per measure - NATO class"
unique(quotes_long$`per measure - NATO class`)

quotes_long <- quotes_long %>% distinct()
quotes_long$name_id <- 1:nrow(quotes_long)

quotes_wide <- quotes_long %>% pivot_wider(names_from = code_group, values_from = name)
colnames(quotes_wide)
quotes_long <- quotes_wide %>% pivot_longer(cols = c("per measure - measure undirected":"spatial & temporal - WBD23 region"),
                                            names_to = "code_group", values_to = "name")

# Get type of governance objectives
quotes_long$class <- ""
class_clm = which(colnames(quotes_long) == "class")
name_clm = which(colnames(quotes_long) == "name")
quotes_long <- dict_classification(sheet = quotes_long, dct = goals_class, clm = name_clm, class_clm = class_clm)
colnames(quotes_long)[class_clm] <- "per measure - objective class"
unique(quotes_long$`per measure - objective class`)

quotes_long <- quotes_long %>% distinct()
quotes_long$name_id <- 1:nrow(quotes_long)

quotes_wide <- quotes_long %>% pivot_wider(names_from = code_group, values_from = name)
which(colnames(quotes_wide) == "per measure - objective class")
colnames(quotes_wide)
quotes_long <- quotes_wide %>% pivot_longer(cols = c("per measure - measure undirected":"spatial & temporal - WBD23 region"),
                                            names_to = "code_group", values_to = "name")

# Get type of food security indicators
quotes_long$class <- ""
class_clm = which(colnames(quotes_long) == "class")
name_clm = which(colnames(quotes_long) == "name")
quotes_long <- dict_classification(sheet = quotes_long, dct = FSi_class, clm = name_clm, class_clm = class_clm)
colnames(quotes_long)[class_clm] <- "per effect - FS indicator class"
unique(quotes_long$`per measure - FS indicator class`)

quotes_long <- quotes_long %>% distinct()
quotes_long$name_id <- 1:nrow(quotes_long)

quotes_wide <- quotes_long %>% pivot_wider(names_from = code_group, values_from = name)
which(colnames(quotes_wide) == "per measure - FS indicator class")
colnames(quotes_wide)
quotes_long <- quotes_wide %>% pivot_longer(cols = c("per effect - FS indicator class":"spatial & temporal - WBD23 region"),
                                            names_to = "code_group", values_to = "name")

# Get type of food commodities


# Fix data (split data type and whether the type is primary or secondary)
for(i in which(quotes_long$code_group == "modelling - data")){
  dati <- quotes_long$name[i]
  rowi <- quotes_long[i,]
  dat_primi <- sub("\\).*", "", sub(".*\\(", "", dati))
  dat_spliti <- gsub("\\s*\\([^\\)]+\\)","", dati)
  
  rowi_primi <- rowi; rowi_spliti <- rowi
  rowi_primi$code_group <- "modelling - primary or secondary"; rowi_spliti$code_group <- "modelling - data split"
  rowi_primi$name <- dat_primi; rowi_spliti$name <- dat_spliti
  
  quotes_long <- rbind(quotes_long, rowi_primi)
  quotes_long <- rbind(quotes_long, rowi_spliti)
  
}

quotes_long <- quotes_long %>% distinct()
quotes_long$name_id <- 1:nrow(quotes_long)

quotes_wide <- quotes_long %>% pivot_wider(names_from = code_group, values_from = name, id_cols = name_id)

# Pre-process data ########################

colnames(quotes_wide)

leave_out <- c("ID", "Quotation Name", "Document", "Document Groups", "Quotation Content", "Comment", "Codes", "Reference", "Density", "Created by", "Modified by", "Created", 
               "Modified", "code", "name_id", "framing", "location", "definition - crowd-shipping", "decision-making", "agent characteristic", "agent decision", "decision-making method",
               "Design concept", "method", "method - model fitting", "agent ability", "data analysis method", "definition", "new word - ceteris paribus", "new word - ginning", 
               "new word - salient", "patch ability", "spatial", "new word - FEFO", "new word - promulgated", "per effect -unit", "software", "new word - cogent", "new word - precipitously",
               "new word - spurious", "papers", "patch characteristic", "title-tag", "spatial & temporal - trade partnership", "spatial & temporal - special administrative region",
               "spatial & temporal - part of earth", "spatial & temporal - county", "spatial & temporal - constituent country", "spatial & temporal - administrative region", 
               "spatial & temporal - district", "spatial & temporal - overseas department and region", "spatial & temporal - planet", "spatial & temporal - river basin", 
               "spatial & temporal - special municipality", "spatial & temporal - temporal extent unit", "theory-decision making", "theory-yield response", "spatial & temporal - state",
               "spatial & temporal - province", "spatial & temporal - part of continent", "spatial & temporal - municipality", "spatial & temporal - city", "nonfood system commodity",
               "agent", "food system - FS agent", "food system - FS type", "method-sensitivity analysis", "method-validation", "method - model testing", "method - sensitivity analysis",
               "modelling - programming language", "modelling - validation type", "papers - first author", "papers - last author", "per agent - interactions", "per effect - effect", 
               "per effect - FS dimension", "per effect - indicator other", "per effect - unit indicator other", "spatial & temporal - constituent state", "per effect - indicator other",
)

favars <- colnames(quotes_wide)[!(colnames(quotes_wide) %in% leave_out)]

write.xlsx(as.data.frame(favars), file="./variable_summary_tables/all_variables.xlsx", sheetName="all variables", row.names=FALSE)


for(i in favars){
  
  clmn_nr <- which(colnames(quotes_wide)==i)
  
  datai <- quotes_wide[!is.na(quotes_wide[,i]),]
  datai$name <- datai[,i]
  datai_sum <- level1_count(sheet = datai)
  
  tablenamei <- paste0("./variable_summary_tables/", i, ".xlsx")
  tablenamei = gsub("\\?", "", tablenamei) #remove question mark
  
  #datai_sum <- as.data.frame(datai_sum)
  write.xlsx(cbind(i = datai_sum$name, "number" = datai_sum$n), file="../variable_summary_tables/all_variables.xlsx", sheetName=gsub("\\?", "", i), row.names=FALSE, append = TRUE)
  
  #write.table(cbind(i = datai_sum$name, "number" = datai_sum$n), file = tablenamei, row.names = FALSE)
  
  
  print("---------------------------------------------------")
  print(i)
  print(datai_sum)
  print("===================================================")
  print("                                                   ")
}
