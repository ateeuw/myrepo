# Cluster data test

rm(list = ls()) #start with a clean environment

# libraries
library(readxl) #for reading data
library(dplyr) #for piping
library(tidyr) #for data processing
library(countrycode) #for aggregating countries into regions
library(maps) #to get a list of all countries

# functions
source("./functions/split_and_add_by_ID.R")
source("./functions/split_and_add_by_doc.R")
source("./functions/dict_classification.R")
source("./functions/split_and_merge_by_doc.R")

# dictionaries
source("./dictionaries/timpl_class.R")
source("./dictionaries/comm_class.R")
source("./dictionaries/NOTA_class.R") 
source("./dictionaries/NOTA_subclass.R")#dictionary linking governance measures to NATO subclasses
source("./dictionaries/goals_class.R")

# Load data ###################
datadir <- "../Atlas_export_sheets"
quotes <- read_excel(paste0(datadir, "/", "all_quotes.xlsx")) # read_excel("C:/Users/MeyerMA/OneDrive - Universiteit Twente/Paper/Review paper/Import_R_case_studies.xlsx")
# Load data ###################

# Pre-process data ########################
quotes <- quotes[quotes$`Document Groups` == "!Read for Lit review - eligible",] #ignore codes attached in papers ineligible for literature review

#quotes <- quotes[quotes$Document %in% unique(quotes$Document)[1:10],]

# make quotes into long format
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

quotes_wide <- quotes_long %>% spread(code_group, name)

# Fix countries
# identify papers that represent all countries
to_fix <- which(quotes_long$name == "all countries fix in R")

#get list of all countries
x <- map("world", plot=FALSE)
str(x)
x$names

countries <- unique(gsub("\\:.*","",x$names))
countries

#some of the countries extracted from the function above are not really countries (but e.g. sovereign states), and therefore removed
no_country <- c("Ascension Island", "Azores", "Barbuda", "Bonaire", "Canary Islands", "Chagos Archipelago", "Christmas Island",
                "Cocos Islands", "Falkland Islands", "Guadeloupe", "Heard Island", "Madeira Islands", "Martinique", "Mayotte",
                "Micronesia", "Reunion", "Saba", "Saint Kitts" , "Saint Vincent", "Siachen Glacier", "Sint Eustatius", "Sint Maarten",
                "South Georgia", "South Sandwich Islands", "Trinidad", "Vatican", "Virgin Islands, US")

countries <- countries[-which(countries %in% no_country)]

#add row for each country, with the rest of the information remaining the same
for(i in to_fix){
  rowi <- quotes_wide[i,]
  for(j in countries){
    rowj <- rowi
    rowj$`spatial & temporal - country` <- j
    quotes_wide <- rbind(quotes_wide, rowj)
  }
}

#remove rows with "all countries fix in R" identifier
quotes_wide <- quotes_wide[-to_fix,]

#some countries are not recognized by the map function used later on, therefore fixed here
quotes_wide$`spatial & temporal - country`[quotes_wide$`spatial & temporal - country` == "Antigua"] <- "Antigua and Barbuda"
quotes_wide$`spatial & temporal - country`[quotes_wide$`spatial & temporal - country` == "Côte d'Ivoire"] <- "Cote d'Ivoire"
quotes_wide$`spatial & temporal - country`[quotes_wide$`spatial & temporal - country` == "Grenadines"] <- "Saint Vincent and the Grenadines"
quotes_wide$`spatial & temporal - country`[quotes_wide$`spatial & temporal - country` == "Nevis"] <- "Saint Kitts and Nevis"
quotes_wide$`spatial & temporal - country`[quotes_wide$`spatial & temporal - country` == "Tobago"] <- "Trinidad and Tobago"
quotes_wide$`spatial & temporal - country`[quotes_wide$`spatial & temporal - country` == "Eswatini"] <- "Swaziland"

# Fix data (split data type and whether the type is primary or secondary)
quotes_wide$`modelling - data primary or secondary` <- sub("\\).*", "", sub(".*\\(", "", quotes_wide$`modelling - data`)) 
quotes_wide$`modelling - data` <- gsub("\\s*\\([^\\)]+\\)","", quotes_wide$`modelling - data`)

# Pre-process data ########################

# modelling #############################
modelling <- quotes_wide

# modelling$`modelling - data` <- gsub("\\s*\\([^\\)]+\\)","",as.character(modelling$`modelling - data`)) #disregarding whether data are primary or secondary
modelling$`modelling - coupling?`[which(modelling$`modelling - coupling?` == "not applicable")] <- "just one"

modelling_codes <- c("modelling - aim", "modelling - feedback-loop?", "modelling - sensitivity analysis?", "modelling - validation?", "modelling - data",
                     "per model - type") #not included: subtype and subdomain

keep <- which(colnames(modelling) %in% c(modelling_codes, "name_id", "Document", "ID"))
modelling <- modelling[,keep]
empty_rows <- which(rowSums(is.na(modelling[,4:ncol(modelling)]))==length(4:ncol(modelling)))
modelling <- modelling[-empty_rows,]

# nested data
nest <- quotes_wide[,which(colnames(quotes_wide) %in% c("per model - type", "modelling - coupling?", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest[,4:ncol(nest)]))==length(4:ncol(nest)))
nest <- nest[-empty_rows,]

nest <- split_and_add_by_ID(sheet = nest, clm = 4) #coupling
nest <- nest[-which(is.na(nest$`per model - type`)),]
nest <- nest[,-which(colnames(nest) %in% c("name_id","ID"))]

nest2 <- quotes_wide[,which(colnames(quotes_wide) %in% c("per model - type", "per model - domain", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest2[,4:ncol(nest2)]))==length(4:ncol(nest2)))
nest2 <- nest2[-empty_rows,]

nest2 <- split_and_add_by_ID(sheet = nest2, clm = 4) #domain
nest2 <- nest2[-which(is.na(nest2$`per model - type`)),]
nest2 <- nest2[,-which(colnames(nest2) %in% c("name_id","ID"))]

nest3 <- quotes_wide[,which(colnames(quotes_wide) %in% c("modelling - data", "modelling - data primary or secondary", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest3[,4:ncol(nest3)]))==length(4:ncol(nest3)))
nest3 <- nest3[-empty_rows,]

nest3 <- split_and_add_by_ID(sheet = nest3, clm = 4) #data type + prim/sec
# nest3 <- nest3[-which(is.na(nest3$`modelling - data`)),]
nest3 <- nest3[,-which(colnames(nest3) %in% c("name_id","ID"))]

# level one non-nested data
for(i in c(4:9)){
  modelling <- split_and_add_by_doc(sheet = modelling, clm = i)
}

modelling <- modelling[,-which(colnames(modelling) %in% c("name_id","ID"))]
modelling <- modelling %>% distinct()

modelling <- merge(modelling, nest)
modelling <- merge(modelling, nest2)
modelling <- merge(modelling, nest3)
modelling <- modelling %>% distinct()
# modelling #############################


# agent & per agent - agent #############################
agent <- quotes_wide
agent_codes <- c("agent - representation", "per agent - agent")
keep <- which(colnames(agent) %in% c(agent_codes, "name_id", "Document"))
agent<- agent[,keep]
empty_rows <- which(rowSums(is.na(agent[,3:ncol(agent)]))==length(3:ncol(agent)))
agent <- agent[-empty_rows,]

# agent_rep <- agent[,-c(4)]
# agent_rep <- agent_rep[!is.na(agent_rep$`agent - representation`),]
# agent_rep <- agent_rep[,-(which(colnames(agent_rep)=="name_id"))]
# agent_rep <- agent_rep %>% distinct()

agent_agt <- agent[,-c(3)]
agent_agt$class <- ""
agent_agt <- dict_classification(sheet = agent_agt, dct = timpl_class, clm = 3, class_clm = 4)
agent_agt <- agent_agt[!is.na(agent_agt$class),]
agent_agt <- agent_agt[-which(agent_agt$class == ""),]
agent_agt <- agent_agt[,-(which(colnames(agent_agt) %in% c("name_id")))]
colnames(agent_agt)[which(colnames(agent_agt) == "class")] <- "per agent - agent class"
agent_agt <- agent_agt %>% distinct()

#agent <- merge(agent_rep, agent_agt)
agent <- agent_agt
agent <- agent[,-(which(colnames(agent) %in% c("per agent - agent")))]
agent <- agent %>% distinct()

# agent & per agent - agent #############################

agmod <- merge(agent, modelling)

# food system #############################
food <- quotes_wide
food_codes <- c("food system - commodity", "food system - echelon")
keep <- which(colnames(food) %in% c(food_codes, "name_id", "Document"))
food <- food[,keep]
empty_rows <- which(rowSums(is.na(food[,3:ncol(food)]))==length(3:ncol(food)))
food <- food[-empty_rows,]

food_com <- food[,-c(4)]
food_com$class <- ""
food_com <- dict_classification(sheet = food_com, dct = comm_class, clm = 3, class_clm = 4)
food_com <- food_com[!is.na(food_com$class),]
food_com <- food_com[-which(food_com$class == ""),]
food_com <- food_com[,-(which(colnames(food_com)=="name_id"))]
food_com <- food_com %>% distinct()

food_ech <- food[,-c(3)]
food_ech <- food_ech[!is.na(food_ech$`food system - echelon`),]
food_ech <- food_ech[,-(which(colnames(food_ech)=="name_id"))]
food_ech <- food_ech %>% distinct()

food <- merge(food_com, food_ech)
food <- food[,-(which(colnames(food) %in% c("food system - commodity")))]
food <- food %>% distinct()

colnames(food)[which(colnames(food) == "class")] <- "food system - commodity class"
# food system #############################

foagmod <- merge(agmod, food)

# spatial & temporal #############################
spat <- quotes_wide
spat_codes <- c("spatial & temporal - representation split", "spatial & temporal - representation features", "spatial & temporal - country", "spatial & temporal - ref scale")
keep <- which(colnames(spat) %in% c(spat_codes, "name_id", "Document"))
spat <- spat[,keep]
spat$continent <- countrycode(sourcevar = spat$`spatial & temporal - country`, origin = "country.name", destination = "continent")
spat$WBregion7 <- countrycode(sourcevar = spat$`spatial & temporal - country`, origin = "country.name", destination = "region")
spat$WBregion23 <- countrycode(sourcevar = spat$`spatial & temporal - country`, origin = "country.name", destination = "region23")
empty_rows <- which(rowSums(is.na(spat[,3:ncol(spat)]))==length(3:ncol(spat)))
spat <- spat[-empty_rows,]

# level one non-nested data
spat <- split_and_merge_by_doc(clms = 3:7, sheet = spat)

# spatial & temporal #############################

spfoagmod <- merge(foagmod, spat)

# governance #############################
gov <- quotes_wide
gov_codes <- c("governance - combined measures?", "per measure - measure", "per measure - type 2", "per measure - objective", "per measure - formulation", "per measure - scale", "per measure - target implementer", "per measure - spatially targeted?")
keep <- which(colnames(gov) %in% c(gov_codes, "name_id", "Document", "ID"))
gov <- gov[,keep]
empty_rows <- which(rowSums(is.na(gov[,4:ncol(gov)]))==length(4:ncol(gov)))
gov <- gov[-empty_rows,]

# nested data
gov$class <- ""
gov <- dict_classification(sheet = gov, dct = undir_gov, clm = 6, class_clm = 12)
colnames(gov)[12] <- "per measure - measure undirected"
gov$class <- ""
gov <- dict_classification(sheet = gov, dct = NOTA_subclass, clm = 12, class_clm = 13)
colnames(gov)[13] <- "per measure - NATO subclass"

nest <- gov[,which(colnames(gov) %in% c("governance - combined measures?", "per measure - NATO subclass", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest[,4:ncol(nest)]))==length(4:ncol(nest)))
if(sum(empty_rows) != 0){
  nest <- nest[-empty_rows,]
}

nest <- split_and_add_by_ID(sheet = nest, clm = 4) #combined measures <-- there is a mistake here
nest <- nest[-which(nest$`per measure - NATO subclass` == ""),]
nest <- nest[,-which(colnames(nest) %in% c("name_id","ID"))]

nest2 <- gov[,which(colnames(gov) %in% c("per measure - type 2", "per measure - NATO subclass", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest2[,4:ncol(nest)]))==length(4:ncol(nest)))
if(sum(empty_rows) != 0){
  nest2 <- nest2[-empty_rows,]
}

nest2 <- split_and_add_by_ID(sheet = nest2, clm = 4) #type 2
nest2 <- nest2[-which(nest2$`per measure - NATO subclass` == ""),]
nest2 <- nest2[,-which(colnames(nest2) %in% c("name_id","ID"))]

nest3 <- gov[,which(colnames(gov) %in% c("per measure - objective", "per measure - NATO subclass", "name_id", "Document", "ID"))]
nest3$class <- ""
nest3 <- dict_classification(sheet = nest3, dct = goals_class, clm = 4, class_clm = 6)
colnames(nest3)[6] <- "per measure - goal class"
empty_rows <- which(rowSums(is.na(nest3[,4:ncol(nest3)]))==length(4:ncol(nest3)))
if(sum(empty_rows) != 0){
  nest3 <- nest3[-empty_rows,]
}

nest3 <- split_and_add_by_ID(sheet = nest3, clm = 6) #objective class
nest3 <- nest3[-which(nest3$`per measure - NATO subclass` == ""),]
nest3 <- nest3[-which(nest3$`per measure - goal class` == ""),]
nest3 <- nest3[,-which(colnames(nest3) %in% c("name_id","ID", "per measure - objective"))]

nest <- merge(nest, nest2)
nest <- nest %>% distinct()
nest <- merge(nest, nest3)
nest <- nest %>% distinct()

nest2 <- gov[,which(colnames(gov) %in% c("per measure - formulation", "per measure - NATO subclass", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest2[,4:ncol(nest)]))==length(4:ncol(nest)))
if(sum(empty_rows) != 0){
  nest2 <- nest2[-empty_rows,]
}

nest2 <- split_and_add_by_ID(sheet = nest2, clm = 4) #formulation
nest2 <- nest2[-which(nest2$`per measure - NATO subclass` == ""),]
nest2 <- nest2[,-which(colnames(nest2) %in% c("name_id","ID"))]

nest <- merge(nest, nest2)
nest <- nest %>% distinct()

nest2 <- gov[,which(colnames(gov) %in% c("per measure - spatially targeted?", "per measure - NATO subclass", "name_id", "Document", "ID"))]
empty_rows <- which(rowSums(is.na(nest2[,4:ncol(nest2)]))==length(4:ncol(nest2)))
if(sum(empty_rows) != 0){
  nest2 <- nest2[-empty_rows,]
}

nest2 <- split_and_add_by_ID(sheet = nest2, clm = 4) #formulation
nest2 <- nest2[-which(nest2$`per measure - NATO subclass` == ""),]
nest2 <- nest2[,-which(colnames(nest2) %in% c("name_id","ID"))]

nest <- merge(nest, nest2)
nest <- nest %>% distinct()

gov <- nest
# governance #############################

govspfoagmod <- merge(spfoagmod, gov)
