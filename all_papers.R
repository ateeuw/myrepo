# overview of papers

rm(list = ls())

###################### load libraries ###################### 
library("readxl") #for reading excel files
library("stringr")
library("VennDiagram")
###################### load libraries ###################### 

dataloc <- "../papers list both.xlsx"
dataloc2 <- "../20210422_rayyan_export.xlsx"

###################### load data ###################### 
papers <- read_excel(dataloc, sheet = "-duplicates")
bpapers <- read_excel(dataloc, sheet = "all")
rayyan <- read_excel(dataloc2, sheet = "articles")
###################### load data ###################### 

scopus <- bpapers[!is.na(bpapers$Scopus),]
wos <- bpapers[!is.na(bpapers$`Web of Science`),]

dup <- nrow(bpapers) - nrow(papers)
dup

too_old <- which(papers$Year<2000)
youngpapers <- papers[-too_old,]
too_young <- which(papers$Year>2020)

dup <- grepl("duplicate", rayyan$notes)
rayyannodup <- rayyan[!dup,]

empty <- is.na(rayyannodup$title)
rayyannona <- rayyannodup[!empty,]

###################### exclusion reasons ######################  
# ineligible <- papers[!is.na(papers$Reason),]
# ineligible <- papers[papers$Reason != "NA",]
# ineligible <- papers[papers$Reason != "not sure",]

#ineligible <- papers[papers$Eligible_abstract == 0,]


ineligible <- papers[papers$Eligible_abstract == 1,]

ineligible$Reason <- gsub("pathways", "pathway", ineligible$Reason)
ineligible$Reason <- gsub("in Chinese", "in chinese", ineligible$Reason)
ineligible$Reason <- gsub("policy not aimed at FS", "not aimed at FS", ineligible$Reason)
ineligible$Reason <- gsub("Policy not aimed at FS", "not aimed at FS", ineligible$Reason)
ineligible$Reason <- gsub("not aimed at FS", "policy not aimed at FS", ineligible$Reason)
ineligible$Reason <- gsub("RCPs", "representative concentration pathway", ineligible$Reason)
ineligible$Reason <- gsub("LCA", "life cycle assessment", ineligible$Reason)
ineligible$Reason <- gsub("FS", "food security", ineligible$Reason)
ineligible$Reason <- gsub("conferece", "conference", ineligible$Reason)
ineligible$Reason <- gsub("no policy inclusion", "no policy", ineligible$Reason)
ineligible$Reason <- gsub("policy", "governance measure", ineligible$Reason)
ineligible$Reason <- gsub("governance measure goals not measures", "governance goal not measure", ineligible$Reason)

all_reasons <- c()

for(i in 1:nrow(ineligible)){
  reasons <- strsplit(ineligible$Reason[i], ";")[[1]]
  for(j in 1:length(reasons)){
    if(reasons[j] %in% all_reasons){
      next
    }else{
      all_reasons <- c(all_reasons, reasons[j])
    }
  }
}

all_reasons <- unique(str_trim(all_reasons))
all_reasons

policy_simpl <- c("governance measure = implied", "not governance measure, representative concentration pathway", "governance measure = outcome", "no governance measure", 
                  "not governance measure, farm management", "not governance measure, store management", "not governance measure, water management", "not governance measure, socioeconomic pathway",
                  "governance goal not measure")
FS_simpl <- c("about obesity", "governance measure not aimed at food security", "about health", "about malnutrition", "food security impact not included", "about food safety")
foodsystem_simpl <- c("no food system", "about bio-energy", "about fish", "ecology", "bio-medical", "hunting")
model_simpl <- c("meta-analysis", "opinion paper", "no simulation model", "vision paper", "position paper", "literature review", "framework", "life cycle assessment")
other <- c("historical")
not_eng <- c("in spanish", "in chinese")

ineligible$simpl <- ""

govpapers <- c()
FSpapers <- c()
foodpapers <- c()
modelpapers <- c()
otherpapers <- c()
notavpapers <- c()
oldpapers <- c()
confpapers <- c()
nengpapers <- c()
bookpapers <- c()
retpapers <- c()

for(i in 1:nrow(ineligible)){
  reasons <- str_trim(strsplit(ineligible$Reason[i], ";")[[1]])
  for(j in 1:length(reasons)){
    if(reasons[j] %in% policy_simpl){
      govpapers <- c(govpapers, ineligible$ID[i])
    }else if(reasons[j] %in% FS_simpl){
      FSpapers <- c(FSpapers, ineligible$ID[i])
    }else if(reasons[j] %in% foodsystem_simpl){
      foodpapers <- c(foodpapers, ineligible$ID[i])
    }else if(reasons[j] %in% model_simpl){
      modelpapers <- c(modelpapers, ineligible$ID[i])
    }else if(reasons[j] %in% other){
      otherpapers <- c(otherpapers, ineligible$ID[i])
    }else if(reasons[j] == "not available"){
      notavpapers <- c(notavpapers, ineligible$ID[i])
    }else if(reasons[j] == "too old"){
      oldpapers <- c(oldpapers, ineligible$ID[i])
    }else if(reasons[j] == "conference"){
      confpapers <- c(confpapers, ineligible$ID[i])
    }else if(reasons[j] %in% not_eng){
      nengpapers <- c(nengpapers, ineligible$ID[i])
    }else if(reasons[j] == "book"){
      bookpapers <- c(bookpapers, ineligible$ID[i])
    }else if(reasons[j] == "retracted article"){
      retpapers <- c(retpapers, ineligible$ID[i])
    }
  }
}

govpapers <- unique(govpapers)
FSpapers <- unique(FSpapers)
foodpapers <- unique(foodpapers)
modelpapers <- unique(modelpapers)
otherpapers <-unique(otherpapers)
notavpapers <- unique(notavpapers)
oldpapers <- unique(oldpapers)
confpapers <- unique(confpapers)
nengpapers <- unique(nengpapers)

govpapers <- govpapers[!(govpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
FSpapers <- FSpapers[!(FSpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
foodpapers <- foodpapers[!(foodpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
modelpapers <- modelpapers[!(modelpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
otherpapers <- otherpapers[!(otherpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
notavpapers <- notavpapers[!(notavpapers %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]
confpapers <- confpapers[!(confpapers %in% c(oldpapers))]
bookpapers <- bookpapers[!(bookpapers %in% c(oldpapers, confpapers))]
nengpapers <- nengpapers[!(notavpapers %in% c(oldpapers, confpapers, bookpapers))]
retpapers <- retpapers[!(retpapers %in% c(oldpapers, confpapers, bookpapers, nengpapers))]

screened <- papers$ID[!(papers$ID %in% c(oldpapers, confpapers, nengpapers, bookpapers, retpapers))]

myCol <- c("yellow", "deeppink", 
           #"green", 
           "blue", "turquoise1")

venn.diagram(
  x = list(govpapers, FSpapers, 
           #foodpapers, 
           modelpapers, otherpapers),
  category.names = c("Governance" , "Food security" , 
                     #"Food system", 
                     "Simulation model", "Historical"),
  filename = '../13042021_Figures/abstract_exclusion_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 780 , 
  width = 880 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 1,
  lty = 1,
  fill = myCol,
  
  # Numbers
  cex = .5,
  #fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.4,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  # cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.085, 
               #0.075, 
               0.075, 0.085),
  cat.fontfamily = "sans"# ,
  # rotation = 1
)




