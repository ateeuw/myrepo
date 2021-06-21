# here I will organise the data so that I can perform a cluster analysis

# I will do this by making one dataframe for each type of code: papers, modelling, spatial & temporal, food system, governance, and agent

# Load data ###################
datadir <- "../Atlas_export_sheets"
review_all_columns <- read_excel(paste0(datadir, "/", "all_quotes.xlsx")) # read_excel("C:/Users/MeyerMA/OneDrive - Universiteit Twente/Paper/Review paper/Import_R_case_studies.xlsx")
# Load data ###################

# Pre-process data ########################
quotes <- review_all_columns[review_all_columns$`Document Groups` == "!Read for Lit review - eligible",] #ignore codes attached in papers ineligible for literature review

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

rm(list = c("new_row", "code", "codes_vec", "codes", "i", "j"))

quotes_long$name_id <- 1:nrow(quotes_long)

# Fix spatial & temporal - extent and resolution
quotes_long2 <- quotes_long
quotes_long2$code_group[which(quotes_long2$code_group == "spatial & temporal - spatial extent [m2]")] <- "spatial & temporal - spatial extent"
quotes_long2$code_group[which(quotes_long2$code_group == "spatial & temporal - spatial resolution [m2]")] <- "spatial & temporal - spatial resolution"
quotes_long2$code_group[which(quotes_long2$code_group == "spatial & temporal - temporal extent [d]")] <- "spatial & temporal - temporal extent"
quotes_long2$code_group[which(quotes_long2$code_group == "spatial & temporal - temporal resolution [d]")] <- "spatial & temporal - temporal resolution"


quotes_wide <- quotes_long2 %>% spread(code_group, name)
# Pre-process data ########################

# papers #############################
papers <- quotes_wide

paper_codes <- c("papers - author", "papers - first author", "papers - last author", "papers - title", "papers - year", "papers - journal", "papers - sound science?")
keep <- which(colnames(papers) %in% c(paper_codes, "name_id", "Document"))
papers <- papers[,keep]
empty_rows <- which(rowSums(is.na(papers[,3:ncol(papers)]))==length(3:ncol(papers)))
papers <- papers[-empty_rows,]

for(a in unique(papers$Document)){
  adata <- papers[papers$Document == a,]
  
  for(clm in 4:9){
    row <- which(!is.na(adata[,clm]))
    
    if(length(row)<1){
      print("error: missing code, check")
      print(paste("Document = ", a))
      print(paste("Column = ", clm))
    
      }else if(length(row)>1){
      
        if(nrow(unique(adata[row,clm]))==1)
        {
        code <- unique(adata[row,clm])
        papers[papers$Document == a,clm] = code
      
        }else{
        print("error: too many codes, check")
        print(paste("Document = ", a))
        print(paste("Column = ", clm))
        }
    
      }else{
      
          code <- adata[row,clm]
          papers[papers$Document == a,clm] = code
      }
    }
}

papers <- papers[!is.na(papers$`papers - author`),]
papers <- papers[,-(which(colnames(papers)=="name_id"))]
papers <- papers %>% distinct()

# papers #############################

# modelling #############################
modelling <- quotes_wide
modelling_codes <- c("modelling - coupling?", "modelling - aim", "modelling - feedback-loop?", "modelling - feedback-loop?", "modelling - sensitivity analysis?", "modelling - validation?", "modelling - data")
keep <- which(colnames(modelling) %in% c(modelling_codes, "name_id", "Document"))
modelling <- modelling[,keep]
empty_rows <- which(rowSums(is.na(modelling[,3:ncol(modelling)]))==length(3:ncol(modelling)))
modelling <- modelling[-empty_rows,]

for(a in unique(modelling$Document)){
  adata <- modelling[modelling$Document == a,]
  
  for(clm in c(4,6:8)){
    row <- which(!is.na(adata[,clm]))
    
    if(length(row)<1){
      print("error: missing code, check")
      print(paste("Document = ", a))
      print(paste("Column = ", clm))
      
    }else if(length(row)>1){
      
      if(nrow(unique(adata[row,clm]))==1)
      {
        code <- unique(adata[row,clm])
        modelling[modelling$Document == a,clm] = code
        
      }else{
        print("error: too many codes, check")
        print(paste("Document = ", a))
        print(paste("Column = ", clm))
      }
      
    }else{
      
      code <- adata[row,clm]
      modelling[modelling$Document == a,clm] = code
    }
  }
}

modelling_aim <- modelling[,-5]
modelling_aim <- modelling_aim[!is.na(modelling_aim$`modelling - aim`),]
modelling_aim <- modelling_aim[,-(which(colnames(modelling_aim)=="name_id"))]
modelling_aim <- modelling_aim %>% distinct()

modelling_dat <- modelling[,-3]
modelling_dat <- modelling_dat[!is.na(modelling_dat$`modelling - data`),]
modelling_dat <- modelling_dat[,-(which(colnames(modelling_dat)=="name_id"))]
modelling_dat <- modelling_dat %>% distinct()

modelling <- merge(modelling_aim, modelling_dat)
# modelling #############################

# spatial & temporal #############################
spatial <- quotes_wide
spatial_codes <- c("spatial & temporal - country", "spatial & temporal - ref scale", "spatial & temporal - representation", 
                   "spatial & temporal - spatial extent", "spatial & temporal - spatial resolution", "spatial & temporal - temporal extent",
                   "spatial & temporal - temporal resolution")
keep <- which(colnames(spatial) %in% c(spatial_codes, "name_id", "Document"))
spatial <- spatial[,keep]
empty_rows <- which(rowSums(is.na(spatial[,3:ncol(spatial)]))==length(3:ncol(spatial)))
spatial <- spatial[-empty_rows,]

for(a in unique(spatial$Document)){
  adata <- spatial[spatial$Document == a,]
  
  for(clm in c(5)){
    row <- which(!is.na(adata[,clm]))
    
    if(length(row)<1){
      print("error: missing code, check")
      print(paste("Document = ", a))
      print(paste("Column = ", clm))
      print("")
      
    }else if(length(row)>1){
      
      if(nrow(unique(adata[row,clm]))==1)
      {
        code <- unique(adata[row,clm])
        spatial[spatial$Document == a,clm] <- code
        
      }else{
        print("error: too many codes, check")
        print(paste("Document = ", a))
        print(paste("Column = ", clm))
        print("")
      }
      
    }else{
      
      code <- adata[row,clm]
      spatial[spatial$Document == a,clm] = code
    }
  }
}

spatial_ctr <- spatial[,-c(4, 6, 7, 8, 9)]
spatial_ctr <- spatial_ctr[!is.na(spatial_ctr$`spatial & temporal - country`),]
spatial_ctr <- spatial_ctr[,-(which(colnames(spatial_ctr)=="name_id"))]
spatial_ctr <- spatial_ctr %>% distinct()

spatial_rsc <- spatial[,-c(3, 6, 7, 8, 9)]
spatial_rsc <- spatial_rsc[!is.na(spatial_rsc$`spatial & temporal - ref scale`),]
spatial_rsc <- spatial_rsc[,-(which(colnames(spatial_rsc)=="name_id"))]
spatial_rsc <- spatial_rsc %>% distinct()

spatial_sxt <- spatial[,-c(3, 4, 7, 8, 9)]
spatial_sxt <- spatial_sxt[!is.na(spatial_sxt$`spatial & temporal - spatial extent`),]
spatial_sxt <- spatial_sxt[,-(which(colnames(spatial_sxt)=="name_id"))]
spatial_sxt <- spatial_sxt %>% distinct()

spatial_srs <- spatial[,-c(3, 4, 6, 8, 9)]
spatial_srs <- spatial_srs[!is.na(spatial_srs$`spatial & temporal - spatial resolution`),]
spatial_srs <- spatial_srs[,-(which(colnames(spatial_srs)=="name_id"))]
spatial_srs <- spatial_srs %>% distinct()

spatial_txt <- spatial[,-c(3, 4, 6, 7, 9)]
spatial_txt <- spatial_txt[!is.na(spatial_txt$`spatial & temporal - temporal extent`),]
spatial_txt <- spatial_txt[,-(which(colnames(spatial_txt)=="name_id"))]
spatial_txt <- spatial_txt %>% distinct()

spatial_trs <- spatial[,-c(3, 4, 6, 7, 8)]
spatial_trs <- spatial_trs[!is.na(spatial_trs$`spatial & temporal - temporal resolution`),]
spatial_trs <- spatial_trs[,-(which(colnames(spatial_trs)=="name_id"))]
spatial_trs <- spatial_trs %>% distinct()

spatial <- merge(spatial_ctr, spatial_rsc)
spatial <- merge(spatial, spatial_sxt)
spatial <- merge(spatial, spatial_srs)
spatial <- merge(spatial, spatial_txt)
spatial <- merge(spatial, spatial_trs)

# spatial & temporal #############################

# food system #############################
food <- quotes_wide
food_codes <- c("food system - commodity", "nonfood system - commodity", "food system - echelon")
keep <- which(colnames(food) %in% c(food_codes, "name_id", "Document"))
food <- food[,keep]
empty_rows <- which(rowSums(is.na(food[,3:ncol(food)]))==length(3:ncol(food)))
food <- food[-empty_rows,]

food_com <- food[,-c(4,5)]
food_com <- food_com[!is.na(food_com$`food system - commodity`),]
food_com <- food_com[,-(which(colnames(food_com)=="name_id"))]
food_com <- food_com %>% distinct()

food_ech <- food[,-c(3,5)]
food_ech <- food_ech[!is.na(food_ech$`food system - echelon`),]
food_ech <- food_ech[,-(which(colnames(food_ech)=="name_id"))]
food_ech <- food_ech %>% distinct()

food_ncom <- food[,-c(3,4)]
food_ncom <- food_ncom[!is.na(food_ncom$`nonfood system - commodity`),]
food_ncom <- food_ncom[,-(which(colnames(food_ncom)=="name_id"))]
food_ncom <- food_ncom %>% distinct()

food <- merge(food_com, food_ech)
food <- merge(food, food_ncom)

# food system #############################

# governance & per measure - measure #############################
governance <- quotes_wide
governance_codes <- c("governance - combined measures?", "per measure - measure")
keep <- which(colnames(governance) %in% c(governance_codes, "ID", "name_id", "Document"))
governance  <- governance[,keep]
empty_rows <- which(rowSums(is.na(governance[,4:ncol(governance)]))==length(4:ncol(governance)))
governance <- governance[-empty_rows,]

# first get combined? governance measure right

for(a in unique(governance$Document)){
  adata <- governance[governance$Document==a,]
  
  combined <- unique(adata$`governance - combined measures?`)
  combined <- combined[!is.na(combined)]
  
  print(a)
  print(combined)
  print("")
  
  if("yes" %in% combined){
    y_id <- adata$ID[which(adata$`governance - combined measures?` == "yes")]
    y_measures <- adata$`per measure - measure`[which(adata$ID %in% y_id)]
    y_measures <- y_measures[!is.na(y_measures)]
    
    governance[governance$Document==a & governance$`per measure - measure` %in% y_measures,]$`governance - combined measures?` <- "yes"
    
  }else{
    y_measures <- "none!"
  }
  
  n_measures <- adata$`per measure - measure`[!(adata$`per measure - measure` %in% y_measures)]
  n_measures <- unique(n_measures)
  n_measures <- n_measures[!is.na(n_measures)]
  if(length(n_measures) >= 1){
    governance[governance$Document==a & governance$`per measure - measure` %in% n_measures,]$`governance - combined measures?` <- "no"
  }
}

governance <- governance[!is.na(governance$`per measure - measure`),]
governance <- governance[,-(which(colnames(governance) %in% c("name_id", "ID")))]
governance <- governance %>% distinct()
# governance & per measure - measure #############################

# agent & per agent - agent #############################
agent <- quotes_wide
agent_codes <- c("agent - representation", "per agent - agent")
keep <- which(colnames(agent) %in% c(agent_codes, "name_id", "Document"))
agent<- agent[,keep]
empty_rows <- which(rowSums(is.na(agent[,3:ncol(agent)]))==length(3:ncol(agent)))
agent <- agent[-empty_rows,]

agent_rep <- agent[,-c(4)]
agent_rep <- agent_rep[!is.na(agent_rep$`agent - representation`),]
agent_rep <- agent_rep[,-(which(colnames(agent_rep)=="name_id"))]
agent_rep <- agent_rep %>% distinct()

agent_agt <- agent[,-c(3)]
agent_agt <- agent_agt[!is.na(agent_agt$`per agent - agent`),]
agent_agt <- agent_agt[,-(which(colnames(agent_agt)=="name_id"))]
agent_agt <- agent_agt %>% distinct()

agent <- merge(agent_rep, agent_agt)

# agent & per agent - agent #############################

# per model #############################
permodel <- quotes_wide
permodel_codes <- c("per model - type", "per model - subtype", "per model - domain", "per model - subdomain")
keep <- which(colnames(permodel) %in% c(permodel_codes, "ID", "name_id", "Document"))
permodel <- permodel[,keep]
empty_rows <- which(rowSums(is.na(permodel[,4:ncol(permodel)]))==length(4:ncol(permodel)))
permodel <- permodel[-empty_rows,]

for(a in unique(permodel$Document)){
  adata <- permodel[permodel$Document==a,]
  
  for(i in unique(adata$ID)){
    idata <- adata[adata$ID==i,]
    mtype <- unique(idata$`per model - type`)[!is.na(unique(idata$`per model - type`))]
    
    if(length(mtype)==0){
      print("warning: no model type given for this quotation")
      print(paste("Document = ", a))
      print(paste("Quotation = ", i))
      print("")
    }else if(length(mtype)==1){
      permodel[permodel$ID == i,]$`per model - type` <- mtype
    }else{
      permodel[permodel$ID == i,]$`per model - type` <- mtype[1]
      for(m in 2:length(mtype)){
        to_add <- permodel[permodel$ID == i,]
        to_add$`per model - type` <- mtype[m]
        permodel <- rbind(permodel, to_add)
      }
    }
  }
}

permodel_dom <- permodel[,-c(5,6)]
permodel_dom <- permodel_dom[!is.na(permodel_dom$`per model - domain`),]
permodel_dom <- permodel_dom[,-(which(colnames(permodel_dom)%in%c("name_id", "ID")))]
permodel_dom <- permodel_dom %>% distinct()

permodel_sdom <- permodel[,-c(4,6)]
permodel_sdom <- permodel_sdom[!is.na(permodel_sdom$`per model - subdomain`),]
permodel_sdom <- permodel_sdom[,-(which(colnames(permodel_sdom)%in%c("name_id", "ID")))]
permodel_sdom <- permodel_sdom %>% distinct()

permodel_styp <- permodel[,-c(4,5)]
permodel_styp <- permodel_styp[!is.na(permodel_styp$`per model - subtype`),]
permodel_styp <- permodel_styp[,-(which(colnames(permodel_styp)%in%c("name_id", "ID")))]
permodel_styp <- permodel_styp %>% distinct()

permodel <- merge(permodel_dom, permodel_sdom)
permodel <- merge(permodel, permodel_styp)

# per model #############################

# per measure #############################
permeas <- quotes_wide
permeas_codes <- c("per measure - measure", "per measure - type", "per measure - type 2", "per measure - objective", "per measure - formulation",
                   "per measure - scale", "per measure - target implementer", "per measure - spatially targeted?")
keep <- which(colnames(permeas) %in% c(permeas_codes, "ID", "name_id", "Document"))
permeas <- permeas[,keep]
empty_rows <- which(rowSums(is.na(permeas[,4:ncol(permeas)]))==length(4:ncol(permeas)))
permeas <- permeas[-empty_rows,]

for(a in unique(permeas$Document)){
  adata <- permeas[permeas$Document==a,]
  
  for(i in unique(adata$ID)){
    idata <- adata[adata$ID==i,]
    mtype <- unique(idata$`per measure - measure`)[!is.na(unique(idata$`per measure - measure`))]
    
    if(length(mtype)==0){
      print("warning: no measure given for this quotation")
      print(paste("Document = ", a))
      print(paste("Quotation = ", i))
      print("")
    }else if(length(mtype)==1){
      permeas[permeas$ID == i,]$`per measure - measure` <- mtype
    }else{
      permeas[permeas$ID == i,]$`per measure - measure` <- mtype[1]
      for(m in 2:length(mtype)){
        to_add <- permeas[permeas$ID == i,]
        to_add$`per measure - measure` <- mtype[m]
        permeas <- rbind(permeas, to_add)
      }
    }
  }
}

for(a in unique(permeas$Document)){
  adata <- permeas[permeas$Document==a,]
  
  for(i in unique(adata$ID)){
    idata <- adata[adata$ID==i,]
    
    for(clm in c(7,8)){
      row <- which(!is.na(idata[,clm]))
      
      if(length(row)<1){
        print("warning: missing code, check")
        print(paste("Document = ", a))
        print(paste("Quotation = ", i))
        print(paste("Column = ", clm))
        print("")
        
      }else if(length(row)>1){
        
        if(nrow(unique(idata[row,clm]))==1)
        {
          code <- unique(idata[row,clm])
          permeas[permeas$ID == i,clm] <- code
          
        }else{
          print("warning: too many codes, check")
          print(paste("Document = ", a))
          print(paste("Quotation = ", i))
          print(paste("Column = ", clm))
          print("")
        }
        
      }else{
        
        code <- idata[row,clm]
        permeas[permeas$ID == i,clm] = code
      }
    }
  }
}

permeas_frm <- permeas[,-c(6,9,10,11)]
permeas_frm <- permeas_frm[!is.na(permeas_frm$`per measure - formulation`),]
permeas_frm <- permeas_frm[,-(which(colnames(permeas_frm)%in%c("name_id", "ID")))]
permeas_frm <- permeas_frm %>% distinct()

permeas_obj <- permeas[,-c(4,9,10,11)]
permeas_obj <- permeas_obj[!is.na(permeas_obj$`per measure - objective`),]
permeas_obj <- permeas_obj[,-(which(colnames(permeas_obj)%in%c("name_id", "ID")))]
permeas_obj <- permeas_obj %>% distinct()

permeas_tim <- permeas[,-c(4,6,10,11)]
permeas_tim <- permeas_tim[!is.na(permeas_tim$`per measure - target implementer`),]
permeas_tim <- permeas_tim[,-(which(colnames(permeas_tim)%in%c("name_id", "ID")))]
permeas_tim <- permeas_tim %>% distinct()

permeas_typ <- permeas[,-c(4,6,9,11)]
permeas_typ <- permeas_typ[!is.na(permeas_typ$`per measure - type`),]
permeas_typ <- permeas_typ[,-(which(colnames(permeas_typ)%in%c("name_id", "ID")))]
permeas_typ <- permeas_typ %>% distinct()

permeas_typ2 <- permeas[,-c(4,6,9,10)]
permeas_typ2 <- permeas_typ2[!is.na(permeas_typ2$`per measure - type 2`),]
permeas_typ2 <- permeas_typ2[,-(which(colnames(permeas_typ2)%in%c("name_id", "ID")))]
permeas_typ2 <- permeas_typ2 %>% distinct()

permeas <- merge(permeas_frm, permeas_obj)
permeas <- merge(permeas, permeas_tim)
permeas <- merge(permeas, permeas_typ)
permeas <- merge(permeas, permeas_typ2)

# per measure #############################

# per effect ############################# not quite right but close <- use the other for loop used for measure
pereff <- quotes_wide
pereff_codes <- c("per measure - measure", "per effect - FS indicator", "per effect - direction FS indicator",
                  "per effect - place", "per effect - unit", "per effect - direct?", "per effect - on FS?",
                  "per effect - affected agent", "per effect - type other")
keep <- which(colnames(pereff) %in% c(pereff_codes, "ID", "name_id", "Document"))
pereff <- pereff[,keep]
empty_rows <- which(rowSums(is.na(pereff[,4:ncol(pereff)]))==length(4:ncol(pereff)))
pereff <- pereff[-empty_rows,]

# I need two do two tricks:
# 1) split type other - none from the rest
# 2) split non-FS and FS effects
# 3) process all three separately
# 3a) process no other

# 1) remove all type other - none
no_other_ids <- pereff$ID[which(pereff$`per effect - type other` == "none")]
no_other <- pereff[which(pereff$ID %in% no_other_ids),]
no_other <- no_other[,-c(4:9,11)]
pereff$`per effect - type other`[pereff$`per effect - type other` == "none"] <- NA

# 2) split non-FS and FS effects
non_FS_ids <- pereff$ID[which(pereff$`per effect - on FS?` == "no")]
non_FS <- pereff[which(pereff$ID %in% non_FS_ids),]
non_FS <- non_FS[,-c(6,7,11)]

FS_ids <- pereff$ID[which(pereff$`per effect - on FS?` == "yes")] 
FS <- pereff[which(pereff$ID %in% FS_ids),]
FS <- FS[,-10]

# 3) process all three separately
# 3a) process no other

for(a in unique(no_other$Document)){
  adata <- no_other[no_other$Document==a,]
  
  for(i in unique(adata$ID)){
    idata <- adata[adata$ID==i,]
    
    for(clm in c(4)){ #5,8,9
      row <- which(!is.na(idata[,clm]))
      
      if(length(row)<1){
        print("warning: missing code, check")
        print(paste("Document = ", a))
        print(paste("Quotation = ", i))
        print(paste("Column = ", clm))
        print("")
        
      }else if(length(row)>1){
        
        if(nrow(unique(idata[row,clm]))==1)
        {
          code <- unique(idata[row,clm])
          no_other[no_other$ID == i,clm] <- code
          
        }else{
          #print("warning: too many codes, check")
          #print(paste("Document = ", a))
          #print(paste("Quotation = ", i))
          #print(paste("Column = ", clm))
          #print("")
        }
        
      }else{
        
        code <- idata[row,clm]
        no_other[no_other$ID == i,clm] = code
      }
    }
  }
}

no_other <- no_other[!is.na(no_other$`per measure - measure`),]
no_other$`per effect - on FS?` <- "no"
no_other <- no_other[,-(which(colnames(no_other)%in%c("name_id", "ID")))]
no_other <- no_other %>% distinct()

# 3b) no FS

for(a in unique(non_FS$Document)){
  adata <- non_FS[non_FS$Document==a,]
  
  for(i in unique(adata$ID)){
    idata <- adata[adata$ID==i,]
    
    for(clm in c(4:8)){ #5:7
      row <- which(!is.na(idata[,clm]))
      
      if(length(row)<1){
        #print("warning: missing code, check")
        #print(paste("Document = ", a))
        print(paste("Quotation = ", i))
        print(paste("Column = ", clm))
        print("")
        
      }else if(length(row)>1){
        
        if(nrow(unique(idata[row,clm]))==1)
        {
          code <- unique(idata[row,clm])
          non_FS[non_FS$ID == i,clm] <- code
          
        }else{
          #print("warning: too many codes, check")
          #print(paste("Document = ", a))
          #print(paste("Quotation = ", i))
          #print(paste("Column = ", clm))
          #print("")
        }
        
      }else{
        
        code <- idata[row,clm]
        non_FS[non_FS$ID == i,clm] = code
      }
    }
  }
}

non_FS_aff <- non_FS[,-c(8,9)]
non_FS_aff <- non_FS_aff[!is.na(non_FS_aff$`per effect - affected agent`),]
non_FS_aff <- non_FS_aff[,-(which(colnames(non_FS_aff)%in%c("name_id", "ID")))]
non_FS_aff <- non_FS_aff %>% distinct()

non_FS_tot <- non_FS[,-c(4,9)]
non_FS_tot <- non_FS_tot[!is.na(non_FS_tot$`per effect - type other`),]
non_FS_tot <- non_FS_tot[,-(which(colnames(non_FS_tot)%in%c("name_id", "ID")))]
non_FS_tot <- non_FS_tot %>% distinct()

non_FS_mes <- non_FS[,-c(4,8)]
non_FS_mes <- non_FS_mes[!is.na(non_FS_mes$`per measure - measure`),]
non_FS_mes <- non_FS_mes[,-(which(colnames(non_FS_mes)%in%c("name_id", "ID")))]
non_FS_mes <- non_FS_mes %>% distinct()

non_FS <- merge(non_FS_aff, non_FS_tot)
non_FS <- merge(non_FS, non_FS_mes)

# 3c) on FS
for(a in unique(FS$Document)){
  adata <- FS[FS$Document==a,]
  
  for(i in unique(adata$ID)){
    idata <- adata[adata$ID==i,]
    
    for(clm in c(5,8,9)){ #5:7
      row <- which(!is.na(idata[,clm]))
      
      if(length(row)<1){
        #print("warning: missing code, check")
        #print(paste("Document = ", a))
        print(paste("Quotation = ", i))
        print(paste("Column = ", clm))
        print("")
        
      }else if(length(row)>1){
        
        if(nrow(unique(idata[row,clm]))==1)
        {
          code <- unique(idata[row,clm])
          FS[FS$ID == i,clm] <- code
          
        }else{
          #print("warning: too many codes, check")
          #print(paste("Document = ", a))
          #print(paste("Quotation = ", i))
          #print(paste("Column = ", clm))
          #print("")
        }
        
      }else{
        
        code <- idata[row,clm]
        FS[FS$ID == i,clm] = code
      }
    }
  }
}

FS_aff <- FS[,-c(6,7,10,11)]
FS_aff <- FS_aff[!is.na(FS_aff$`per effect - affected agent`),]
FS_aff <- FS_aff[,-(which(colnames(FS_aff)%in%c("name_id", "ID")))]
FS_aff <- FS_aff %>% distinct()

FS_dFS <- FS[,-c(4,7,10,11)]
FS_dFS <- FS_dFS[!is.na(FS_dFS$`per effect - direction FS indicator`),]
FS_dFS <- FS_dFS[,-(which(colnames(FS_dFS)%in%c("name_id", "ID")))]
FS_dFS <- FS_dFS %>% distinct()

FS_FSi <- FS[,-c(4,6,10,11)]
FS_FSi <- FS_FSi[!is.na(FS_FSi$`per effect - FS indicator`),]
FS_FSi <- FS_FSi[,-(which(colnames(FS_FSi)%in%c("name_id", "ID")))]
FS_FSi <- FS_FSi %>% distinct()

FS_uni <- FS[,-c(4,6,7,11)]
FS_uni <- FS_uni[!is.na(FS_uni$`per effect - unit`),]
FS_uni <- FS_uni[,-(which(colnames(FS_uni)%in%c("name_id", "ID")))]
FS_uni <- FS_uni %>% distinct()

FS_mes <- FS[,-c(4,6,7,10)]
FS_mes <- FS_mes[!is.na(FS_mes$`per measure - measure`),]
FS_mes <- FS_mes[,-(which(colnames(FS_mes)%in%c("name_id", "ID")))]
FS_mes <- FS_mes %>% distinct()

FS <- merge(FS_aff, FS_dFS)
FS <- merge(FS, FS_FSi)
FS <- merge(FS, FS_uni)
FS <- merge(FS, FS_mes)

# per effect #############################

# all data #############################
all_dat <- merge(papers, modelling)
all_dat <- all_dat %>% distinct()
all_dat <- merge(all_dat, spatial)
all_dat <- all_dat %>% distinct()
all_dat <- merge(all_dat, food)
all_dat <- all_dat %>% distinct()
all_dat <- merge(all_dat, governance)
all_dat <- all_dat %>% distinct()
all_dat <- merge(all_dat, agent)
all_dat <- all_dat %>% distinct()
all_dat <- merge(all_dat, permodel)
all_dat <- all_dat %>% distinct()
all_dat <- merge(all_dat, permeas)
all_dat <- all_dat %>% distinct()
# all_dat <- merge(all_dat, FS)
# all_dat <- all_dat %>% distinct()
# 
# 
# 
# all_dat2 <- alldat
# all_dat2 <- merge(all_dat2, no_other)
# all_dat3 <- alldat2
# all_dat3 <- merge(all_dat3, non_FS)

# all data #############################



# code_groups <- colnames(quotes_wide)
# code_groups <- c("papers - year", 
#                  "agent - representation", "per agent - agent", 
#                  "food system - commodity", "food system - echelon", "nonfood system - commodity",
#                  "modelling - aim", "modelling - coupling?", "modelling - data", "modelling - feedback-loop?", "modelling - sensitivity analysis?", "modelling - validation?", "per model - domain", "per model - subdomain", "per model - subtype", "per model - type", 
#                  "per effect - FS indicator", 
#                  "per measure - formulation", "per measure - measure", "per measure - objective", "per measure - scale", "per measure - spatially targeted?", "per measure - target implementer", "per measure - type", "per measure - type 2",
#                  "spatial & temporal - country", "spatial & temporal - ref scale", "spatial & temporal - representation", "spatial & temporal - spatial extent [m2]", "spatial & temporal - spatial extent", "spatial & temporal - spatial resolution [m2]", "spatial & temporal - spatial resolution", "spatial & temporal - temporal extent [d]", "spatial & temporal - temporal extent", "spatial & temporal - temporal resolution [d]", "spatial & temporal - temporal resolution [d]")

#quotes_wide <- quotes_wide[,c(-1,-2, -4:-14)]

# keep <- which(colnames(quotes_wide) %in% c(code_groups, "name_id", "Document"))

# quotes_wide <- quotes_wide[,keep]

# Pre-process data ########################