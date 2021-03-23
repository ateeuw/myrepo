# This is a document where I test how I can analyse my atlas ti data
# Now I will clean it (23.3.2021)

# load libraries
library("readxl") #for reading excel files
library("tidyr") #for data processing 
library("dplyr") #for data processing 
library("ggplot2") #for visualisation
library("RColorBrewer") #for color pallettes
library("formattable") #for making ncie tables
library("kableExtra") #for making nice tables
library("networkD3") #for sankey diagrams
library("htmltools") #to manage html objects
library("webshot") #to save screenshots of html objects
library("ggpubr") #to make nice figures

###################### define paths for import and export ###################### 
datadir <- "../Atlas_export_sheets"
figdir <- "../Figures"

###################### source functions ###################### 
source("./functions/away_codegr.R")
source("./functions/away_gr.R")
source("./functions/away_spaces.R")
source("./functions/away_totals.R")
source("./functions/dict_classification.R")
source("./functions/level1_count.R")
source("./functions/export_formattable.R")

###################### load data ###################### 
quotes <- read_excel(paste0(datadir, "/", "all_quotes.xlsx"))

###################### pre-process quotes ########################
quotes <- quotes[quotes$`Document Groups` == "!Read for Lit review - eligible",] #ignore codes attached in papers ineligible for literature review

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
###################### pre-process quotes ########################

###################### prepare papers - year ########################
year <- quotes_long[quotes_long$code_group == "papers - year",]
year <- year[!is.na(year$code_group),]
year_sum <- level1_count(sheet = year)

y_range <- min(as.numeric(as.character(year_sum$name))):max(as.numeric(as.character(year_sum$name)))

year_sum$name <- factor(year_sum$name, levels = y_range)
year_sum <- year_sum[order(year_sum$name),]
colnames(year_sum) <- c("publication year", "number")
year_sum$proportion <- round(year_sum$number/n_studies, 2)

year_sum$number <- color_bar("lightgreen")(year_sum$number)

ft_year <- year_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_year

rm(list = c("year", "year_sum"))
###################### prepare papers - year ########################

###################### prepare papers - journal ########################
jrnl <- quotes_long[quotes_long$code_group == "papers - journal",]
jrnl <- jrnl[!is.na(jrnl$code_group),]
jrnl_sum <- level1_count(sheet = jrnl)

jrnl_sum$name <- factor(jrnl_sum$name, levels = rev(unique(jrnl_sum$name)))
colnames(jrnl_sum) <- c("journal", "number")
jrnl_sum$proportion <- round(jrnl_sum$number/n_studies, 2)

jrnl_sum$number <- color_bar("lightgreen")(jrnl_sum$number)

ft_jrnl <- jrnl_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_jrnl

rm(list = c("jrnl", "jrnl_sum"))
###################### prepare papers - journal ########################

###################### prepare modelling - coupling? ########################
coup <- quotes_long[quotes_long$code_group == "modelling - coupling?",]
coup <- coup [!is.na(coup $code_group),]
coup_sum <- level1_count(sheet = coup )

coup_sum$name <- factor(coup_sum$name, levels = rev(unique(coup_sum$name)))
colnames(coup_sum) <- c("model coupling?", "number")
coup_sum$proportion <- round(coup_sum$number/n_studies, 2)

coup_sum$number <- color_bar("lightblue")(coup_sum$number)

ft_coup <- coup_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_coup

rm(list = c("coup", "coup_sum"))
###################### prepare modelling - coupling? ########################

###################### prepare modelling - aim ########################
aim <- quotes_long[quotes_long$code_group == "modelling - aim",]
aim <- aim[!is.na(aim$code_group),]
aim_sum <- level1_count(sheet = aim)

aim_sum$name <- factor(aim_sum$name, levels = rev(unique(aim_sum$name)))
colnames(aim_sum) <- c("model aim", "number")
aim_sum$proportion <- round(aim_sum$number/n_studies, 2)

aim_sum$number <- color_bar("lightblue")(aim_sum$number)

ft_aim <- aim_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_aim

rm(list = c("aim", "aim_sum"))
###################### prepare modelling - aim ########################

###################### prepare modelling - feedback-loops? ########################
loop <- quotes_long[quotes_long$code_group == "modelling - feedback-loop?",]
loop <- loop[!is.na(loop$code_group),]
loop_sum <- level1_count(sheet = loop)

loop_sum$name <- factor(loop_sum$name, levels = rev(unique(loop_sum$name)))
colnames(loop_sum) <- c("feedback loops in model?", "number")
loop_sum$proportion <- round(loop_sum$number/n_studies, 2)

loop_sum$number <- color_bar("lightblue")(loop_sum$number)

ft_loop <- loop_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_loop

rm(list = c("loop", "loop_sum"))
###################### prepare modelling - feedback-loops? ########################

###################### prepare modelling - sensitivity analysis? ########################
sens <- quotes_long[quotes_long$code_group == "modelling - sensitivity analysis?",]
sens <- sens[!is.na(sens$code_group),]
sens_sum <- level1_count(sheet = sens)

sens_sum$name <- factor(sens_sum$name, levels = rev(unique(sens_sum$name)))
colnames(sens_sum) <- c("sensitivity analysis in model?", "number")
sens_sum$proportion <- round(sens_sum$number/n_studies, 2)

sens_sum$number <- color_bar("lightblue")(sens_sum$number)

ft_sens <- sens_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_sens

rm(list = c("sens", "sens_sum"))
###################### prepare modelling - sensitivity analysis? ########################

###################### prepare modelling - validation? ########################
val <- quotes_long[quotes_long$code_group == "modelling - validation?",]
val <- val[!is.na(val$code_group),]
val_sum <- level1_count(sheet = val)

val_sum$name <- factor(val_sum$name, levels = rev(unique(val_sum$name)))
colnames(val_sum) <- c("model validated?", "number")
val_sum$proportion <- round(val_sum$number/n_studies, 2)

val_sum$number <- color_bar("lightblue")(val_sum$number)

ft_val <- val_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_val

rm(list = c("val", "val_sum"))
###################### prepare modelling - validation? ########################

###################### prepare modelling - data ########################
modelling_data <- quotes_long[quotes_long$code_group == "modelling - data",]
modelling_data <- modelling_data[!is.na(modelling_data$code_group),]
modelling_data_sum <- level1_count(sheet = modelling_data)

modelling_data_sum$name <- factor(modelling_data_sum$name, levels = rev(unique(modelling_data_sum$name)))
colnames(modelling_data_sum) <- c("type of data", "number")
modelling_data_sum$proportion <- round(modelling_data_sum$number/n_studies, 2)

modelling_data_sum$number <- color_bar("lightblue")(modelling_data_sum$number)

ft_data <- modelling_data_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_data

rm(list = c("modelling_data", "modelling_data_sum"))

###################### prepare modelling - data ########################

###################### prepare per model - type ########################
mtype <- quotes_long[quotes_long$code_group == "per model - type",]
mtype <- mtype[!is.na(mtype$code_group),]
mtype_sum <- level1_count(sheet = mtype)
mtype_sum$name <- factor(mtype_sum$name, levels = rev(unique(mtype_sum$name)))
colnames(mtype_sum) <- c("type of model", "number")
mtype_sum$proportion <- round(mtype_sum$number/n_studies, 2)

mtype_sum$number <- color_bar("lightblue")(mtype_sum$number)

ft_mtyp <- mtype_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_mtyp 

rm(list = c("mtype", "mtype_sum"))
###################### prepare per model - type ########################

###################### prepare per model - domain ########################
mdom <- quotes_long[quotes_long$code_group == "per model - domain",]
mdom <- mdom[!is.na(mdom$code_group),]
mdom_sum <- level1_count(sheet = mdom)
mdom_sum$name <- factor(mdom_sum$name, levels = rev(unique(mdom_sum$name)))
colnames(mdom_sum) <- c("model domains", "number")
mdom_sum$proportion <- round(mdom_sum$number/n_studies, 2)

mdom_sum$number <- color_bar("lightblue")(mdom_sum$number)

ft_mdom <- mdom_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_mdom

rm(list = c("mdom", "mdom_sum"))
###################### prepare per model - domain ########################

###################### prepare per model - subdomain ########################
mdom <- quotes_long[quotes_long$code_group == "per model - subdomain",]
mdom <- mdom[!is.na(mdom$code_group),]
mdom_sum <- level1_count(sheet = mdom)
nota <- which(unique(mdom_sum$name) == "not applicable")
mdom_sum$name <- factor(mdom_sum$name, levels = c(unique(mdom_sum$name)[-nota], "not applicable"))
mdom_sum <- mdom_sum[order(mdom_sum$name),]
colnames(mdom_sum) <- c("model sub domains", "number")
mdom_sum$proportion <- round(mdom_sum$number/n_studies, 2)

mdom_sum$number <- color_bar("lightblue")(mdom_sum$number)

ft_msdom <- mdom_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_msdom

rm(list = c("mdom", "mdom_sum"))
###################### prepare per model - subdomain ########################

###################### prepare spatial & temporal - country ########################
cntr <- quotes_long[quotes_long$code_group == "spatial & temporal - country",]
cntr <- cntr[!is.na(cntr$code_group),]
cntr_sum <- level1_count(sheet = cntr)
cntr_sum$name <- factor(cntr_sum$name, levels = rev(unique(cntr_sum$name)))
colnames(cntr_sum) <- c("country", "number")
cntr_sum$proportion <- round(cntr_sum$number/n_studies, 2)

cntr_sum$number <- color_bar("lightpink")(cntr_sum$number)

ft_cntr <- cntr_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_cntr 

rm(list = c("cntr", "cntr_sum"))
###################### prepare spatial & temporal - country ########################

###################### prepare spatial & temporal - ref scale ########################
rsc <- quotes_long[quotes_long$code_group == "spatial & temporal - ref scale",]
rsc <- rsc[!is.na(rsc$code_group),]
rsc_sum <- level1_count(sheet = rsc)
rsc_sum$name <- factor(rsc_sum$name, levels = rev(c("earth", 
                                                    "earth > x > continent", 
                                                    "continent", 
                                                    "continent > x > country", 
                                                    "country", 
                                                    "country > x > province/state",
                                                    "province/state",
                                                    "province/state > x > municipality",
                                                    "municipality",
                                                    "municipality > x > city",
                                                    "city",
                                                    "city > x > city_district",
                                                    "city district")))
rsc_sum <- rsc_sum[order(rsc_sum$name),]
colnames(rsc_sum) <- c("scale of model", "number")
rsc_sum$proportion <- round(rsc_sum$number/n_studies, 2)

rsc_sum$number <- color_bar("lightpink")(rsc_sum$number)

ft_rsc <- rsc_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_rsc 

rm(list = c("rsc", "rsc_sum"))
###################### prepare spatial & temporal - ref scale ########################

###################### prepare spatial & temporal - representation ########################
repr <- quotes_long[quotes_long$code_group == "spatial & temporal - representation",]
repr <- repr[!is.na(repr$code_group),]
repr_sum <- level1_count(sheet = repr)
repr_sum$name <- factor(repr_sum$name, levels = rev(unique(repr_sum$name)))
colnames(repr_sum) <- c("spatial representation", "number")
repr_sum$proportion <- round(repr_sum$number/n_studies, 2)

repr_sum$number <- color_bar("lightpink")(repr_sum$number)

ft_repr <- repr_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_repr 

rm(list = c("repr", "repr_sum"))
###################### prepare spatial & temporal - representation ########################

###################### prepare spatial & temporal - spatial extent ########################
spext <- quotes_long[quotes_long$code_group == "spatial & temporal - spatial extent [m2]",]
spext$name <- as.numeric(as.character(spext$name))
spext <- spext[!is.na(spext$code_group),]

spext_sum <- spext %>% group_by(Document, name) %>% count(name)
spext_sum$n <- 1
spext_sum <- spext %>% group_by(Document, name) %>% count(name)
spext_sum$name <- as.numeric(as.character(spext_sum$name))
spext_sum$logname <- log(as.numeric(as.character(spext_sum$name)))
spext_sum <- spext_sum[order(spext_sum$name),]
#spext_sum <- spext_sum %>% group_by(name) %>% count(name)

quant_n <- length(unique(spext$Document))

dc_spext <- ggdotchart(spext_sum, x = "Document", y = "name",
           add = "segments",
           add.params = list(color = "black"),
          sorting = "ascending") +
  yscale("log10", .format = TRUE) +
  ylab("spatial extent (m2)") + xlab("Reviewed paper") +
  bgcolor("lightpink") +
  border("#BFD5E3") +
  ggtitle(paste(quant_n, "studies out of", n_studies))

dc_spext

rm(list = c("spext", "spext_sum", "quant_n"))
###################### prepare spatial & temporal - spatial extent ########################

###################### prepare spatial & temporal - spatial resolution ########################
spres <- quotes_long[quotes_long$code_group == "spatial & temporal - spatial resolution [m2]",]
spres$name <- as.numeric(as.character(spres$name))
spres <- spres[!is.na(spres$code_group),]

spres_sum <- spres %>% group_by(Document, name) %>% count(name)
spres_sum$n <- 1
spres_sum$name <- as.numeric(as.character(spres_sum$name))
spres_sum <- spres_sum[order(spres_sum$name),]
#spext_sum <- spext_sum %>% group_by(name) %>% count(name)

quant_n <- length(unique(spres$Document))

dc_spres <- ggdotchart(spres_sum, x = "Document", y = "name",
                       add = "segments",
                       add.params = list(color = "black"),
                       sorting = "ascending") +
  yscale("log10", .format = TRUE) +
  ylab("spatial resolution (m2)") + xlab("Reviewed paper") +
  bgcolor("lightpink") +
  border("#BFD5E3") +
  ggtitle(paste(quant_n, "studies out of", n_studies))

dc_spres

rm(list = c("spres", "spres_sum", "quant_n"))
###################### prepare spatial & temporal - spatial resolution ########################

###################### prepare spatial & temporal - temporal extent ########################
tmext <- quotes_long[quotes_long$code_group == "spatial & temporal - temporal extent [d]",]
tmext$name <- as.numeric(as.character(tmext$name))
tmext <- tmext[!is.na(tmext$code_group),]

tmext_sum <- tmext %>% group_by(Document, name) %>% count(name)
tmext_sum$n <- 1
tmext_sum <- tmext %>% group_by(Document, name) %>% count(name)
tmext_sum$name <- as.numeric(as.character(tmext_sum$name))
tmext_sum$logname <- log(as.numeric(as.character(tmext_sum$name)))
tmext_sum <- tmext_sum[order(tmext_sum$name),]

quant_n <- length(unique(tmext$Document))
tm_range <- c(min(tmext_sum$name), max(tmext_sum$name))

dc_tmext <- ggdotchart(tmext_sum, x = "Document", y = "name",
           add = "segments",
           add.params = list(color = "black"),
           sorting = "ascending") +
  yscale("log10", .format = TRUE) +
  ylab("temporal extent (days)") + xlab("Reviewed paper") +
  bgcolor("lightpink") +
  border("#BFD5E3") +
  ggtitle(paste(quant_n, "studies out of", n_studies)) +

dc_tmext

rm(list = c("tmext", "tmext_sum", "quant_n"))
###################### prepare spatial & temporal - spatial extent ########################

###################### prepare spatial & temporal - temporal resolution ########################
tmres <- quotes_long[quotes_long$code_group == "spatial & temporal - temporal resolution [d]",]
tmres$name <- as.numeric(as.character(tmres$name))
tmres <- tmres[!is.na(tmres$code_group),]

tmres_sum <- tmres %>% group_by(Document, name) %>% count(name)
tmres_sum$n <- 1
tmres_sum$name <- as.numeric(as.character(tmres_sum$name))
tmres_sum <- tmres_sum[order(tmres_sum$name),]
#spext_sum <- spext_sum %>% group_by(name) %>% count(name)

quant_n <- length(unique(tmres$Document))

dc_tmres <- ggdotchart(tmres_sum, x = "Document", y = "name",
                       add = "segments",
                       add.params = list(color = "black"),
                       sorting = "ascending") +
  yscale("log10", .format = TRUE) +
  ylab("temporal resolution (days)") + xlab("Reviewed paper") +
  bgcolor("lightpink") +
  border("#BFD5E3") +
  ggtitle(paste(quant_n, "studies out of", n_studies))

dc_tmres

rm(list = c("tmres", "tmres_sum", "quant_n"))
###################### prepare spatial & temporal - temporal resolution ########################

###################### prepare food system - echelon ########################
###################### prepare food system - echelon ########################

##MESSY##

# model type and model domain
colnames(model_type_model_domain)[1] <- "model_type"
model_type_model_domain <- away_gr(sheet = model_type_model_domain)
model_type_model_domain <- away_codegr_row(sheet = model_type_model_domain, codegr = "^.*(per model - type: )")
model_type_model_domain <- away_codegr(sheet = model_type_model_domain, codegr = "^.*(per model - domain: )") #^.*() ensures bullet point in front of codegroup is also removed
model_type_model_domain <- away_spaces(sheet = model_type_model_domain)
model_type_model_domain_long <- gather(model_type_model_domain, "domain", "cooc", 2:ncol(model_type_model_domain), factor_key = TRUE)

# all quotes
quotes <- quotes[quotes$`Document Groups` == "!Read for Lit review - eligible",]
agent_codes <- grepl("agent - representation:", quotes$Codes) | grepl("agent - paradigm:", quotes$Codes) | grepl("agent - method:", quotes$Codes) | grepl("agent - theory:", quotes$Codes) | grepl("per agent - ability", quotes$Codes) | grepl("per agent - adaptation", quotes$Codes) | grepl("per agent - agent", quotes$Codes) | grepl("per agent - characteristic", quotes$Codes) | grepl("per agent - decision", quotes$Codes) | grepl("per agent - echelon", quotes$Codes) | grepl("per agent - heterogeneity", quotes$Codes) | grepl("per agent - learning", quotes$Codes) | grepl("per agent - number", quotes$Codes) | grepl("per effect - affected agent", quotes$Codes) | grepl("per interaction - interaction", quotes$Codes) | grepl("per interaction - other agent", quotes$Codes) | grepl("per interaction - exchange", quotes$Codes)
agent_quotes <- quotes[agent_codes, ]

agents <- quotes[grepl("(per agent - agent:)", quotes$Codes),] 
agents$code <- NA

agents_long <- agents[1,]
agents_long$code <- NA
agents_long[,] <- NA

for(i in 1:nrow(agents)){
  codes <- agents$Codes[i]
  codes_vec <- unlist(strsplit(codes, "\r\n"))
  for(j in codes_vec){
    new_row <- agents[i,]
    new_row$code <- j
    agents_long <- rbind(agents_long, new_row)
  }
}

agents_long <- agents_long[-1,]

agents_long <- agents_long[grepl("per agent - agent:", agents_long$code),]
agents_long <- away_codegr_row(sheet = agents_long, codegr = "^(per agent - agent: )", clm = 14)
agents_sum <- agents_long %>% group_by(Document) %>% count(code)
agents_sum$n <- 1
agents_sum <- agents_sum %>% group_by(code) %>% count(code)

agents_sum$class <- ""
#agents_sum$n_code <- 0
agent_class <- dict_classification(sheet = agents_sum, dct = agent_types, clm = 1, class_clm = 3)
#agent_class$clm_width <- 1/agent_class$n_code

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

###################### process data ########################

# merge ref_scales and model_types
ref_scale_model_type_long <- merge(ref_scale_long, model_types_long, by = "Papers") #total <- merge(data frameA,data frameB,by="ID")
for(i in 1:nrow(ref_scale_model_type_long)){ref_scale_model_type_long$cooc[i] = min(ref_scale_model_type_long$`mentioned?.x`[i], ref_scale_model_type_long$`mentioned?.y`[i])}

ref_scale_model_type_sum <- ref_scale_model_type_long %>% 
  group_by(scale, type) %>%
  summarise(cooc = sum(cooc))

ref_scale_model_type_sum$scale <- factor(ref_scale_model_type_sum$scale, levels = (c("earth\r\n", 
                                                               "earth_>_x_>_continent\r\n", 
                                                               "continent\r\n", 
                                                               "continent_>_x_>_country\r\n", 
                                                               "country\r\n", 
                                                               "country_>_x_>_province/state\r\n",
                                                               "province/state\r\n",
                                                               "province/state_>_x_>_municipality\r\n",
                                                               "municipality\r\n",
                                                               "municipality_>_x_>_city\r\n",
                                                               "city\r\n",
                                                               "city_>_x_>_city_district\r\n",
                                                               "city_district\r\n")))

# interaction data from quotes_long
intract <- quotes_long[quotes_long$code_group %in% c("per agent - agent", "per interaction - other agent", "per interaction - exchange"),]
intract_w <- spread(intract, code_group, name)
agent_w <- intract_w[,c(1,15)]
other_w <- intract_w[,c(1,17)]
exchange_w <- intract_w[,c(1,7,16)]

intract_m <- merge(agent_w, other_w,  by = "ID")
intract_m <- merge(intract_m, exchange_w,  by = "ID")
intract_m <- na.omit(intract_m)
colnames(intract_m) <- c("ID", "agent", "other", "codes", "exchange")
intract_m <- intract_m[-which(intract_m$agent == "not applicable" | intract_m$exchange == "not applicable"| intract_m$other == "not applicable"),]
intract_source <- intract_m %>% group_by(agent, exchange) %>% count(agent, exchange)
intract_source$sender <- paste("sender:", intract_source$agent)
intract_target <- intract_m %>% group_by(exchange, other) %>% count(exchange, other)
intract_target$target <- paste("receiver:", intract_target$other)

# goals from quotes_long
goals <- quotes_long[quotes_long$code_group == "per measure - objective",]
goals <- goals[!is.na(goals$name),]
goals <- goals %>% group_by(Document) %>% count(name)
goals$n <- 1
goals <- goals %>% group_by(name) %>% count(name)
goals$class <- ""
goals$n_of_class <- 0
goals <- dict_classification(sheet = goals, dct = goals_class, clm = 1, class_clm = 3)

# governance measures from quotes_long
measures <- unique(quotes_long$code[grepl("per measure - measure: ", quotes_long$code)])
measures <- gsub("per measure - measure: ", "", measures)

governance <- quotes_long[quotes_long$code_group %in% c("per measure - objective",
                                                        "per measure - measure",
                                                        "per effect - effect"),]
governance_w <- spread(governance, code_group, name)
objective_w <- na.omit(governance_w[,c(1,14)])
objective_w$objective_class <- ""
objective_w <- dict_classification(sheet = objective_w, dct = goals_class, clm = 2, class_clm = 3)

measure_w <- na.omit(governance_w[,c(1,7,13)])
measure_w$measure_class <- ""
measure_w <- dict_classification(sheet = measure_w, dct = measure_class, clm = 3, class_clm = 4)

effect_w <- na.omit(governance_w[,c(1,12)])

governance_om <- merge(objective_w, measure_w,  by = "ID")
governance_me <- merge(measure_w, effect_w,  by = "ID")
governance_om <- na.omit(governance_om)
governance_me <- na.omit(governance_me)
colnames(governance_om) <- c("ID", "objective", "objective_class", "codes", "measure", "measure_class")
colnames(governance_me) <- c("ID", "codes", "measure", "measure_class", "effect")

#governance_source <- governance_om %>% group_by(ID) %>% count(objective, measure)
#governance_source$n <- 1
governance_source <- governance_om %>% group_by(objective_class, measure_class) %>% count(objective_class, measure_class)

#governance_target <- governance_me %>% group_by(ID) %>% count(measure, effect)
#governance_target$n <- 1
#governance_target <- governance_target %>% group_by(measure, effect) %>% count(measure, effect)
governance_target <- governance_me %>% group_by(measure_class, effect) %>% count(measure_class, effect)

# echelon vs scale of governance
head(quotes_long)
ech <- quotes_long[quotes_long$code_group %in% c("food system - echelon"),]
sc <- quotes_long[quotes_long$code_group %in% c("spatial & temporal - ref scale"),]

ech_sum <- ech %>% group_by(Document, name) %>% count(name)
ech_sum$n <- 1

ech_sum <- ech_sum %>% group_by(name) %>% count(name)
formattable(ech_sum[order(-ech_sum$n),], list(`n` = color_bar("#71CA97")))

sc_sum <- sc %>% group_by(Document, name) %>% count(name)
sc_sum$n <- 1

sc_sum <- sc_sum %>% group_by(name) %>% count(name)
unique(sc_sum$name)
sc_sum$name <- factor(sc_sum$name, levels = rev(c("earth", 
                                               "earth > x > continent", 
                                              "continent", 
                                                                  "continent > x > country", 
                                                                  "country", 
                                                                  "country > x > province/state",
                                                                  "province/state",
                                                                  "province/state > x > municipality",
                                                                  "municipality",
                                                                  "municipality > x > city",
                                                                  "city",
                                                                  "city > x > city_district",
                                                                  "city district")))

formattable(sc_sum[order(sc_sum$name),], list(`n` = color_bar("#71CA97")))

msc <- quotes_long[quotes_long$code_group %in% c("per measure - scale"),]

msc_sum <- msc %>% group_by(Document, name) %>% count(name)
#msc_sum$n <- 1

msc_sum <- msc_sum %>% group_by(name) %>% count(name)
unique(msc_sum$name)
msc_sum$name <- factor(msc_sum$name, levels = rev(c("unclear",
                                                    "earth", 
                                                  "earth > x > continent", 
                                                  "continent", 
                                                  "continent > x > country", 
                                                  "country", 
                                                  "country > x > province/state",
                                                  "province/state",
                                                  "province/state > x > municipality",
                                                  "municipality",
                                                  "municipality > x > city",
                                                  "city",
                                                  "city > x > city_district",
                                                  "city district")))

formattable(msc_sum[order(msc_sum$name),], list(`n` = color_bar("#71CA97")))

###################### visualise data ###################### 

#data types
png(paste0(figdir, "/data_types.png"), width = 640, height = 580)
ggplot(modelling_data_sum, aes(fill=data_class, y=number, x = data)) +
  geom_bar(position = "stack", stat = "identity", colour = "black") + 
  scale_fill_manual(name = "Data class", values = c("#FEF445", "#CEE741", "#8FD14F", "#FAc710")) + 
  ggtitle(paste("Total number of studies =", length(unique(modelling_data$Papers)))) +
  ylab("Number of studies") +
  xlab("Type of data") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.6)), 
        axis.text.y = element_text(size = rel(1.6)), 
        axis.title = element_text(size=rel(1.4), face="bold"),
        legend.title = element_text(size = rel(1.3), face = "bold"), 
        legend.position = c(0.8, 0.2),
        legend.text = element_text(size = rel(1.4)),
        title = element_text(size = rel(1.4))) + 
  scale_y_continuous(breaks = seq(0,15,3)) +
  coord_flip()
dev.off()

#model types
png(paste0(figdir, "/model_types.png"), width = 640, height = 580)
ggplot(model_types_sum, aes(fill=type, y=number, x = type)) +
  geom_bar(position = "stack", stat = "identity", colour = "black") + 
  scale_fill_manual(name = "Data class", values = c("#2D9BF0", "#12CDD4", "#0CA789", "#652CB3", "#0CA789", "#DA0063", "#9510AC", "#12CDD4", "#414BB2")) + 
  ggtitle(paste("Total number of studies =", length(unique(model_types$Papers)))) +
  ylab("Number of studies") +
  xlab("Type of model") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.6)), 
        axis.text.y = element_text(size = rel(1.6)), 
        axis.title = element_text(size=rel(1.4), face="bold"),
        legend.title = element_text(size = rel(1.3), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4)),
        title = element_text(size = rel(1.4))) + 
  scale_y_continuous(breaks = seq(0,15,3)) +
  coord_flip()
dev.off()

#ref scales
png(paste0(figdir, "/ref_scales.png"), width = 640, height = 580)
ggplot(ref_scale_sum, aes(fill=scale, y=number, x = scale)) +
  geom_bar(position = "stack", stat = "identity", colour = "black") + 
  scale_fill_manual(name = "Data class", values = rev(c("#EA2727", "#EA2775", "#EA27C9", "#BD27EA", "#8527EA", "#4D27EA", "#274FEA", "#2795EA", "#27B9EA", "#27EAEA", "#27EAC0", "#27EA6B", "#85EA27"))) + 
  ggtitle(paste("Total number of studies =", length(unique(ref_scale$Papers)))) +
  ylab("Number of studies") +
  xlab("Spatial scale") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.6)), 
        axis.text.y = element_text(size = rel(1.6)), 
        axis.title = element_text(size=rel(1.4), face="bold"),
        legend.title = element_text(size = rel(1.3), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4)),
        title = element_text(size = rel(1.4))) + 
  scale_y_continuous(breaks = seq(0,15,3)) +
  coord_flip()
dev.off()

#model type x ref scales
png(paste0(figdir, "/model_type_x_ref_scales.png"), width = 740, height = 580)
ggplot(ref_scale_model_type_sum, aes(x = scale, y = type)) +
  geom_point(aes(size = cooc)) +
  ggtitle(paste("Total number of studies =", length(unique(ref_scale_model_type_long$Papers)))) +
  ylab("Model types") +
  xlab("Spatial scale") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.6), angle = 90), 
        axis.text.y = element_text(size = rel(1.6)), 
        axis.title = element_text(size=rel(1.4), face="bold"),
        legend.title = element_text(size = rel(1.3), face = "bold"), 
        #legend.position = "none",
        legend.text = element_text(size = rel(1.4)),
        title = element_text(size = rel(1.4))) + 
  #scale_y_continuous(breaks = seq(0,15,3)) +
  coord_flip()
dev.off()

#model type x model domain
png(paste0(figdir, "/model_type_x_model_domain.png"), width = 640, height = 580)
ggplot(model_type_model_domain_long, aes(x = domain, y = model_type)) +
  geom_point(aes(size = cooc)) +
  ggtitle(paste("Total number of studies =", length(unique(ref_scale_model_type_long$Papers)))) +
  ylab("Model types") +
  xlab("Model domain") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.6), angle = 90), 
        axis.text.y = element_text(size = rel(1.6)), 
        axis.title = element_text(size=rel(1.4), face="bold"),
        legend.title = element_text(size = rel(1.3), face = "bold"), 
        #legend.position = "none",
        legend.text = element_text(size = rel(1.4)),
        title = element_text(size = rel(1.4))) + 
  #scale_y_continuous(breaks = seq(0,15,3)) +
  coord_flip()
dev.off()

# agent types
levels(agent_class$code)
agent_class$code <- factor(as.character(agent_class$code), levels = c(agent_types[["not applicable"]],
                                                                      agent_types[["overarching agent types"]],
                                                                      agent_types[["other type of agents"]],
                                                                      agent_types[["farmer-type agents"]],
                                                                      agent_types[["distribution-type agents"]],
                                                                      agent_types[["retailer-type agents"]],
                                                                      agent_types[["consumer-type agents"]]))

agent_class$class <- factor(as.character(agent_class$class), levels = c("not applicable",
                                                                        "overarching agent types",
                                                                        "other type of agents",
                                                                        "farmer-type agents",
                                                                        "distribution-type agents",
                                                                        "retailer-type agents",
                                                                        "consumer-type agents"))

agent_class <- agent_class[order(agent_class$code, agent_class$class),]



png(paste0(figdir, "/agent_types.png"), width = 900, height = 700)
ggplot(agent_class, aes(fill=code, y=n, x = class)) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity", colour = "black") + 
  geom_text(aes(label = code, group = class), 
            position = position_dodge2(width = 0.9, preserve = "single"), vjust=0.4, hjust = -0.1, angle = 0, size = rel(5)) +
  #scale_fill_manual(name = "Data class", values = rev(c("#EA2727", "#EA2775", "#EA27C9", "#BD27EA", "#8527EA", "#4D27EA", "#274FEA", "#2795EA", "#27B9EA", "#27EAEA", "#27EAC0", "#27EA6B", "#85EA27"))) + 
  ggtitle(paste("Total number of studies =", length(unique(quotes[quotes$`Document Groups`== "!Read for Lit review - eligible",]$Document)))) +
  ylab("Number of studies") +
  expand_limits(y = 7) +
  xlab("Agent type") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.6)), 
        axis.text.y = element_text(size = rel(1.6)), 
        axis.title = element_text(size=rel(1.4), face="bold"),
        legend.title = element_text(size = rel(1.3), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4)),
        title = element_text(size = rel(1.4))) + 
  scale_y_continuous(breaks = seq(0,10,2)) +
  coord_flip()
dev.off()

#agent interaction sankey diagram

#intract_target$target <- paste("receiver:", intract_target$target)
colnames(intract_source) <- c("agent",  "target", "value", "source")
colnames(intract_target) <- c("source", "other", "value", "target")
links <- rbind(intract_source, intract_target)
colnames(links)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

library(networkD3)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE, fontSize = 10)
p

other_agent <- intract[intract$code_group == "per interaction - other agent",]
View(other_agent)

other_agent_sum <- other_agent %>% count(Document, name)
other_agent_sum$n <- 1

other_agent_sum <- other_agent %>% count(name)

# goals
levels(goals$name)
goals$name <- factor(as.character(goals$name), levels = c(goals_class[["availability"]],
                                                                goals_class[["accessibility"]],
                                                                goals_class[["utilisation"]],
                                                                goals_class[["stability"]],
                                                                goals_class[["environmental"]],
                                                                goals_class[["economic"]],
                                                                goals_class[["logistics"]],
                                                                goals_class[["infrastructure & technology"]],
                                                                goals_class[["health & wellbeing"]],
                                                          goals_class[["other"]]))

goals$class <- factor(as.character(goals$class), levels = c("availability",
                                                                        "accessibility",
                                                                        "utilisation",
                                                                        "stability",
                                                                        "environmental",
                                                                        "economic",
                                                                        "logistics",
                                                            "infrastructure & technology",
                                                            "health & wellbeing",
                                                            "other"))

goals <- goals[order(goals$name, goals$class),]



png(paste0(figdir, "/goal_types.png"), width = 900, height = 1000)
ggplot(goals, aes(fill=name, y=n, x = class)) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity", colour = "black") + 
  geom_text(aes(label = name, group = class), 
            position = position_dodge2(width = 0.9, preserve = "single"), vjust=0.4, hjust = -0.1, angle = 0, size = rel(5)) +
  #scale_fill_manual(name = "Data class", values = rev(c("#EA2727", "#EA2775", "#EA27C9", "#BD27EA", "#8527EA", "#4D27EA", "#274FEA", "#2795EA", "#27B9EA", "#27EAEA", "#27EAC0", "#27EA6B", "#85EA27"))) + 
  ggtitle(paste("Total number of studies =", length(unique(quotes[quotes$`Document Groups`== "!Read for Lit review - eligible",]$Document)))) +
  ylab("Number of studies") +
  expand_limits(y = 5) +
  xlab("Goals") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.6)), 
        axis.text.y = element_text(size = rel(1.6)), 
        axis.title = element_text(size=rel(1.4), face="bold"),
        legend.title = element_text(size = rel(1.3), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4)),
        title = element_text(size = rel(1.4))) + 
  scale_y_continuous(breaks = seq(0,10,2)) +
  coord_flip()
dev.off()

n_reviewed <- length(unique(quotes[quotes$`Document Groups`== "!Read for Lit review - eligible",]$Document))

#
library(gt)
library(tidyverse)
library(glue)

goals$name <- as.character(goals$name)
goals <- goals[,1:3]
colnames(goals) <- c("goals", "n_measures", "class")
m <- max(goals$n_measures)

goals %>%
  group_by(class) %>%
  arrange(class,desc(n_measures)) %>%
  gt() %>%
  tab_header(
    title = "Goals of governance measures",
    subtitle = paste("Gathered from", n_reviewed, "papers")
  ) %>% #tab_row_group(
    #group = "class"
  #) %>%
  fmt_passthrough( # Not sure about this but it works...
    columns = vars(class) # First column: supp (character)
  ) %>%
  # data_color( # Update cell colors...
  #   columns = vars(class), # ...for supp column!
  #   colors = scales::col_factor( # <- bc it's a factor
  #     palette = c(
  #       "chocolate1","chocolate3", "coral1", "coral3", "chartreuse3", "cyan", "azure1", "azure3", "blueviolet", "brown1"), # Two factor levels, two colors
  #     domain = unique(goals$class)# Levels
  #   )) %>% 
  data_color( # Update cell colors...
    columns = vars(n_measures), # ...for dose column 
    colors = scales::col_numeric( # <- bc it's numeric
      palette = c(
        "gray90","gray50"), # A color scheme (gradient)
      domain = c(0,m) # Column scale endpoints
    )) %>% tab_options(row_group.background.color = "yellow")
  
formattable(goals[,1:2])

#


# governance sankey diagram
#intract_target$target <- paste("receiver:", intract_target$target)
colnames(governance_source) <- c("source", "target", "value")
colnames(governance_target) <- c("source", "target", "value")
links <- rbind(governance_source, governance_target)
colnames(links)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE, fontSize = 10)
p
