#this is a cleaner version of atlas_analysis.R
#initiated: 09.08.2021
#finished:
#author: Aleid Teeuwen
#project: The impact of governance on food security: a systematic review of simulation models

rm(list = ls()) #start with a clean environment

###################### load libraries ###################### 
library("readxl") #for reading excel files
library("tidyr") #for data processing 
library("dplyr") #for data processing 
library("ggplot2") #for visualisation
library("RColorBrewer") #for color pallettes
library("formattable") #for making ncie tables #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
library("kableExtra") #for making nice tables #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
library("networkD3") #for sankey diagrams
library("htmltools") #to manage html objects
library("webshot") #to save screenshots of html objects
library("ggpubr") #to make nice figures
library("magick") # image formatting
library("sparkline") #making nice tables
library("grid") #to place text in plots
library("VennDiagram") #to make venn diagrams
library("reshape2") #data processing
library("igraph") #to make network graphs
library("visNetwork") #to make network graphs
###################### load libraries ###################### 


###################### define paths for import and export ###################### 
datadir <- "../Atlas_export_sheets"
figdir <- "../13042021_Figures"
###################### define paths for import and export ###################### 


###################### source functions ######################  
source("./functions/away_codegr.R") #!! check which functions are really used
source("./functions/away_gr.R")
source("./functions/away_spaces.R")
source("./functions/away_totals.R")
source("./functions/dict_classification.R") 
source("./functions/level1_count.R")
source("./functions/level2_count.R")
source("./functions/level2_summ.R")
source("./functions/level2_class_summ.R")
source("./functions/check_dictionary.R")
source("./functions/make_cooc_doc.R")
source("./functions/reverse_dict.R")
###################### source functions ######################


###################### source dictionaries ######################  
source("./dictionaries/goals_class.R") #!! check which dictionaries are really used
source("./dictionaries/measure_class.R")
source("./dictionaries/comm_class.R")
source("./dictionaries/FSi_class.R")
source("./dictionaries/timpl_class.R")
source("./dictionaries/NOTA_class.R")
source("./dictionaries/NOTA_subclass.R")
###################### source dictionaries ######################


###################### load data ###################### 
quotes <- read_excel(paste0(datadir, "/", "all_quotes.xlsx"))
###################### load data ###################### 


###################### show progress ###################### 
eligible <- length(unique(quotes[quotes$`Document Groups`=="!Read for Lit review - eligible",]$Document))
eligible
ineligible <- length(unique(quotes[quotes$`Document Groups`=="!Read for Lit review - ineligible",]$Document))
ineligible
read <- eligible + ineligible
read
to_read <- length(unique(quotes[quotes$`Document Groups`=="!To read for Lit review",]$Document))
to_read
progress <- round(100*read/(read+to_read), 1)
print(paste("progress:", progress, "%"))

rm(list = c("eligible", "ineligible", "read", "to_read", "progress"))
###################### show progress ###################### 


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

###################### check whether dictionaries are up to date ########################
check_dictionary(codegroup = "food system - commodity", codedictionary = comm_class, dat_long = quotes_long)
check_dictionary(codegroup = "per effect - FS indicator", codedictionary = FSi_class, dat_long = quotes_long)
check_dictionary(codegroup = "per measure - objective", codedictionary = goals_class, dat_long = quotes_long) # to do: split stability into economic and physical
check_dictionary(codegroup = "per measure - measure", codedictionary = undir_gov, dat_long = quotes_long)
quotes_long$class <- ""
quotes_long2 <- dict_classification(sheet = quotes_long, dct = undir_gov, clm = 16, class_clm = 17)
quotes_long2$name[which(quotes_long2$code_group == "per measure - measure")] <- quotes_long2$class[which(quotes_long2$code_group == "per measure - measure")]
check_dictionary(codegroup = "per measure - measure", codedictionary = NOTA_subclass, dat_long = quotes_long2)
check_dictionary(codegroup = "per measure - target implementer", codedictionary = timpl_class, dat_long = quotes_long)
check_dictionary(codegroup = "per agent - agent", codedictionary = timpl_class, dat_long = quotes_long)
check_dictionary(codegroup = "per effect - affected agent", codedictionary = timpl_class, dat_long = quotes_long)
###################### check whether dictionaries are up to date ########################

###################### FIGURES ########################
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
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")


ft_year %>% as_image(width = 22, file = paste0(figdir, "/papers_year_table.png"))

rm(list = c("year", "year_sum", "y_range"))
###################### prepare papers - year ########################

###################### prepare papers - journal ########################
jrnl <- quotes_long[quotes_long$code_group == "papers - journal",]
jrnl <- jrnl[!is.na(jrnl$code_group),]
jrnl_sum <- level1_count(sheet = jrnl)

jrnl_sum$name <- factor(jrnl_sum$name, levels = rev(unique(jrnl_sum$name)))
colnames(jrnl_sum) <- c("journal", "number")
jrnl_sum$proportion <- round(jrnl_sum$number/n_studies, 2)

jrnl_sum$number <- color_bar("lightgreen")(jrnl_sum$number)

ft_jrnl <- jrnl_sum[1:20,] %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Top 20. Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_jrnl %>% as_image(width = 22, file = paste0(figdir, "/papers_journal_top20_table.png"))

rm(list = c("jrnl", "jrnl_sum"))
###################### prepare papers - journal ########################


###################### prepare modelling - coupling? ########################
coup <- quotes_long[quotes_long$code_group == "modelling - coupling?",]
coup <- coup [!is.na(coup $code_group),]
coup_sum <- level1_count(sheet = coup )

coup_sum$name <- factor(coup_sum$name, levels = c("yes", "no", "no, just one"))
coup_sum <- coup_sum[order(coup_sum$name),]

colnames(coup_sum) <- c("model coupling?", "number")
coup_sum$proportion <- round(coup_sum$number/n_studies, 2)

coup_sum$number <- color_bar("lightblue")(coup_sum$number)

ft_coup <- coup_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_coup %>% as_image(width = 22, file = paste0(figdir, "/modelling_coupling_table.png"))

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
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_aim %>% as_image(file = paste0(figdir, "/modelling_aim_table.png"))

rm(list = c("aim", "aim_sum"))
###################### prepare modelling - aim ########################


###################### prepare modelling - feedback-loops? ########################
loop <- quotes_long[quotes_long$code_group == "modelling - feedback-loop?",]
loop <- loop[!is.na(loop$code_group),]
loop_sum <- level1_count(sheet = loop)

loop_sum$name <- factor(loop_sum$name, levels = c("yes", "no"))
loop_sum <- loop_sum[order(loop_sum$name),]

colnames(loop_sum) <- c("feedback loops in model?", "number")
loop_sum$proportion <- round(loop_sum$number/n_studies, 2)

loop_sum$number <- color_bar("lightblue")(loop_sum$number)

ft_loop <- loop_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_loop %>% as_image(width = 22, file = paste0(figdir, "/modelling_feedback-loop_table.png"))

rm(list = c("loop", "loop_sum"))
###################### prepare modelling - feedback-loops? ########################


###################### prepare modelling - sensitivity analysis? ########################
sens <- quotes_long[quotes_long$code_group == "modelling - sensitivity analysis?",]
sens <- sens[!is.na(sens$code_group),]
sens_sum <- level1_count(sheet = sens)

sens_sum$name <- factor(sens_sum$name, levels = c("yes", "no"))
sens_sum <- sens_sum[order(sens_sum$name),]

colnames(sens_sum) <- c("sensitivity analysis in model?", "number")
sens_sum$proportion <- round(sens_sum$number/n_studies, 2)

sens_sum$number <- color_bar("lightblue")(sens_sum$number)

ft_sens <- sens_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_sens %>% as_image(width = 22, file = paste0(figdir, "/modelling_sensitivity-analysis_table.png"))

rm(list = c("sens", "sens_sum"))
###################### prepare modelling - sensitivity analysis? ########################

###################### prepare modelling - validation? ########################
val <- quotes_long[quotes_long$code_group == "modelling - validation?",]
val <- val[!is.na(val$code_group),]
val_sum <- level1_count(sheet = val)

val_sum$name <- factor(val_sum$name, levels = c("yes", "no"))
val_sum <- val_sum[order(val_sum$name),]

colnames(val_sum) <- c("model validated?", "number")
val_sum$proportion <- round(val_sum$number/n_studies, 2)

val_sum$number <- color_bar("lightblue")(val_sum$number)

ft_val <- val_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_val %>% as_image(width = 22, file = paste0(figdir, "/modelling_validation_table.png"))

rm(list = c("val", "val_sum"))
###################### prepare modelling - validation? ########################


###################### prepare modelling - sensitivity analysis and/or validation? ########################
sens <- quotes_long[quotes_long$code_group %in% c("modelling - sensitivity analysis?", "modelling - validation?"),]
sens <- sens[!is.na(sens$code_group),]

data_sum <- sens %>% 
  group_by(code_group, name, Document) %>%
  count(code_group, name)

data_sum$n <- 1

data_wide <- spread(data_sum, code_group, name)
colnames(data_wide)[3:4] <- c( "sensitivity", "validation")


data_sum <- data_wide %>% 
  group_by(sensitivity, validation) %>%
  count(sensitivity, validation)

data_sum$proportion <- round(data_sum$n/n_studies, 2)

data_sum$n <- color_bar("lightblue")(data_sum$n)

ft_seval <- data_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(n) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_seval %>% as_image(width = 22, file = paste0(figdir, "/modelling_feedback-sensitivity-&-validation_table.png"))

rm(list = c("sens", "data_wide", "data_sum"))
###################### prepare modelling - sensitivity analysis and/or validation? ########################


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
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_data %>% as_image(width = 22, file = paste0(figdir, "/modelling_data_table.png"))

modelling_data$primary <- sub("\\).*", "", sub(".*\\(", "", modelling_data$name)) 
modelling_data$name <- gsub("\\s*\\([^\\)]+\\)","", modelling_data$name)

modelling_data <- modelling_data %>% 
  group_by(primary, name, Document) %>%
  count(primary, name)

modelling_data$n <- 1

colnames(modelling_data)[1:2] <- c("primary or secondary", "data type")

modelling_data_sum <- modelling_data %>% 
  group_by(`data type`, `primary or secondary`) %>%
  count(`data type`, `primary or secondary`)

modelling_data_spread <- modelling_data_sum %>% spread(`primary or secondary`, n)
modelling_data_spread[is.na(modelling_data_spread)] <- 0

modelling_data_spread$primary <- color_bar("lightblue")(modelling_data_spread$primary)
modelling_data_spread$secondary <- color_bar("lightblue")(modelling_data_spread$secondary)

mo_data_prsec <- modelling_data_spread %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(primary, secondary) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

mo_data_prsec %>% as_image(width = 22, file = paste0(figdir, "/modelling_data-type-&-primarysecondary_table.png"))

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
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_mtyp %>% as_image(width = 22, file = paste0(figdir, "/per-model_type_table.png"))

rm(list = c("mtype", "mtype_sum"))

# Co-occurence network graph
gov <- quotes_long[grep("modelling - coupling?: yes", quotes_long$Codes, fixed = TRUE),]
n_combined <- length(unique(gov$Document))

gov <- gov[gov$code_group == "per model - type",]
gov <- gov[!is.na(gov$code_group),]
gov <- gov[!is.na(gov$name),]

gov$class <- ""
#gov <- dict_classification(sheet = gov, dct = measure_class, clm = 16, class_clm = 17)

gov_sum <- gov %>% 
  group_by(ID, name) %>%
  count(ID, name)

V <- crossprod(table(gov_sum[c(1,2)]))
diag(V) <- 0 #figure out how to do it properly with selfcooc$n
V

Vdat <- as.data.frame(V)
colnames(Vdat) <- gsub("\\s*\\([^\\)]+\\)","", colnames(Vdat)) #remove parentheses because they cause problems later
rownames(Vdat) <- gsub("\\s*\\([^\\)]+\\)","", rownames(Vdat))
colnames(Vdat) <- trimws(colnames(Vdat), which = c("both"), whitespace = "[ \t\r\n]") #remove white spaces at the beginning and end because they cause problems later 
rownames(Vdat) <- trimws(rownames(Vdat), which = c("both"), whitespace = "[ \t\r\n]")
colnames(Vdat) <- gsub(" ","_", colnames(Vdat)) #remove spaces because they cause problems later
rownames(Vdat) <- gsub(" ","_", rownames(Vdat))
Vdat$measures1 <- rownames(Vdat)

Vlong <- melt(Vdat, id.vars = c("measures1"))
colnames(Vlong)[2] <- "measures2"

Vnodes <- as.data.frame(cbind(unique(Vlong$measures1), unique(Vlong$measures1)))
colnames(Vnodes) <- c("id", "label")
Vedges <- Vlong
colnames(Vedges) <- c("from", "to", "width")

#Create graph for Louvain
graph <- graph_from_data_frame(Vedges, directed = FALSE)
#Louvain Comunity Detection
cluster <- cluster_louvain(graph)
cluster_df <- data.frame(as.list(membership(cluster)))
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$label <- rownames(cluster_df)
colnames(Vnodes) <- c("id", "label")

#Create group column
Vnodes <- left_join(Vnodes, cluster_df, by = "label")
colnames(Vnodes)[3] <- "group"
nodes <- Vnodes
edges <- Vedges

visNetwork(nodes, edges) %>%
  visOptions( highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction( navigationButtons = TRUE) %>%
  visPhysics( maxVelocity = 35)

# co-occurence table
x = 2
png(filename = paste0(figdir, "/modelling-coupled_model-types_co-occurence.png"), width = 1300, height = 1300)
ggplot(Vedges, aes(x = from, y = to, col = width, label = width)) +
  geom_point(aes(size = width)) +
  geom_text(col = "black", fontface = "bold", size = 10) +
  ggtitle(paste("Total number of studies =", n_studies, ". Studies with coupled models =", n_combined)) +
  ylab("Model type") +
  xlab("Co-occuring model type") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.4*x), angle = 90), 
        axis.text.y = element_text(size = rel(1.4*x)), 
        axis.title = element_text(size=rel(0.6*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_size(range = c(10,30), breaks = c(0:max(Vedges$width))) +
  scale_color_gradient(low = "lightblue", high = "blue") #+
dev.off()


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
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_mdom %>% as_image(width = 22, file = paste0(figdir, "/per-model_domain_table.png"))

rm(list = c("mdom", "mdom_sum"))
###################### prepare per model - domain ########################


###################### per model - domain co-occurence ########################
# first limit data to where the co-occurence of governance measures is specified
mdom <- quotes_long[quotes_long$code_group == "per model - domain",]
mdom <- mdom[!is.na(mdom$code_group),]
mdom <- mdom[!is.na(mdom$name),]
mdom_sum <- mdom %>% 
  group_by(Document, name) %>%
  count(Document, name)

mdom_sum$n <- 1

# venn diagram instead of co-occurence
economic <- mdom_sum[mdom_sum$name == "economic",]$Document
biophys <- mdom_sum[mdom_sum$name == "bio-physical",]$Document
social <- mdom_sum[mdom_sum$name == "social",]$Document
logistic <- mdom_sum[mdom_sum$name == "logistic",]$Document

myCol <- c("yellow", "deeppink", 
           "cyan", "gray20")

venn.diagram(
  x = list(economic, biophys, 
           social, logistic),
  category.names = c("Economic" , "Bio-physical" , 
                     "Social", "Logistic"),
  filename = '../13042021_Figures/modelling_domain_venn.png',
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
               0.085, 0.085),
  cat.fontfamily = "sans"# ,
  # rotation = 1
)

###################### per model - domain co-occurence ########################


###################### per model - domain x type co-occurence ########################
dat <- quotes_long[quotes_long$code_group %in% c("per model - domain", "per model - type"),]
dat <- dat[!is.na(dat$code_group),]
dat <- dat[!is.na(dat$name),]
dat_sum <- dat %>% 
  group_by(Document, ID, code_group, name) %>%
  count(Document, ID, code_group, name)

dat_sum <- dat_sum[,-which(colnames(dat_sum)=="n")]
str(dat_sum)
dat_sum <- pivot_wider(data = dat_sum, id_cols = c(Document, ID), names_from = code_group, values_from = name, values_fn = list)

dat_cooc <- dat_sum[1,]
dat_cooc[,3:4] <- ""
str(dat_cooc[,3:4])

for(i in 1:nrow(dat_sum)){
  doms <- unlist(dat_sum$`per model - domain`[i])
  if(is.null(doms)){next
  }else{
    typs <- unlist(dat_sum$`per model - type`[i])
    if(is.null(typs)){next
    }else{
      for(j in 1:length(doms)){
        for(k in 1:length(typs)){
          newrow <- dat_cooc[i,]
          newrow$`per model - domain` <- doms[j]
          newrow$`per model - type` <- typs[k]
          newrow[,1:2] <- dat_sum[i,1:2]
          dat_cooc <- rbind(dat_cooc, newrow)
        }
      }
    } 
  }
}

dat_cooc <- dat_cooc[-1,] 

dat_cooc <- dat_cooc %>% 
  group_by(`per model - domain`, `per model - type`) %>%
  count(`per model - domain`, `per model - type`)

x <- 2

png(filename = paste0(figdir, "/per-model_type_domain_co-occurence.png"), width = 1300, height = 1300)
ggplot(dat_cooc, aes(y = `per model - domain`, x = `per model - type`, col = `per model - type`, label = n)) +
  geom_point(aes(size = n)) +
  geom_text(col = "black", fontface = "bold", size = 10) +
  #ggtitle(paste("Total number of studies =", n_studies, ". Studies with combined governances measures =", n_combined)) +
  ylab("Model type") +
  xlab("Model domain") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.4*x), angle = 90), 
        axis.text.y = element_text(size = rel(1.4*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_size(range = c(10,50), breaks = c(0:max(dat_cooc$n))) +
  #scale_color_gradient(low = "white", high = "red") +
  coord_flip()
dev.off()

rm(list = c("dat", "dat_sum", "dat_cooc", "x"))
###################### per model - domain x type co-occurence ########################

###################### prepare per model - subdomain ######################## 
mdom <- quotes_long[quotes_long$code_group == "per model - subdomain",] #!! here I would need to go through all again to paint a fair picture. some categories (like land market, which actually was modelled in a lot of models), have not been consistently coded
mdom <- mdom[!is.na(mdom$code_group),]
mdom_sum <- level1_count(sheet = mdom)
nota <- which(unique(mdom_sum$name) == "not specified")
mdom_sum$name <- factor(mdom_sum$name, levels = c(unique(mdom_sum$name)[-nota], "not specified"))
mdom_sum <- mdom_sum[order(mdom_sum$name),]
colnames(mdom_sum) <- c("model sub domains", "number")
mdom_sum$proportion <- round(mdom_sum$number/n_studies, 2)

mdom_sum$number <- color_bar("lightblue")(mdom_sum$number)

ft_msdom <- mdom_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_msdom %>% as_image(width = 22, file = paste0(figdir, "/per-model_subdomain_table.png"))

rm(list = c("mdom", "mdom_sum"))
###################### prepare per model - subdomain ########################

###################### prepare per model - subtype ########################
mdom <- quotes_long[quotes_long$code_group == "per model - subtype",]
mdom <- mdom[!is.na(mdom$code_group),]
mdom_sum <- level1_count(sheet = mdom)
nota <- which(unique(mdom_sum$name) == "not specified")
mdom_sum$name <- factor(mdom_sum$name, levels = c(unique(mdom_sum$name)[-nota], "not specified"))
mdom_sum <- mdom_sum[order(mdom_sum$name),]
colnames(mdom_sum) <- c("model subtype", "number")
mdom_sum$proportion <- round(mdom_sum$number/n_studies, 2)

mdom_sum$number <- color_bar("lightblue")(mdom_sum$number)

ft_msdom <- mdom_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_msdom %>% as_image(width = 22, file = paste0(figdir, "/per-model_subtype_table.png"))

rm(list = c("mdom", "mdom_sum", "nota"))
###################### prepare per model - subtype ########################


###################### prepare spatial & temporal - country ########################

#
# identify papers that represent all countries
to_fix <- which(quotes_long$name == "all countries fix in R")

#get list of all countries
library(maps)
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

#
cntr <- quotes_long[quotes_long$code_group == "spatial & temporal - country",]
cntr <- cntr[!is.na(cntr$code_group),]
cntr_sum <- level1_count(sheet = cntr)
cntr_sum$name <- factor(cntr_sum$name, levels = rev(unique(cntr_sum$name)))
colnames(cntr_sum) <- c("country", "number")
cntr_sum$proportion <- round(cntr_sum$number/n_studies, 2)

cntr_summ <- cntr_sum
cntr_sum$number <- color_bar("lightpink")(cntr_sum$number)

ft_cntr <- cntr_sum[1:20,] %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Top 20 countries, gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_cntr %>% as_image(width = 22, file = paste0(figdir, "/spatial&temporal_country-top20_table.png"))

rm(list = c("cntr", "cntr_sum"))

library("rworldmap")
cntr_summ <- as.data.frame(cntr_summ)
nmap <- joinCountryData2Map(cntr_summ, joinCode = "NAME", suggestForFailedCodes = TRUE, nameJoinColumn = "country", verbose=TRUE)


mycol <- scale_colour_gradientn(colours=c("brown", "purple"))

png(filename = paste0(figdir, "/spatial&temporal_country_map.png"), width = 1300, height = 800)
mapParams <- mapCountryData(nmap, 
                            nameColumnToPlot="number",
                            oceanCol = "azure",
                            catMethod = "fixedWidth",
                            missingCountryCol = gray(.8),
                            colourPalette = "topo",
                            addLegend = T,
                            mapTitle = "Number of studies",
                            border = NA,
                            numCats = max(cntr_summ$number))
# add legend and display map
# do.call(addMapLegend, c(mapParams,
#                              x = 'bottom',
#                              title = "Number of studies",
#                              horiz = TRUE,
#                              bg = "transparent",
#                              bty = "n",
#                         title = "Number of studies"))
dev.off()

# try bubble plot
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapBubbles(nmap,
           nameZSize = "number",
           nameZColour = "GEO3major",
           colourPalette = "rainbow",
           oceanCol = "azure",
           landCol = "wheat")

mapBars(nmap,
           nameZs = "number",
           nameZColour = "GEO3major",
           colourPalette = "rainbow",
           oceanCol = "azure",
           landCol = "wheat")

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
                                                    "city > x > village/city district",
                                                    "village/city district",
                                                    "village/city district > x")))
rsc_sum <- rsc_sum[order(rsc_sum$name),]
colnames(rsc_sum) <- c("scale of model", "number")
rsc_sum$proportion <- round(rsc_sum$number/n_studies, 2)

rsc_sum$number <- color_bar("lightpink")(rsc_sum$number)

ft_rsc <- rsc_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_rsc %>% as_image(width = 22, file = paste0(figdir, "/spatial&temporal_ref-scale_table.png"))

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
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_repr %>% as_image(width = 22, file = paste0(figdir, "/spatial&temporal_representation_table.png"))

repr_cooc <- make_cooc_doc(sheet = quotes_long, 
                           codegr1 = "spatial & temporal - representation split", 
                           codegr2 = "spatial & temporal - representation features")


repr <- repr_cooc %>%
  group_by(`spatial & temporal - representation split`, `spatial & temporal - representation features`) %>%
  count(`spatial & temporal - representation split`, `spatial & temporal - representation features`)

repr$n <- color_bar("lightpink")(repr$n)

ft_repr_split <- repr %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(n) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_repr_split %>% as_image(width = 22, file = paste0(figdir, "/spatial&temporal_representation-split_table.png"))


rm(list = c("repr", "repr_sum"))
###################### prepare spatial & temporal - representation ########################


###################### prepare spatial & temporal - spatial extent ########################
# to do: check whether Anderson 2004 really has two spatial extents (this is possible)
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
  scale_y_continuous(trans = "log10", limits = c(10^6, 10^15), breaks = 10^(6:15), labels = c(paste(c(1,10,100), "million"), 
                                                                                              paste(c(1, 10, 100), "billion"),
                                                                                              paste(c(1, 10, 100), "trillion"),
                                                                                              "1 quadrillion")) +
  ylab(bquote("spatial extent "~(m^2))) + xlab("Reviewed paper") +
  bgcolor("lightpink") +
  border("#BFD5E3") +
  ggtitle(paste(quant_n, "studies out of", n_studies)) + 
  geom_hline(yintercept=510100000000000, col = "deeppink", linetype = "dashed") + annotate("text", x = 3, y = 510100000000000, label = "the earth", col = "grey30") + #the world
  geom_hline(yintercept=9597000000000, col = "deeppink", linetype = "dashed") + annotate("text", x = 3, y = 9597000000000, label = "China", col = "grey30") + #china 
  geom_hline(yintercept=130279000000, col = "deeppink", linetype = "dashed") + annotate("text", x = 3, y = 130279000000, label = "England", col = "grey30") + #england
  geom_hline(yintercept=783800000, col = "deeppink", linetype = "dashed") + annotate("text", x = 3, y = 783800000, label = "New York", col = "grey30") + #new york
  annotation_logticks(sides="l")

png(filename = paste0(figdir, "/spatial&temporal_spatial-extent_dotchart.png"), width = 1200, height = 800)
dc_spext 
dev.off()

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

quant_n <- length(unique(spres$Document))

dc_spres <- ggdotchart(spres_sum, x = "Document", y = "name",
                       add = "segments",
                       add.params = list(color = "black"),
                       sorting = "ascending") +
  scale_y_continuous(trans = "log10", breaks = 10^(2:9), labels = c("100", "1000", "10,000", "100,000", "1,000,000", "10,000,000", "100,000,000", "1,000,000,000")) +
  ylab(bquote("spatial resolution "~(m^2))) + xlab("Reviewed paper") +
  bgcolor("lightpink") +
  border("#BFD5E3") +
  ggtitle(paste(quant_n, "studies out of", n_studies)) + 
  annotation_logticks(sides="l")

png(filename = paste0(figdir, "/spatial&temporal_spatial-resolution_dotchart.png"), width = 250, height = 500)
dc_spres
dev.off()

rm(list = c("spres", "spres_sum", "quant_n"))
###################### prepare spatial & temporal - spatial resolution ########################


###################### prepare spatial & temporal - temporal extent ########################
# to do: check if chen 2018 really has two temporal extents
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
  scale_y_continuous(trans = "log10", breaks = 10^(2:5), labels = c("100", "1,000", "10,000", "100,000"), limits = c(100,100000)) +
  ylab("temporal extent (days)") + xlab("Reviewed paper") +
  bgcolor("lightpink") +
  border("#BFD5E3") +
  ggtitle(paste(quant_n, "studies out of", n_studies)) +
  annotation_logticks(sides="l") +
  geom_hline(yintercept=365, col = "deeppink", linetype = "dashed") + annotate("text", x = 20, y = 365, label = "1 year", col = "grey30") + #1 year
  geom_hline(yintercept=1826, col = "deeppink", linetype = "dashed") + annotate("text", x = 20, y = 1826, label = "5 years", col = "grey30") +
  geom_hline(yintercept=18262, col = "deeppink", linetype = "dashed") + annotate("text", x = 20, y = 18262, label = "50 years", col = "grey30") +
  geom_hline(yintercept=73048, col = "deeppink", linetype = "dashed") + annotate("text", x = 20, y = 73048, label = "200 years", col = "grey30") 

png(filename = paste0(figdir, "/spatial&temporal_temporal-extent_dotchart.png"), width = 1000, height = 800)
dc_tmext
dev.off()

rm(list = c("tmext", "tmext_sum", "quant_n", "tm_range"))
###################### prepare spatial & temporal - temporal extent ########################

###################### prepare spatial & temporal - temporal resolution ########################
tmres <- quotes_long[quotes_long$code_group == "spatial & temporal - temporal resolution [d]",]
tmres$name <- as.numeric(as.character(tmres$name))
tmres <- tmres[!is.na(tmres$code_group),]

tmres_sum <- tmres %>% group_by(Document, name) %>% count(name)
tmres_sum$n <- 1
tmres_sum$name <- as.numeric(as.character(tmres_sum$name))
tmres_sum <- tmres_sum[order(tmres_sum$name),]

quant_n <- length(unique(tmres$Document))

dc_tmres <- ggdotchart(tmres_sum, x = "Document", y = "name",
                       add = "segments",
                       add.params = list(color = "black"),
                       sorting = "ascending") +
  scale_y_continuous(trans = "log10", breaks = 10^(0:4), labels = c("1", "10", "100", "1,000", "10,000")) +
  ylab("temporal resolution (days)") + xlab("Reviewed paper") +
  bgcolor("lightpink") +
  border("#BFD5E3") +
  annotation_logticks(sides="l") +
  ggtitle(paste(quant_n, "studies out of", n_studies)) +
  geom_hline(yintercept=365, col = "deeppink", linetype = "dashed") + annotate("text", x = 2, y = 365, label = "1 year", col = "grey30") + #1 year
  geom_hline(yintercept=30, col = "deeppink", linetype = "dashed") + annotate("text", x = 2, y = 30, label = "1 month", col = "grey30") +
  geom_hline(yintercept=7, col = "deeppink", linetype = "dashed") + annotate("text", x = 2, y = 7, label = "1 week", col = "grey30") + #1 year
  geom_hline(yintercept = 3652, col = "deeppink", linetype = "dashed") + annotate("text", x = 2, y = 3652, label = "1 decade", col = "grey30") +
  geom_hline(yintercept = 1, col = "deeppink", linetype = "dashed") + annotate("text", x = 2, y = 1, label = "1 day", col = "grey30")

png(filename = paste0(figdir, "/spatial&temporal_temporal-resolution_dotchart.png"), width = 750, height = 800)
dc_tmres
dev.off()

rm(list = c("tmres", "tmres_sum", "quant_n"))
###################### prepare spatial & temporal - temporal resolution ########################


###################### prepare food system - echelon ########################
ech_long <- quotes_long
ech_long$name[ech_long$name %in% c("distribution", "transport")] <- "distribution/transport"
ech_long$name[ech_long$name %in% c("processing", "manufacturing", "storage", "wholesale")] <- "processing/manufacturing/storage/wholesale"
ech <- ech_long[ech_long$code_group == "food system - echelon",]
ech <- ech[!is.na(ech$code_group),]
ech_sum <- level1_count(sheet = ech)
ech_sum$name <- factor(ech_sum$name, levels = rev(unique(ech_sum$name)))
colnames(ech_sum) <- c("value chain echelon", "number")
ech_sum$proportion <- round(ech_sum$number/n_studies, 2)

ech_sum$number <- color_bar("orange")(ech_sum$number)

ft_ech <- ech_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_ech %>% as_image(width = 22, file = paste0(figdir, "/food-system_echelon_table.png"))

ech_sum <- ech %>% group_by(Document, name) %>% count(Document, name)
ech_sum$n <- 1

production <- ech_sum[ech_sum$name == "production",]$Document
distribution <- ech_sum[ech_sum$name == "distribution/transport" ,]$Document
processing <- ech_sum[ech_sum$name == "processing/manufacturing/storage",]$Document
retail <- ech_sum[ech_sum$name == "retail",]$Document
consumption <- ech_sum[ech_sum$name == "comsumption",]$Document

myCol <- c("yellow", "deeppink", 
           "green", 
           "grey", "turquoise1")

venn.diagram( #!! make better
  x = list(production, distribution, 
           processing, 
           retail, consumption),
  category.names = c("production", "distribution", 
                     "processing", 
                     "retail", "consumption"),
  filename = '../13042021_Figures/food-system_echelon_venn.png',
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
               0.075, 
               0.075, 0.085),
  cat.fontfamily = "sans"# ,
  # rotation = 1
)

# model type x echelon cooc
dat_cooc <- make_cooc_doc(sheet = ech_long, codegr1 = "per model - type", codegr2 = "food system - echelon")

dat_cooc <- dat_cooc %>% 
  group_by(`food system - echelon`, `per model - type`) %>%
  count(`food system - echelon`, `per model - type`)

x <- 2

dat_cooc$`food system - echelon` <- factor(dat_cooc$`food system - echelon`, levels = c("hunting/fishing/gathering",
                                                                                        "production",
                                                                                        "distribution/transport",
                                                                                        "trade",
                                                                                        "processing/manufacturing/storage/wholesale",
                                                                                        "retail",
                                                                                        "consumption"))

dat_cooc <- dat_cooc[order(dat_cooc$`food system - echelon`),] 
png(filename = paste0(figdir, "/per-model_type_food-system_echelon_co-occurence.png"), width = 1300, height = 1300)
ggplot(dat_cooc, aes(y = `food system - echelon`, x = `per model - type`, col = `per model - type`, label = n)) +
  geom_point(aes(size = n)) +
  geom_text(col = "black", fontface = "bold", size = 10) +
  #ggtitle(paste("Total number of studies =", n_studies, ". Studies with combined governances measures =", n_combined)) +
  ylab("Value chain echelon") +
  xlab("Model type") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.4*x), angle = 90), 
        axis.text.y = element_text(size = rel(1.4*x)), 
        axis.title = element_text(size=rel(1.2*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_size(range = c(10,50), breaks = c(0:max(dat_cooc$n))) +
  #scale_color_gradient(low = "white", high = "red") +
  coord_flip()
dev.off()

rm(list = c("dat", "dat_sum", "dat_cooc", "x"))
rm(list = c("ech", "ech_sum", "ech_long"))
###################### prepare food system - echelon ########################

###################### prepare food system - commodity ########################
com <- quotes_long[quotes_long$code_group == "food system - commodity",]
com <- com[!is.na(com$code_group),]
com_sum <- level1_count(sheet = com)
com_sum$name <- factor(com_sum$name, levels = rev(unique(com_sum$name)))
colnames(com_sum) <- c("food commodity", "number")
com_sum$proportion <- round(com_sum$number/n_studies, 2)

com_sum$number <- color_bar("orange")(com_sum$number)

ft_com <- com_sum[1:20,] %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Top 20 commodities, gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_com %>% as_image(width = 22, file = paste0(figdir, "/food-system_commodity-top20_table.png")) 
# commodity classes
com$class <- ""
com <- dict_classification(sheet = com, dct = comm_class, clm = 16, class_clm = 17)
colnames(com)[16:17] <- c("commodity", "name")
com_sum <- level1_count(sheet = com)
com_sum$name <- factor(com_sum$name, levels = rev(unique(com_sum$name)))
colnames(com_sum) <- c("food commodity group", "number")
com_sum$proportion <- round(com_sum$number/n_studies, 2)

com_sum$number <- color_bar("orange")(com_sum$number)

ft_comgr <- com_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers. Grouped using FAO commodity list. *Groups used for commodities not listed by FAO")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_comgr %>% as_image(width = 22, file = paste0(figdir, "/food-system_commodity-grouped_table.png")) 
rm(list = c("com", "com_sum", "code_vec"))

###################### prepare food system - commodity ########################


###################### prepare nonfood system - commodity ########################
com <- quotes_long[quotes_long$code_group == "nonfood system - commodity",]
com <- com[!is.na(com$code_group),]
com_sum <- level1_count(sheet = com)
com_sum$name <- factor(com_sum$name, levels = rev(unique(com_sum$name)))
colnames(com_sum) <- c("food commodity", "number")
com_sum$proportion <- round(com_sum$number/n_studies, 2)

com_sum$number <- color_bar("orange")(com_sum$number)

ft_noncom <- com_sum[1:20,] %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Top 20 non-food commodities, gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_noncom %>% as_image(width = 22, file = paste0(figdir, "/nonfood-system_commodity-top20_table.png")) 
rm(list = c("com", "com_sum"))
###################### prepare nonfood system - commodity ########################


###################### governance - combined measures? ########################
gov <- quotes_long[quotes_long$code_group == "governance - combined measures?",]
gov <- gov[!is.na(gov$code_group),]
gov_sum <- level1_count(sheet = gov)
gov_sum$name <- factor(gov_sum$name, levels = rev(unique(gov_sum$name)))
colnames(gov_sum) <- c("combined governance measures?", "number")
gov_sum$proportion <- round(gov_sum$number/n_studies, 2)

gov_sum$number <- color_bar("red")(gov_sum$number)

ft_gov <- gov_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_gov %>% as_image(width = 22, file = paste0(figdir, "/governance_combined-measures_table.png")) 

# which governance measures are combined?

# first limit data to where the co-occurence of governance measures is specified
gov <- quotes_long2[grep("governance - combined measures?: yes", quotes_long$Codes, fixed = TRUE),]
n_combined <- length(unique(gov$Document))

gov <- gov[gov$code_group == "per measure - measure",]
gov <- gov[!is.na(gov$code_group),]
gov <- gov[!is.na(gov$name),]

gov$class <- ""
gov <- dict_classification(sheet = gov, dct = NOTA_subclass, clm = 16, class_clm = 17)
colnames(gov)[17] <- "subclass"
gov$class <- ""
gov <- dict_classification(sheet = gov, dct = NOTA_class, clm = 17, class_clm = 18)

gov_sum <- gov %>% 
  group_by(ID, name, subclass, class) %>%
  count(ID, name, subclass, class)

selfcooc <- gov %>% 
  group_by(ID, subclass, class) %>%
  count(ID, subclass, class)

selfcooc <- selfcooc[selfcooc$n > 1,]

selfcooc <- selfcooc %>% group_by(subclass, class) %>% count(subclass, class)

V <- crossprod(table(gov_sum[c(1,3)]))
diag(V) <- 0 #figure out how to do it properly with selfcooc$n
V

Vdat <- as.data.frame(V)
colnames(Vdat) <- gsub("\\s*\\([^\\)]+\\)","", colnames(Vdat)) #remove parentheses because they cause problems later
rownames(Vdat) <- gsub("\\s*\\([^\\)]+\\)","", rownames(Vdat))
colnames(Vdat) <- trimws(colnames(Vdat), which = c("both"), whitespace = "[ \t\r\n]") #remove white spaces at the beginning and end because they cause problems later 
rownames(Vdat) <- trimws(rownames(Vdat), which = c("both"), whitespace = "[ \t\r\n]")
colnames(Vdat) <- gsub(" ","_", colnames(Vdat)) #remove spaces because they cause problems later
rownames(Vdat) <- gsub(" ","_", rownames(Vdat))
Vdat$measures1 <- rownames(Vdat)

library(reshape2)
library(igraph)
library(visNetwork)
Vlong <- melt(Vdat, id.vars = c("measures1"))
colnames(Vlong)[2] <- "measures2"

Vnodes <- as.data.frame(cbind(unique(Vlong$measures1), unique(Vlong$measures1)))
colnames(Vnodes) <- c("id", "label")
Vedges <- Vlong
colnames(Vedges) <- c("from", "to", "width")

#Create graph for Louvain
graph <- graph_from_data_frame(Vedges, directed = FALSE)
#Louvain Comunity Detection
cluster <- cluster_louvain(graph)
cluster_df <- data.frame(as.list(membership(cluster)))
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$label <- rownames(cluster_df)
colnames(Vnodes) <- c("id", "label")
#Create group column
Vnodes <- left_join(Vnodes, cluster_df, by = "label")
colnames(Vnodes)[3] <- "group"
nodes <- Vnodes
edges <- Vedges

visNetwork(nodes, edges) %>%
  visOptions( highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction( navigationButtons = TRUE) %>%
  visPhysics( maxVelocity = 35)
# to do: colour by NATO

Vedges$from <- gsub("_", " ", Vedges$from)
Vedges$to <- gsub("_", " ", Vedges$to)
Vedges$class <- ""
Vedges <- dict_classification(sheet = Vedges, dct = NOTA_class, clm = 2, class_clm = 4)
str(Vedges$class)
Vedges$class1 <- as.factor(Vedges$class)
Vedges <- dict_classification(sheet = Vedges, dct = NOTA_class, clm = 1, class_clm = 4)

Vedges$from <- factor(x = Vedges$from, 
                      levels = (c("bespoke messages", "group targeted messages", "propaganda", #nodality
                                 "open compacts", "open permits", "standard constraints", #authority
                                 "bearer-directed payments", "bounties", "conduits", "contracts", "transfers", #treasure
                                 "at large treatment", "group treatment", "transportation and redistribution", #organisation
                                 "unclear"))) 

Vedges$to <- factor(x = Vedges$to, 
                      levels = rev(c("bespoke messages", "group targeted messages", "propaganda", #nodality
                                 "open compacts", "open permits", "standard constraints", #authority
                                 "bearer-directed payments", "bounties", "conduits", "contracts", "transfers", #treasure
                                 "at large treatment", "group treatment", "transportation and redistribution", #organisation
                                 "unclear"))) 

with(Vedges, Vedges[order(from, to, decreasing = F),])

Vedges$text <- Vedges$width
Vedges$text <- as.character(Vedges$text)
Vedges$text[Vedges$text == "0"] <- ""

x = 2
png(filename = paste0(figdir, "/governance_combined-measures_co-occurence.png"), width = 1300, height = 1300)
ggplot(Vedges, aes(x = from, y = to, label = text)) +
  geom_point(aes(size = width, col = class), shape = 15) +
  geom_point(aes(size = width, col = class1), shape = 16) +
  geom_text(col = "black", fontface = "bold", size = 10) +
  ggtitle(paste("Total number of studies =", n_studies, ". Studies with combined governances measures =", n_combined)) +
  ylab("Governance measure") +
  xlab("Co-occuring governance measure") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = rel(1.4*x), angle = 90), 
        axis.text.y = element_text(size = rel(1.4*x)), 
        axis.title = element_text(size=rel(0.6*x), face="bold"),
        legend.title = element_text(size = rel(1.3*x), face = "bold"), 
        legend.position = "none",
        legend.text = element_text(size = rel(1.4*x)),
        title = element_text(size = rel(1.4*x))) + 
  scale_size(range = c(0,30), breaks = c(1:max(Vedges$width))) +
  scale_color_manual(values = brewer.pal(n = 5, name = "Set1")) #+
# coord_flip()
dev.off()

rm(list = c("x", "gov", "gov_sum", "Vedges", "edges", "nodes", "Vnodes", "cluster_df", "graph", "cluster", "V", "Vdat", "Vlong"))
###################### governance - combined measures? ########################


###################### per measure - measure ########################
meas <- quotes_long2[quotes_long2$code_group == "per measure - measure",]
meas <- meas[!is.na(meas$code_group),]
meas_sum <- level1_count(sheet = meas)

meas_sum$name <- factor(meas_sum$name, levels = rev(unique(meas_sum$name)))
colnames(meas_sum) <- c("governance measure", "number")
meas_sum$proportion <- round(meas_sum$number/n_studies, 2)

meas_sum$number <- color_bar("red")(meas_sum$number)

ft_meas <- meas_sum[1:20,] %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Top-20 governance measures, gathered from", n_studies, "papers")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_meas %>% as_image(width = 22, file = paste0(figdir, "/per-measure_measure-top20_table.png")) 

# measure classes
# to do: add total number of governance measures to caption
meas$class <- ""
meas <- dict_classification(sheet = meas, dct = NOTA_subclass, clm = 16, class_clm = 17)
meas_sum <- level2_count(sheet = meas)
meas_sum$class <- factor(meas_sum$class, levels = rev(unique(meas_sum$class)))
colnames(meas_sum) <- c("governance measures by type", "number")
meas_sum$proportion <- round(meas_sum$number/n_studies, 2)

meas_sum$`governance measures by type` <- factor(x = meas_sum$`governance measures by type`,
                    levels = rev(c("group targeted messages", "propaganda", "packaged self-serve messages", "bespoke messages",   #nodality
                                   "standard constraints", "open permits", "open compacts", "directed constraints",    #authority
                                   "bearer-directed payments", "bounties", "conduits", "contracts", "transfers", #treasure
                                   "at large treatment", "transportation and redistribution", "processing", "storage and custody", "group treatment",  #organisation
                                   "unclear")))

meas_sum$number <- color_bar("red")(meas_sum$number)

meas_sum <- meas_sum[order(meas_sum$`governance measures by type`, decreasing = TRUE),]
head(meas_sum)

str(meas_sum$`governance measures by type`)

ft_measgr <- meas_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(`governance measures by type`) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center") %>%
  pack_rows("Nodality", 1, 4) %>%
  pack_rows("Authority", 5, 8) %>%
  pack_rows("Treasure", 9, 13) %>%
  pack_rows("Organisation", 14, 18) %>%
  pack_rows("Unclear", 19, 19) 

ft_measgr %>% as_image(width = 22, file = paste0(figdir, "/per-measure_measure-grouped_table.png")) 

rm(list = c("meas", "meas_sum"))

# to do: make a list with all measures grouped per type
###################### per measure - measure ########################


###################### per measure - type 2 ########################
mstyp2 <- quotes_long2[quotes_long2$code_group %in% c("per measure - type 2", "per measure - measure"),]
mstyp2 <- mstyp2[!is.na(mstyp2$code_group),]

mstyp2 <- mstyp2 %>% spread(code_group, name)
colnames(mstyp2)[which(colnames(mstyp2)=="per measure - type 2")] <- "type2"
colnames(mstyp2)[which(colnames(mstyp2)=="per measure - measure")] <- "measure"
meas <- mstyp2[,which(colnames(mstyp2) %in% c("measure", "ID", "Document"))]
typ2 <- mstyp2[,which(colnames(mstyp2) %in% c("type2", "ID"))]

mstyp2 <- merge(meas, typ2, by = "ID")
mstyp2 <- na.omit(mstyp2)


mstyp2 <- mstyp2 %>% group_by(Document, measure, type2) %>%
  count(Document, measure, type2)

mstyp2$n <- 1

mstyp2_sum <- mstyp2 %>% 
  group_by(type2) %>%
  count(type2)

mstyp2_sum <- mstyp2_sum %>%
  arrange(desc(n))

mstyp2_sum$type2 <- factor(mstyp2_sum$type2, levels = rev(unique(mstyp2_sum$type2)))
mstyp2_sum$proportion <- round(mstyp2_sum$n/n_measures, 2)
colnames(mstyp2_sum)[1:2] <- c("organisation of governance measure", "number")

mstyp2_sum$number <- color_bar("red")(mstyp2_sum$number)

ft_mstyp2 <- mstyp2_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_measures, "governance measures")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_mstyp2 %>% as_image(width = 22, file = paste0(figdir, "/per-measure_measure-type2_table.png")) 

rm(list = c("mstyp2", "mstyp2_sum"))
###################### per measure - type 2 ########################


###################### per measure - objective ########################
obj <- level2_summ(level1code = "per measure - measure", level2code = "per measure - objective", dat_long = quotes_long2)
obj$proportion <- round(obj$n/n_measures, 2)
colnames(obj)[1:2] <- c("governance objectives", "number")

obj2 <- obj
obj$number <- color_bar("red")(obj$number)

ft_obj <- obj[1:20,] %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_measures, "governance measures. Top 20.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_obj %>% as_image(width = 22, file = paste0(figdir, "/per-measure_objective-top20_table.png")) 

#objectives grouped according to food security dimension or - if not related to food security - other societal domains
objclss <- level2_class_summ(level1code = "per measure - measure", level2code = "per measure - objective", dat_long = quotes_long2, classdct = goals_class)
colnames(objclss) <- c("governance objective groups", "number")
objclss$number <- color_bar("red")(objclss$number)

objclss$`governance objective groups`
objclss$`governance objective groups` <- factor(objclss$`governance objective groups`, 
                                                levels = c("availability",
                                                           "access  - general",
                                                           "access - economic",
                                                           "access  - physical",
                                                           "utilisation",
                                                           "stability",
                                                           "macro-economic",
                                                           "environmental/climate",
                                                           "infrastructure & technology",
                                                           "health & wellbeing",
                                                           "macro-logistics",
                                                           "other"))

objclss <- objclss[order(objclss$`governance objective groups`),]
ft_objgr <- objclss %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_measures, "governance measures.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center") %>%
  pack_rows("Food security objectives", 1, 6) %>%
  pack_rows("Other objectives", 7, 12) 

ft_objgr %>% as_image(width = 22, file = paste0(figdir, "/per-measure_objective-grouped_table.png")) 

rm(list = c("obj", "objclss"))

###################### per measure - objective ########################

###################### per measure - scale ########################
msc <- level2_summ(level1code = "per measure - measure", level2code = "per measure - scale", dat_long = quotes_long)
msc$proportion <- round(msc$n/n_measures, 2)
colnames(msc)[1:2] <- c("scales of governance", "number")

msc$number <- color_bar("red")(msc$number)

msc$`scales of governance` <- factor(msc$`scales of governance`, levels = rev(c("unclear", "earth", 
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
                                                                                "city > x > village/city_district",
                                                                                "village/city district")))
msc <- msc[order(msc$`scales of governance`),]

ft_msc <- msc %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_measures, "governance measures.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_msc %>% as_image(width = 22, file = paste0(figdir, "/per-measure_scale_table.png")) 

rm(list = c("msc"))
###################### per measure - scale ########################


###################### per measure - target implementer ########################
mdat <- level2_summ(level1code = "per measure - measure", level2code = "per measure - target implementer", dat_long = quotes_long2)

colnames(mdat) <- c("implementer of governance measure", "number")
mdat$proportion <- round(mdat$number/n_measures, 2)

mdat$number <- color_bar("red")(mdat$number)

ft_impl <- mdat %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_measures, "governance measures.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_impl %>% as_image(width = 22, file = paste0(figdir, "/per-measure_target-implementer_table.png")) 

# target implementer grouped
mdatclss <- level2_class_summ(level1code = "per measure - measure", level2code = "per measure - target implementer", dat_long = quotes_long2, classdct = timpl_class)
colnames(mdatclss) <- c("target implementing entities", "number")
mdatclss$number <- color_bar("red")(mdatclss$number)

ft_objgr <- mdatclss %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_measures, "governance measures.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_objgr %>% as_image(width = 22, file = paste0(figdir, "/per-measure_target-implementer-grouped_table.png")) 


rm(list = c("mstyp", "mstyp_sum", "mdatclss"))
###################### per measure - target implementer ########################

###################### per measure - spatially targeted? ########################
mdat <- level2_summ(level1code = "per measure - measure", level2code = "per measure - spatially targeted?", dat_long = quotes_long2)
colnames(mdat) <- c("spatially targeted governance?", "number")
mdat$proportion <- round(mdat$number/n_measures, 2)
mdat$number <- color_bar("red")(mdat$number)

ft_sptg <- mdat %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_measures, "governance measures.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_sptg %>% as_image(width = 22, file = paste0(figdir, "/per-measure_spatially-targeted_table.png")) 

rm(list = c("mdat"))
###################### per measure - spatially targeted? ########################


###################### per effect - FS indicator ########################
mdat <- level2_summ(level1code = "per effect - direct?", level2code = "per effect - FS indicator", dat_long = quotes_long2)
colnames(mdat) <- c("food security indicator", "number")

n_effects <- length(unique(quotes_long[quotes_long$code_group == "per effect - FS indicator",]$ID))

mdat$proportion <- round(mdat$number/n_effects, 2)
mdat$number <- color_bar("yellow")(mdat$number)

ft_FSin <- mdat[1:20,] %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_effects, "impacts of", n_measures, "governance measures. Top 20.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_FSin %>% as_image(width = 22, file = paste0(figdir, "/per-effect_FS-indicator-top20_table.png"))

# FS indicator classes
mdatclss <- level2_class_summ(level1code = "per measure - measure", level2code = "per effect - FS indicator", dat_long = quotes_long2, classdct = FSi_class)
colnames(mdatclss) <- c("food security indicator grouped", "number")
mdatclss$proportion <- round(mdatclss$number/n_effects, 2)
mdatclss$`food security indicator grouped` <- as.character(mdatclss$`food security indicator grouped`)
mdatclss$`food security indicator grouped`[mdatclss$`food security indicator grouped` == "access - economic"] <- "...access - economic"
mdatclss$`food security indicator grouped`[mdatclss$`food security indicator grouped` == "access - physical"] <- "...access - physical"

mdatclss$`food security indicator grouped` <- factor(mdatclss$`food security indicator grouped`, levels = c("availability",
                                                                                                            "access - general",
                                                                                                            "...access - economic",
                                                                                                            "...access - physical",
                                                                                                            "utilisation",
                                                                                                            "stability",
                                                                                                            "unclear"))
mdatclss <- mdatclss[order(mdatclss$`food security indicator grouped`),]

mdatclss$number <- color_bar("yellow")(mdatclss$number)

ft_fsigr <- mdatclss %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_effects, "impacts of", n_measures, "governance measures.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_fsigr %>% as_image(width = 22, file = paste0(figdir, "/per-effect_FS-indicator-grouped_table.png"))

rm(list = c("mdat", "mdatclss"))
###################### per effect - FS indicator ########################


###################### per effect - direct? ########################
mdat <- level2_summ(level1code = "per measure - measure", level2code = "per effect - direct?", dat_long = quotes_long)
colnames(mdat) <- c("direct impact?", "number")

mdat$proportion <- round(mdat$number/n_effects, 2)
mdat$number <- color_bar("yellow")(mdat$number)

ft_dir <- mdat %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_effects, "impacts of", n_measures, "governance measures.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_dir %>% as_image(width = 22, file = paste0(figdir, "/per-effect_direct_table.png")) #!! proportions don't add up

rm(list = c("mdat"))
###################### per effect - direct? ########################


###################### per effect - place ########################
mdat <- level2_summ(level1code = "per effect - FS indicator", level2code = "per effect - place", dat_long = quotes_long)

colnames(mdat) <- c("place", "number")
mdat$place <- as.character(mdat$place)
mdat$place[which(mdat$place == "local")] <- "within jurisdiction"
mdat$place[which(mdat$place == "far away")] <- "outside jurisdiction"

mdat$proportion <- round(mdat$number/n_effects, 2)
mdat$number <- color_bar("yellow")(mdat$number)

ft_plc <- mdat %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_effects, "impacts of", n_measures, "governance measures.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_plc %>% as_image(width = 22, file = paste0(figdir, "/per-effect_place_table.png")) #!! proportions don't add up

rm(list = c("mdat"))
###################### per effect - place ########################


###################### per effect - intended ########################
mdat <- level2_summ(level1code = "per effect - FS indicator", level2code = "per effect - intended?", dat_long = quotes_long)
colnames(mdat) <- c("intended impact?", "number")

mdat$proportion <- round(mdat$number/n_effects, 2)
mdat$number <- color_bar("yellow")(mdat$number)

ft_intd <- mdat %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers, describing", n_effects, "impacts of", n_measures, "governance measures.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_intd %>% as_image(width = 22, file = paste0(figdir, "/per-effect_intended_table.png")) #!! proportions don't add up

rm(list = c("mdat"))
###################### per effect - intended ########################


###################### per effect - on FS? ########################
mstyp <- quotes_long[quotes_long$code_group %in% c("per effect - on FS?", "per measure - measure"),]
mstyp <- mstyp[!is.na(mstyp$code_group),]
mstyp$class <- ""
mstyp_sum <- level2_summ(level1code = "per measure - measure", level2code = "per effect - on FS?", dat_long = mstyp) #something goes wrong here

n_effects <- length(unique(mstyp$ID))

mstyp_sum$codeB <- factor(mstyp_sum$codeB , levels = rev(unique(mstyp_sum$codeB)))
colnames(mstyp_sum) <- c("food security impact?", "number")
mstyp_sum$proportion <- round(mstyp_sum$number/n_effects, 2)

mstyp_sum$number <- color_bar("yellow")(mstyp_sum$number)

ft_eofs <- mstyp_sum %>% 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers describing", n_effects, "governance impacts")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_eofs %>% as_image(width = 22, file = paste0(figdir, "/per-effect_on-FS_table.png"))

rm(list = c("mstyp", "mstyp_sum"))
###################### per effect - on FS? ########################


###################### per effect - affected agent ########################
mstyp <- quotes_long[quotes_long$code_group %in% c("per effect - affected agent", "per measure - measure"),]
mstyp <- mstyp[!is.na(mstyp$code_group),]
mstyp$class <- ""
mstyp_sum <- level2_summ(dat_long = mstyp, level1code = "per measure - measure", level2code = "per effect - affected agent")

mstyp_sum$codeB <- factor(mstyp_sum$codeB, levels = rev(unique(mstyp_sum$codeB)))
colnames(mstyp_sum) <- c("affected agent", "number")
mstyp_sum$proportion <- round(mstyp_sum$number/n_effects, 2)

mstyp_sum$number <- color_bar("yellow")(mstyp_sum$number)

ft_eafa <- mstyp_sum[c(1:20),] %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Top 20. Gathered from", n_studies, "papers describing", n_effects, "governance impacts.")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_eafa %>% as_image(width = 22, file = paste0(figdir, "/per-effect_affected-agent-top20_table.png"))

# agent classes
mstyp$class <- ""
mstyp <- dict_classification(sheet = mstyp, dct = timpl_class, clm = 16, class_clm = 17)
colnames(mstyp)[16:17] <- c("agent type", "name")
mstyp_sum <- level1_count(sheet = mstyp)
mstyp_sum <- mstyp_sum[mstyp_sum$name != "",] 

mstyp_sum$name <- as.factor(mstyp_sum$name)
mstyp_sum$name <- factor(mstyp_sum$name, levels = unique(mstyp_sum$name)) 

# none <- which(levels(mstyp_sum$name) == "none")
# mstyp_sum$name <- factor(mstyp_sum$name, levels = c(levels(mstyp_sum$name)[-none], levels(mstyp_sum$name)[none]))
mstyp_sum <- mstyp_sum[order(mstyp_sum$name),] 

colnames(mstyp_sum) <- c("affected agent types", "number")
mstyp_sum$proportion <- round(mstyp_sum$number/n_studies, 2)

mstyp_sum$number <- color_bar("chocolate")(mstyp_sum$number)

ft_eafagr <- mstyp_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers.")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_eafagr %>% as_image(width = 22, file = paste0(figdir, "/per-effect_affected-agent-grouped_table.png")) #!! maybe use packrows to differentiate food and non-food agents

rm(list = c("mstyp", "mstyp_sum"))
###################### per effect - affected agent ########################

###################### per effect - type other ########################
mstyp <- quotes_long[quotes_long$code_group %in% c("per effect - type other", "per measure - measure"),]
mstyp <- mstyp[!is.na(mstyp$code_group),]
mstyp$class <- ""
mstyp_sum <- level2_summ(level1code = "per measure - measure", level2code = "per effect - type other", dat_long = mstyp)

n_effects <- length(unique(mstyp$ID))

mstyp_sum$codeB <- factor(mstyp_sum$codeB, levels = rev(unique(mstyp_sum$codeB)))
colnames(mstyp_sum) <- c("type of non-food security effects", "number")
mstyp_sum$proportion <- round(mstyp_sum$number/n_effects, 2)

mstyp_sum$number <- color_bar("yellow")(mstyp_sum$number)

ft_etpo <- mstyp_sum %>% 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers describing", n_effects, "governance impacts")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_etpo %>% as_image(width = 22, file = paste0(figdir, "/per-effect_type-other_table.png"))

rm(list = c("mstyp", "mstyp_sum"))
###################### per effect - type other ########################

###################### governance measures sankey ########################
gov <- quotes_long2[quotes_long2$code_group %in% c("per measure - objective", "per measure - measure", "per effect - FS indicator"),]
gov_w <- spread(gov, code_group, name)
obj_w <- gov_w[,c(1,18)]
meas_w <- gov_w[,c(1,17)]
fsi_w <- gov_w[,c(1,16)]

gov_s <- merge(obj_w, meas_w,  by = "ID")
gov_t <- merge(meas_w, fsi_w,  by = "ID")
gov_s <- na.omit(gov_s)
gov_t <- na.omit(gov_t)

gov_s$m_class <- ""
gov_s$o_class <- ""
gov_s <- dict_classification(sheet = gov_s, dct = goals_class, clm = 2, class_clm = 5)
gov_s <- dict_classification(sheet = gov_s, dct = NOTA_subclass, clm = 3, class_clm = 4)

gov_t$m_class <- ""
gov_t$f_class <- ""
gov_t <- dict_classification(sheet = gov_t, dct = NOTA_subclass, clm = 2, class_clm = 4)
gov_t <- dict_classification(sheet = gov_t, dct = FSi_class, clm = 3, class_clm = 5)

gov_s <- gov_s %>% group_by(o_class, m_class, ID) %>% count(o_class, m_class, ID)
gov_s$o_class <- paste("obj:", gov_s$o_class)
gov_t <- gov_t%>% group_by(m_class, f_class, ID) %>% count(m_class, f_class, ID)
gov_t$f_class <- paste("FSi:", gov_t$f_class)

#intract_target$target <- paste("receiver:", intract_target$target)
colnames(gov_s) <- c("source",  "target", "ID", "value")
colnames(gov_t) <- c("source",  "target", "ID", "value")
links <- rbind(gov_s, gov_t)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
s_gov <- sankeyNetwork(Links = links, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget",
                       Value = "value", NodeID = "name", 
                       sinksRight=FALSE, fontSize = 19)
s_gov #!! colour by NATO
saveNetwork(s_gov, paste0(figdir, "/obj_meas_FSindicator_sankey.html"))
webshot(paste0(figdir, "/obj_meas_FSindicator_sankey.html"),paste0(figdir, "/obj_meas_FSindicator_sankey.png"), vwidth = 1200, vheight = 900)
###################### governance measures sankey ########################

###################### agent - agent representation ########################
mstyp <- quotes_long[quotes_long$code_group == "agent - representation",]
mstyp <- mstyp[!is.na(mstyp$code_group),]
mstyp_sum <- level1_count(sheet = mstyp)

mstyp_sum$name <- factor(mstyp_sum$name, levels = rev(unique(mstyp_sum$name)))
colnames(mstyp_sum) <- c("agent representation", "number")
mstyp_sum$proportion <- round(mstyp_sum$number/n_studies, 2)

mstyp_sum$number <- color_bar("chocolate")(mstyp_sum$number)

ft_arep <- mstyp_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_arep %>% as_image(width = 22, file = paste0(figdir, "/agent_representation_table.png"))

rm(list = c("mstyp", "mstyp_sum"))
###################### agent - agent representation ########################

###################### agent - paradigm ########################
mstyp <- quotes_long[quotes_long$code_group == "agent - paradigm",]
mstyp <- mstyp[!is.na(mstyp$code_group),]
mstyp_sum <- level1_count(sheet = mstyp)

mstyp_sum$name <- factor(mstyp_sum$name, levels = rev(unique(mstyp_sum$name)))
colnames(mstyp_sum) <- c("agent decision-making paradigm", "number")
mstyp_sum$proportion <- round(mstyp_sum$number/n_studies, 2)

mstyp_sum$number <- color_bar("chocolate")(mstyp_sum$number)

ft_apar <- mstyp_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_apar %>% as_image(width = 22, file = paste0(figdir, "/agent_paradigm_table.png"))

rm(list = c("mstyp", "mstyp_sum"))
###################### agent - paradigm ########################

###################### agent - theory ########################
mstyp <- quotes_long[quotes_long$code_group == "agent - theory",]
mstyp <- mstyp[!is.na(mstyp$code_group),]
mstyp_sum <- level1_count(sheet = mstyp)

mstyp_sum$name <- factor(mstyp_sum$name, levels = rev(unique(mstyp_sum$name)))
colnames(mstyp_sum) <- c("agent decision-making theory", "number")
mstyp_sum$proportion <- round(mstyp_sum$number/n_studies, 2)

mstyp_sum$number <- color_bar("chocolate")(mstyp_sum$number)

ft_athe <- mstyp_sum %>% 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_athe %>% as_image(width = 22, file = paste0(figdir, "/agent_theory_table.png"))

rm(list = c("mstyp", "mstyp_sum"))
###################### agent - theory ########################

###################### agent - method ########################
mstyp <- quotes_long[quotes_long$code_group == "agent - method",]
mstyp <- mstyp[!is.na(mstyp$code_group),]
mstyp_sum <- level1_count(sheet = mstyp)

mstyp_sum$name <- factor(mstyp_sum$name, levels = rev(unique(mstyp_sum$name)))
colnames(mstyp_sum) <- c("agent decision-making method", "number")
mstyp_sum$proportion <- round(mstyp_sum$number/n_studies, 2)

mstyp_sum$number <- color_bar("chocolate")(mstyp_sum$number)

ft_amet <- mstyp_sum %>% 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_amet %>% as_image(width = 22, file = paste0(figdir, "/agent_method_table.png")) 

rm(list = c("mstyp", "mstyp_sum"))
###################### agent - method ########################


###################### per agent - agent ########################
mstyp <- quotes_long[quotes_long$code_group == "per agent - agent",]
mstyp <- mstyp[!is.na(mstyp$code_group),]
mstyp_sum <- level1_count(sheet = mstyp)

mstyp_sum$name <- factor(mstyp_sum$name, levels = rev(unique(mstyp_sum$name)))
colnames(mstyp_sum) <- c("agent", "number")
mstyp_sum$proportion <- round(mstyp_sum$number/n_studies, 2)

none <- which(levels(mstyp_sum$agent) == "none")
mstyp_sum$agent <- factor(mstyp_sum$agent, levels = rev(c(levels(mstyp_sum$agent)[none], levels(mstyp_sum$agent)[-none])))
mstyp_sum <- mstyp_sum[order(mstyp_sum$agent),] 

mstyp_sum$number <- color_bar("chocolate")(mstyp_sum$number)

ft_agnt <- mstyp_sum %>%  
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_agnt %>% as_image(width = 22, file = paste0(figdir, "/per-agent_agent_table.png"))

# agent classes
mstyp$class <- ""
mstyp <- dict_classification(sheet = mstyp, dct = timpl_class, clm = 16, class_clm = 17)
colnames(mstyp)[16:17] <- c("agent type", "name")
mstyp_sum <- level1_count(sheet = mstyp)

mstyp_sum$name <- as.factor(mstyp_sum$name)
mstyp_sum$name <- factor(mstyp_sum$name, levels = unique(mstyp_sum$name)) 

none <- which(levels(mstyp_sum$name) == "none")
mstyp_sum$name <- factor(mstyp_sum$name, levels = c(levels(mstyp_sum$name)[-none], levels(mstyp_sum$name)[none]))
mstyp_sum <- mstyp_sum[order(mstyp_sum$name),] 

colnames(mstyp_sum) <- c("agent type", "number")
mstyp_sum$proportion <- round(mstyp_sum$number/n_studies, 2)

mstyp_sum$number <- color_bar("chocolate")(mstyp_sum$number)

ft_agntgr <- mstyp_sum %>% 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers.")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_agntgr %>% as_image(width = 22, file = paste0(figdir, "/per-agent_agent-grouped_table.png"))

rm(list = c("mstyp", "mstyp_sum"))
###################### per agent - agent ########################

###################### per agent - heterogeneity ########################
mstyp <- quotes_long[quotes_long$code_group == "per agent - heterogeneity",]
mstyp <- mstyp[!is.na(mstyp$code_group),]
mstyp_sum <- level1_count(sheet = mstyp)

mstyp_sum$name <- factor(mstyp_sum$name, levels = c("continuous", "1 type", paste(2:90, "types"), "not applicable"))
mstyp_sum <- mstyp_sum[order(mstyp_sum$name),]
colnames(mstyp_sum) <- c("agent heterogeneity", "number")
mstyp_sum$proportion <- round(mstyp_sum$number/n_studies, 2)

mstyp_sum$number <- color_bar("chocolate")(mstyp_sum$number)

ft_ahet <- mstyp_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers")) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_ahet %>% as_image(width = 22, file = paste0(figdir, "/per-agent_heterogeneity_table.png"))

rm(list = c("mstyp", "mstyp_sum"))
###################### per agent - heterogeneity ########################

###################### FIGURES ########################
