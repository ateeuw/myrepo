# This script is based on a script originally written by Markus A. Meyer for his publication 'The role of resilience in food system studies in 
# low- and middle-income countries' (2020), 10.1016/j.gfs.2020.100356

# install.packages("dbscan")
# install.packages("dplyr")
# install.packages("FactoMineR")
# install.packages("fmsb")
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("gridExtra")
# install.packages("proxy")
# install.packages("SnowballC")
# install.packages("tm")
# install.packages("tidyr")

# Load libraries ###################
library("dbscan")
library("dplyr")
library("fmsb")
library("FactoMineR")
library("ggplot2")
library("ggpubr")
library("grid")
library("gridExtra")
library("NbClust")
library("proxy")
library("readxl")
library("reshape2")
library("SnowballC")
library("tm")
library("tidyr")
# Load libraries ###################

# Load data ###################
datadir <- "../Atlas_export_sheets"
review_all_columns <- read_excel(paste0(datadir, "/", "all_quotes.xlsx")) # read_excel("C:/Users/MeyerMA/OneDrive - Universiteit Twente/Paper/Review paper/Import_R_case_studies.xlsx")


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
#

quotes_wide <- quotes_long %>% spread(code_group, name)
code_groups <- colnames(quotes_wide)
code_groups <- c("papers - year", 
                 "agent - representation", "per agent - agent", 
                 "food system - commodity", "food system - echelon", "nonfood system - commodity",
                 "modelling - aim", "modelling - coupling?", "modelling - data", "modelling - feedback-loop?", "modelling - sensitivity analysis?", "modelling - validation?", "per model - domain", "per model - subdomain", "per model - subtype", "per model - type", 
                 "per effect - FS indicator", 
                 "per measure - formulation", "per measure - measure", "per measure - objective", "per measure - scale", "per measure - spatially targeted?", "per measure - target implementer", "per measure - type", "per measure - type 2",
                 "spatial & temporal - country", "spatial & temporal - ref scale", "spatial & temporal - representation", "spatial & temporal - spatial extent [m2]", "spatial & temporal - spatial extent", "spatial & temporal - spatial resolution [m2]", "spatial & temporal - spatial resolution", "spatial & temporal - temporal extent [d]", "spatial & temporal - temporal extent", "spatial & temporal - temporal resolution [d]", "spatial & temporal - temporal resolution [d]")

#quotes_wide <- quotes_wide[,c(-1,-2, -4:-14)]

keep <- which(colnames(quotes_wide) %in% c(code_groups, "name_id", "Document"))

quotes_wide <- quotes_wide[,keep]

# Pre-process data ########################

# processed data: 
all_dat


### clustering
all_dat[is.na(all_dat)] <- ""
#all_dat[all_dat==""] <- 0
all_dat

library(FactoMineR)
data(tea)

res.mca = MCA(all_dat, ncp=20, quanti.sup=8, quali.sup=c(9:31), graph=FALSE)
res.hcpc = HCPC(res.mca)

# df_clust_rev=data.frame(review_all_columns$Scale,review_all_columns$Producer,review_all_columns$Processor,review_all_columns$Logistics,review_all_columns$Sales,review_all_columns$Consumer,
#                         review_all_columns$Social,review_all_columns$Environmental,review_all_columns$Economic,review_all_columns$Institutions,
#                         product_df_wide[,-1],tfidf.res_issue.matrix,tfidf.context.matrix,tfidf.focus.matrix,tfidf.item.matrix,tfidf.method.matrix,tfidf.strand.matrix)
# 
# df_clust_rev_name=data.frame(review_all_columns[,6:10],review_all_columns[,12:15],
#                         product_df_wide_name[,-1],tfidf.res_issue.matrix,tfidf.context.matrix,tfidf.focus.matrix,tfidf.item.matrix,tfidf.method.matrix,tfidf.strand.matrix)


#df_clust_rev=data.frame(sapply(df_clust_rev, function (x) as.numeric(as.character(x))))

dist.all.matrix = proxy::dist(all_dat, item = "cosine")

summary(all_dat)
MFA_rev$group

MFA_rev=MFA(df_clust_rev, group=c(1,5,4,28,6,6,10,8,9,8), type=c(rep("n",3),rep("s",7)), ncp=5, 
    name.group=c("scale","value chain","sustainability","product", "issue","context","focus","item","method","strand"))

MFA_rev_clus=HCPC(MFA_rev, nb.clust = 0, min = 3, max = 15, graph = TRUE)
summary(MFA_rev_clus$data.clust$clust)
MFA_rev_clus$call$t$inert.gain
plot(MFA_rev_clus)

df_clust_rev_agg_num=aggregate(df_clust_rev[,11:85], list(MFA_rev_clus$data.clust$clust), mean)
MFA_rev_clus$data.clust$clust
paper_cluster=data.frame(review_all_columns,MFA_rev_clus$data.clust$clust)
df_clust_rev_clus_name=data.frame(df_clust_rev_name,MFA_rev_clus$data.clust$clust)
write.csv2(paper_cluster, "C:/Users/MeyerMA/OneDrive - Universiteit Twente/Paper/Review paper/paper_analyzed_cluster.csv") 

write.csv2(df_clust_rev_name, "C:/Users/MeyerMA/OneDrive - Universiteit Twente/Paper/Review paper/df_clust_rev_name.csv") 


#scale
df_clust_rev_scale=data.frame(sapply(df_clust_rev[,1], function (x) as.numeric(as.character(x))),MFA_rev_clus$data.clust$clust)

myfun <- function(x) unique(x)[which.max(table(x))]
df_clust_rev_scale[ , moda := myfun(df_clust_rev_scale$sapply.df_clust_rev...1...function.x..as.numeric.as.character.x...), by = df_clust_rev_scale$MFA_rev_clus.data.clust.clust]
df_clust_rev_scale

#cat except for scale
df_clust_rev_cat=data.frame(sapply(df_clust_rev[,2:10], function (x) as.numeric(as.character(x))))
df_clust_rev_agg_cat=aggregate(df_clust_rev_cat, list(MFA_rev_clus$data.clust$clust), mean)

df_clust_rev_cat_dat=c(as.numeric(as.character(c(df_clust_rev_agg_cat[1,2:10],df_clust_rev_agg_cat[2,2:10],df_clust_rev_agg_cat[3,2:10]))))

df_clust_rev_agg_cat_t=data.frame(unlist(substring(names(df_clust_rev_agg_cat[,2:10]), 20)),
                                        as.character(c(rep(df_clust_rev_agg_cat[1,1],length(df_clust_rev_agg_cat[1,2:10])),rep(df_clust_rev_agg_cat[2,1],length(df_clust_rev_agg_cat[2,2:10])),rep(df_clust_rev_agg_cat[3,1],length(df_clust_rev_agg_cat[3,2:10])))),
                                        as.numeric(as.character(df_clust_rev_cat_dat)))
names(df_clust_rev_agg_cat_t)=c("Theme","Cluster","Share")
rownames(df_clust_rev_agg_cat_t) <- c(1:length(df_clust_rev_agg_cat_t[,1]))
df_clust_rev_agg_cat_t
df_clust_rev_agg_cat_t$Theme=rep(factor(df_clust_rev_agg_cat_t$Theme[1:9], levels = substring(names(df_clust_rev_agg_cat[,2:10]), 20)),3)

ggplot(data=df_clust_rev_agg_cat_t,  aes(x=Theme, y=Share,group= Cluster, colour=Cluster))+
  ggplot2::annotate("text", x = 1, y = 0:1, label = 0:1, hjust = 1)+
  geom_polygon(size = 1, alpha= 0.2) + 
  scale_y_continuous(labels = NULL) +
  scale_x_discrete() +
  scale_color_manual(values= c("Red", "Blue","Black"))+
  scale_fill_manual(values= c("Red", "Blue","Black"))+
  theme_light(base_size=16)+
  coord_polar(start = -pi/9)


#quanti, esp. product
df_clust_rev_agg_quanti=aggregate(df_clust_rev[,11:85], list(MFA_rev_clus$data.clust$clust), mean)

df_clust_rev_product_dat=c(as.numeric(as.character(c(df_clust_rev_agg_product[1,2:29],df_clust_rev_agg_product[2,2:29],df_clust_rev_agg_product[3,2:29]))))

df_clust_rev_agg_product_t=data.frame(unlist(names(df_clust_rev_agg_quanti[,2:29])),
                                  as.character(c(rep(df_clust_rev_agg_quanti[1,1],length(df_clust_rev_agg_quanti[1,2:29])),rep(df_clust_rev_agg_quanti[2,1],length(df_clust_rev_agg_quanti[2,2:29])),rep(df_clust_rev_agg_quanti[3,1],length(df_clust_rev_agg_quanti[3,2:29])))),
                                  as.numeric(as.character(df_clust_rev_product_dat)))

names(df_clust_rev_agg_product_t)=c("Product","Cluster","Share")
df_clust_rev_agg_product_t$Product=gsub("\\.", " ", df_clust_rev_agg_product_t$Product) 
df_clust_rev_agg_product_t$Product=tolower(df_clust_rev_agg_product_t$Product)
df_clust_rev_agg_product_t$Product=gsub(" and derived products", "", df_clust_rev_agg_product_t$Product) 
df_clust_rev_agg_product_t$Product=gsub("products from ", "", df_clust_rev_agg_product_t$Product) 
df_clust_rev_agg_product_t$Product=gsub(" and other crops", "", df_clust_rev_agg_product_t$Product)
df_clust_rev_agg_product_t$Product=gsub(" and cereal products", "", df_clust_rev_agg_product_t$Product)
df_clust_rev_agg_product_t$Product=gsub("and", "&", df_clust_rev_agg_product_t$Product)
df_clust_rev_agg_product_t$Product=gsub("fibres of vegetal & animal origin", "fibres", df_clust_rev_agg_product_t$Product)
df_clust_rev_agg_product_t$Product=gsub("stimulant crops", "stimulants", df_clust_rev_agg_product_t$Product)


names_product=ifelse(df_clust_rev_agg_product_t$Share < 0.10, "", df_clust_rev_agg_product_t$Product)
names_product_compl=ifelse(names_product[1:28]=="", ifelse(names_product[29:56]=="",names_product[57:84],names_product[29:56]),names_product[1:28])
  
  
rownames(df_clust_rev_agg_product_t) <- c(1:length(df_clust_rev_agg_product_t[,1]))
df_clust_rev_agg_product_t

df_clust_rev_agg_product_t$Product=rep(factor(df_clust_rev_agg_product_t$Product[1:28], levels = c(df_clust_rev_agg_product_t$Product[1:28])),3)

ggplot(data=df_clust_rev_agg_product_t,  aes(x=Product, y=Share,group= Cluster, colour=Cluster))+
  ggplot2::annotate("text", x = 1, y = 0:1, label = 0:1, hjust = 1)+
  geom_polygon(size = 1, alpha= 0.2) +
  scale_y_continuous(labels = NULL) +
  scale_x_discrete(labels = names_product_compl)+
  scale_color_manual(values= c("Red", "Blue","Black"))+
  scale_fill_manual(values= c("Red", "Blue","Black"))+
  theme_light(base_size=16)+
  coord_polar(start = -pi/9)

gt <- ggplot_gtable(ggplot_build(plot_product))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

#
#geom_text(data=subset(df_clust_rev_agg_product_t, Share > 0.15),
#          aes(Share,label=Product)) +

#quanti, esp. issue
df_clust_rev_issue_dat=c(as.numeric(as.character(c(df_clust_rev_agg_quanti[1,30:35],df_clust_rev_agg_quanti[2,30:35],df_clust_rev_agg_quanti[3,30:35]))))

df_clust_rev_agg_issue_t=data.frame(unlist(names(df_clust_rev_agg_quanti[,30:35])),
                                      as.character(c(rep(df_clust_rev_agg_quanti[1,1],length(df_clust_rev_agg_quanti[1,30:35])),rep(df_clust_rev_agg_quanti[2,1],length(df_clust_rev_agg_quanti[2,30:35])),rep(df_clust_rev_agg_quanti[3,1],length(df_clust_rev_agg_quanti[3,30:35])))),
                                      as.numeric(as.character(df_clust_rev_issue_dat)))
names(df_clust_rev_agg_issue_t)=c("Issue","Cluster","Score")
rownames(df_clust_rev_agg_issue_t) <- c(1:length(df_clust_rev_agg_issue_t[,1]))
df_clust_rev_agg_issue_t
df_clust_rev_agg_issue_t$Issue=rep(factor(gsub("\\..*","",df_clust_rev_agg_issue_t$Issue[1:6]), levels = gsub("\\..*","",names(df_clust_rev_agg_quanti[,30:35]))),3)

ggplot(data=df_clust_rev_agg_issue_t,  aes(x=Issue, y=Score,group= Cluster, colour=Cluster))+
  ggplot2::annotate("text", x = 1,  y=c(0,0.5), label=c("0","0.5"), hjust = 1)+
  geom_polygon(size = 1, alpha= 0.2) + 
  scale_y_continuous(labels = NULL) +
  scale_x_discrete() +
  scale_color_manual(values= c("Red", "Blue","Black"))+
  scale_fill_manual(values= c("Red", "Blue","Black"))+
  theme_light(base_size=16)+
  coord_polar(start = -pi/9)

#quanti, esp. context
df_clust_rev_context_dat=c(as.numeric(as.character(c(df_clust_rev_agg_quanti[1,36:41],df_clust_rev_agg_quanti[2,36:41],df_clust_rev_agg_quanti[3,36:41]))))

df_clust_rev_agg_context_t=data.frame(unlist(names(df_clust_rev_agg_quanti[,36:41])),
                                    as.character(c(rep(df_clust_rev_agg_quanti[1,1],length(df_clust_rev_agg_quanti[1,36:41])),rep(df_clust_rev_agg_quanti[2,1],length(df_clust_rev_agg_quanti[2,36:41])),rep(df_clust_rev_agg_quanti[3,1],length(df_clust_rev_agg_quanti[3,36:41])))),
                                    as.numeric(as.character(df_clust_rev_context_dat)))
names(df_clust_rev_agg_context_t)=c("Context","Cluster","Score")
rownames(df_clust_rev_agg_context_t) <- c(1:length(df_clust_rev_agg_context_t[,1]))
df_clust_rev_agg_context_t
df_clust_rev_agg_context_t$Context=rep(factor(gsub("\\..*","",df_clust_rev_agg_context_t$Context[1:6]), levels = gsub("\\..*","",names(df_clust_rev_agg_quanti[,36:41]))),3)

ggplot(data=df_clust_rev_agg_context_t,  aes(x=Context, y=Score,group= Cluster, colour=Cluster))+
  ggplot2::annotate("text", x = 1, y=c(0,0.5), label=c("0","0.5"), hjust = 1)+
  geom_polygon(size = 1, alpha= 0.2) + 
  scale_y_continuous(labels = NULL) +
  scale_x_discrete() +
  scale_color_manual(values= c("Red", "Blue","Black"))+
  scale_fill_manual(values= c("Red", "Blue","Black"))+
  theme_light(base_size=16)+
  coord_polar(start = -pi/9)

#quanti, esp. focus
df_clust_rev_focus_dat=c(as.numeric(as.character(c(df_clust_rev_agg_quanti[1,42:51],df_clust_rev_agg_quanti[2,42:51],df_clust_rev_agg_quanti[3,42:51]))))

df_clust_rev_agg_focus_t=data.frame(unlist(names(df_clust_rev_agg_quanti[,42:51])),
                                      as.character(c(rep(df_clust_rev_agg_quanti[1,1],length(df_clust_rev_agg_quanti[1,42:51])),rep(df_clust_rev_agg_quanti[2,1],length(df_clust_rev_agg_quanti[2,42:51])),rep(df_clust_rev_agg_quanti[3,1],length(df_clust_rev_agg_quanti[3,42:51])))),
                                      as.numeric(as.character(df_clust_rev_focus_dat)))
names(df_clust_rev_agg_focus_t)=c("Focus","Cluster","Score")
rownames(df_clust_rev_agg_focus_t) <- c(1:length(df_clust_rev_agg_focus_t[,1]))
df_clust_rev_agg_focus_t
#df_clust_rev_agg_focus_t$Focus=gsub("\\..*","",df_clust_rev_agg_focus_t$Focus)
df_clust_rev_agg_focus_t$Focus=rep(factor(gsub("\\..*","",df_clust_rev_agg_focus_t$Focus[1:10]), levels = gsub("\\..*","",names(df_clust_rev_agg_quanti[,42:51]))),3)


ggplot(data=df_clust_rev_agg_focus_t,  aes(x=Focus, y=Score,group= Cluster, colour=Cluster))+
  ggplot2::annotate("text",x = 1, y=c(0,0.5), label=c("0","0.5"), hjust = 1)+
  geom_polygon(size = 1, alpha= 0.2) + 
  scale_y_continuous(labels = NULL) +
  scale_x_discrete() +
  scale_color_manual(values= c("Red", "Blue","Black"))+
  scale_fill_manual(values= c("Red", "Blue","Black"))+
  theme_light(base_size=16)+
  coord_polar(start = -pi/9)

#quanti, esp. item
df_clust_rev_item_dat=c(as.numeric(as.character(c(df_clust_rev_agg_quanti[1,52:59],df_clust_rev_agg_quanti[2,52:59],df_clust_rev_agg_quanti[3,52:59]))))

df_clust_rev_agg_item_t=data.frame(unlist(names(df_clust_rev_agg_quanti[,52:59])),
                                    as.character(c(rep(df_clust_rev_agg_quanti[1,1],length(df_clust_rev_agg_quanti[1,52:59])),rep(df_clust_rev_agg_quanti[2,1],length(df_clust_rev_agg_quanti[2,52:59])),rep(df_clust_rev_agg_quanti[3,1],length(df_clust_rev_agg_quanti[3,52:59])))),
                                    as.numeric(as.character(df_clust_rev_item_dat)))
names(df_clust_rev_agg_item_t)=c("Item","Cluster","Score")
rownames(df_clust_rev_agg_item_t) <- c(1:length(df_clust_rev_agg_item_t[,1]))
df_clust_rev_agg_item_t
df_clust_rev_agg_item_t$Item=rep(factor(gsub("\\..*","",df_clust_rev_agg_item_t$Item[1:8]), levels = gsub("\\..*","",names(df_clust_rev_agg_quanti[,52:59]))),3)

ggplot(data=df_clust_rev_agg_item_t,  aes(x=Item, y=Score,group= Cluster, colour=Cluster))+
  ggplot2::annotate("text", x = 1, y=c(0,1), label=c("0","1"), hjust = 1,size=4)+
  geom_polygon(size = 1, alpha= 0.2,aes(size=35)) + 
  scale_y_continuous(labels = NULL) +
  scale_x_discrete() +
  scale_color_manual(values= c("Red", "Blue","Black"))+
  scale_fill_manual(values= c("Red", "Blue","Black"))+
  theme_light(base_size=16)+
  coord_polar(start = -pi/9)

#quanti, esp. method
df_clust_rev_method_dat=c(as.numeric(as.character(c(df_clust_rev_agg_quanti[1,60:68],df_clust_rev_agg_quanti[2,60:68],df_clust_rev_agg_quanti[3,60:68]))))

df_clust_rev_agg_method_t=data.frame(unlist(names(df_clust_rev_agg_quanti[,60:68])),
                                   as.character(c(rep(df_clust_rev_agg_quanti[1,1],length(df_clust_rev_agg_quanti[1,60:68])),rep(df_clust_rev_agg_quanti[2,1],length(df_clust_rev_agg_quanti[2,60:68])),rep(df_clust_rev_agg_quanti[3,1],length(df_clust_rev_agg_quanti[3,60:68])))),
                                   as.numeric(as.character(df_clust_rev_method_dat)))
names(df_clust_rev_agg_method_t)=c("Method","Cluster","Score")
rownames(df_clust_rev_agg_method_t) <- c(1:length(df_clust_rev_agg_method_t[,1]))
df_clust_rev_agg_method_t
df_clust_rev_agg_method_t$Method=rep(factor(gsub("\\..*","",df_clust_rev_agg_method_t$Method[1:9]), levels = gsub("\\..*","",names(df_clust_rev_agg_quanti[,60:68]))),3)

ggplot(data=df_clust_rev_agg_method_t,  aes(x=Method, y=Score,group= Cluster, colour=Cluster))+
  ggplot2::annotate("text", x = 1, y=c(0,1), label=c("0","1"), hjust = 1)+
  geom_polygon(size = 1, alpha= 0.2) + 
  scale_y_continuous(labels = NULL) +
  scale_x_discrete() +
  scale_color_manual(values= c("Red", "Blue","Black"))+
  scale_fill_manual(values= c("Red", "Blue","Black"))+
  theme_light(base_size=16)+
  coord_polar(start = -pi/9)

#quanti, esp. strand
df_clust_rev_strand_dat=c(as.numeric(as.character(c(df_clust_rev_agg_quanti[1,69:76],df_clust_rev_agg_quanti[2,69:76],df_clust_rev_agg_quanti[3,69:76]))))

df_clust_rev_agg_strand_t=data.frame(unlist(names(df_clust_rev_agg_quanti[,69:76])),
                                     as.character(c(rep(df_clust_rev_agg_quanti[1,1],length(df_clust_rev_agg_quanti[1,69:76])),rep(df_clust_rev_agg_quanti[2,1],length(df_clust_rev_agg_quanti[2,69:76])),rep(df_clust_rev_agg_quanti[3,1],length(df_clust_rev_agg_quanti[3,69:76])))),
                                     as.numeric(as.character(df_clust_rev_strand_dat)))
names(df_clust_rev_agg_strand_t)=c("Strand","Cluster","Score")
rownames(df_clust_rev_agg_strand_t) <- c(1:length(df_clust_rev_agg_strand_t[,1]))
df_clust_rev_agg_strand_t
df_clust_rev_agg_strand_t$Strand=rep(factor(gsub("\\..*","",df_clust_rev_agg_strand_t$Strand[1:8]), levels = gsub("\\..*","",names(df_clust_rev_agg_quanti[,69:76]))),3)

ggplot(data=df_clust_rev_agg_strand_t,  aes(x=Strand, y=Score,group= Cluster, colour=Cluster))+
  ggplot2::annotate("text", x = 1, y=c(0,1), label=c("0","1"), hjust = 1)+
  geom_polygon(size = 1, alpha= 0.2) + 
  scale_y_continuous(labels = NULL) +
  scale_x_discrete() +
  scale_color_manual(values= c("Red", "Blue","Black"))+
  scale_fill_manual(values= c("Red", "Blue","Black"))+
  theme_light(base_size=16)+
  coord_polar(start = -pi/9)


#res issue reclass manual
Import_R_resilience_issues_reclass= Import_R_resilience_issues_reclass
Import_R_resilience_issues_reclass_ca=CA(Import_R_resilience_issues_reclass[,2:13],col.sup = 13:52,excl=10)
Import_R_resilience_issues_reclass_unique_issue<-unique(Import_R_resilience_issues_reclass[,2:13])
Import_R_resilience_issues_reclass_unique_issue
