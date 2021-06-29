# New governance figs


###################### governance - combined measures? ########################

# which governance measures are combined?

# first limit data to where the co-occurence of governance measures is specified
gov <- quotes_long[grep("governance - combined measures?: yes", quotes_long$Codes, fixed = TRUE),]
n_combined <- length(unique(gov$Document))

gov <- gov[gov$code_group == "per measure - measure",]
gov <- gov[!is.na(gov$code_group),]
gov <- gov[!is.na(gov$name),]

gov$class <- ""
gov <- dict_classification(sheet = gov, dct = undir_gov, clm = 16, class_clm = which(colnames(gov)=="class"))
colnames(gov)[which(colnames(gov)=="class")] <- "undir"
gov$class <- ""
gov2 <- dict_classification(sheet = gov, dct = NOTA_class[["Nodality"]], clm = which(colnames(gov)=="undir"), class_clm = which(colnames(gov)=="class"))
gov2 <- dict_classification(sheet = gov2, dct = NOTA_class[["Organisation"]], clm = which(colnames(gov)=="undir"), class_clm = which(colnames(gov)=="class"))
gov2 <- dict_classification(sheet = gov2, dct = NOTA_class[["Treasure"]], clm = which(colnames(gov)=="undir"), class_clm = which(colnames(gov)=="class"))
gov2 <- dict_classification(sheet = gov2, dct = NOTA_class[["Authority"]], clm = which(colnames(gov)=="undir"), class_clm = which(colnames(gov)=="class"))

###################

gov_sum <- gov %>% 
  group_by(ID, name, class) %>%
  count(ID, name, class)

selfcooc <- gov %>% 
  group_by(ID, class) %>%
  count(ID, class)

selfcooc <- selfcooc[selfcooc$n > 1,]

selfcooc <- selfcooc %>% group_by(class) %>% count(class)

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
# to do: create grouped visNetwork

x = 2
png(filename = paste0(figdir, "/governance_combined-measures_co-occurence.png"), width = 1300, height = 1300)
ggplot(Vedges, aes(x = from, y = to, col = width, label = width)) +
  geom_point(aes(size = width)) +
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
  scale_size(range = c(10,30), breaks = c(0:max(Vedges$width))) +
  scale_color_gradient(low = "lightblue", high = "blue") #+
# coord_flip()
dev.off()

rm(list = c("x", "gov", "gov_sum", "Vedges", "edges", "nodes", "Vnodes", "cluster_df", "graph", "cluster", "V", "Vdat", "Vlong"))
###################### governance - combined measures? ########################

###################### per measure - measure ########################
meas <- quotes_long[quotes_long$code_group == "per measure - measure",]
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
meas <- dict_classification(sheet = meas, dct = measure_class, clm = 16, class_clm = 17)
#colnames(meas)[16:17] <- c("governance_measure", "name")
meas_sum <- level2_count(sheet = meas)
meas_sum$class <- factor(meas_sum$class, levels = rev(unique(meas_sum$class)))
colnames(meas_sum) <- c("governance measures by type", "number")
meas_sum$proportion <- round(meas_sum$number/n_studies, 2)

meas_sum$number <- color_bar("red")(meas_sum$number)

ft_measgr <- meas_sum %>% #see https://haozhu233.github.io/kableExtra/awesome_table_in_html.html & http://cran.nexr.com/web/packages/kableExtra/vignettes/use_kableExtra_with_formattable.html 
  group_by(number) %>%
  kable("html", escape = F, caption = paste("Gathered from", n_studies, "papers.")) %>%
  kable_styling(font_size = 20) %>%
  kable_classic(full_width = F, html_font = "Cambria", position = "center")

ft_measgr %>% as_image(width = 22, file = paste0(figdir, "/per-measure_measure-grouped_table.png")) 

rm(list = c("meas", "meas_sum"))

# to do: make a list with all measures grouped per type
###################### per measure - measure ########################