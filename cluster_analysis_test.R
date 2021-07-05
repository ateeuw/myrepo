
# libraries
library(refund)
library(factoextra)
library(FactoMineR)

# # data
# head(modelling) #this is my test dataset, containing nested data about models for the first 10 papers I reviewed
# 
# # analysis
# ?mfpca.sc
# 
# id <- modelling$Document
# Y <- modelling[,c(2:9)]
# 
# mfpca.DTI =  mfpca.sc(Y=Y, id = id, twoway = TRUE)
# 
# 
# 
# # example provided by https://rdrr.io/cran/refund/man/mfpca.sc.html
# data(DTI)
# DTI = subset(DTI, Nscans < 6)  ## example where all subjects have 6 or fewer visits
# id  = DTI$ID
# Y = DTI$cca
# mfpca.DTI =  mfpca.sc(Y=Y, id = id, twoway = TRUE)
# 
# 
# # correspondence analysis
# data(tea)
# res.mca = MCA(tea, quanti.sup=19, quali.sup=c(20:36))
# plot.MCA(res.mca, invisible=c("var","quali.sup"), cex=0.7)
# plot.MCA(res.mca, invisible=c("ind","quali.sup"), cex=0.7)
# plot.MCA(res.mca, invisible=c("ind"))
# plot.MCA(res.mca, invisible=c("ind", "var"))
# 
# dimdesc(res.mca)
# 
# qualcols <- 2:13

#agmod <- agmod[!(agmod$Document %in% c("Berger 2017")),]
govspfoagmod <- govspfoagmod[,-c(1)]
#foagmod <- foagmod %>% distinct()
qualcols <- c(1,3:23)

res.mca = MCA(govspfoagmod, quali.sup = c(qualcols), ncp = 5)
plot.MCA(res.mca, invisible=c("var","quali.sup"), cex=0.7, max.overlaps = 20000)
plot.MCA(res.mca, invisible=c("ind","quali.sup"), cex=0.7, max.overlaps = 20000)
plot.MCA(res.mca, invisible=c("ind"), cex=0.7, max.overlaps = 20000)
plot.MCA(res.mca, invisible=c("ind"), cex=0.7, max.overlaps = 20000, xlim = c(0,1), ylim = c(0,1))
plot.MCA(res.mca, invisible=c("ind", "var"), cex=0.7, max.overlaps = 20000)
#dimdesc(res.mca)

#png("./serverfigures/correspondence_ellipses.png", width = 2500, height = 1500)
plotellipses(res.mca,keepvar="all", max.overlaps = 20000) #this takes a long time ~~~~~~~~~~~
#dev.off()

library("factoextra")
eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

var <- get_mca_var(res.mca)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca, col.var="black", shape.var = 15,
             repel = TRUE)

fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

fviz_mca_var(res.mca, alpha.var="cos2",
             repel = TRUE,
             ggtheme = theme_minimal())

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

fviz_cos2(res.mca, choice = "var", axes = 1:2)

# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)

# Total contribution to dimension 1 and 2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)

fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

# Cos2 of individuals
fviz_cos2(res.mca, choice = "ind", axes = 1:2, top = 20)
# Contribution of individuals to the dimensions
fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 20)

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "class", # color by groups 
             palette = rainbow(12),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "per model - type", # color by groups 
             palette = rainbow(12),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "per model - domain", # color by groups 
             palette = rainbow(4),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "agent - representation", # color by groups 
             palette = rainbow(5),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "modelling - coupling?", # color by groups 
             palette = rainbow(3),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "modelling - sensitivity analysis?", # color by groups 
             palette = rainbow(3),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "modelling - data", # color by groups 
             palette = rainbow(17),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

# res = MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)), ncp=5, name.group=c("origin","odor","visual","odor.after.shaking", "taste","overall"), num.group.sup=c(1,6))
# 
# res = MFA(agmod)
