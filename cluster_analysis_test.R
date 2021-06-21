# http://factominer.free.fr/factomethods/hierarchical-clustering-on-principal-components.html


library(FactoMineR)
data_path <- "./data"

# write.csv(all_dat, file = paste0(data_path, "/review_data_example.csv"))
# dataa <- read.csv(paste0(data_path, "/review_data_example.csv"))
alldata <- all_dat[1:30000, ]
alldata[is.na(alldata)] <- "no data"
alldata[alldata==""] <- "no data"


#Hierarchical clustering on principal components

res.mca = MCA(alldata, ncp=20, quanti.sup=9, quali.sup=c(10:39), graph=FALSE)
res.hcpc = HCPC(res.mca)

res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$category

# Multiple factor analysis
res = MFA(alldata, 
          group=c(3,2,2,2,9,2), 
          type=c(rep("s", 1), rep("n", 1)), 
          ncp=5, 
          name.group=c("papers - year","modelling - coupling?", "modelling - feedback-loop?","modelling - sensitivity analysis?", "taste","overall"), 
          num.group.sup=c(1,6))

