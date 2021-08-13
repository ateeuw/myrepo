# testing MFA with Markus

# Resources:
# http://factominer.free.fr/factomethods/multiple-factor-analysis.html
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/
# http://www.sthda.com/english/articles/22-principal-component-methods/73-multiple-factor-analysis-course-using-factominer/

# large test dataset:
head(govspfoagmod)

# small test dataset:
head(agmod)

# libraries
library(factoextra)
library(FactoMineR)
library(beepr)

# res = MFA(wine, 
#           group=c(2,5,3,10,9,2), 
#           type=c("n",rep("s",5)), 
#           ncp=5, 
#           name.group=c("origin","odor","visual","odor.after.shaking", "taste","overall"), 
#           num.group.sup=c(1,6))

test = MFA(agmod[,2:ncol(agmod)], #leave document out 
          group=c(1,8), 
          type=rep("n", 2), 
          ncp=5, 
          name.group=c("agent","modelling"),
          row.w = c(rep(0.1, 100), rep(0.2, 100), rep(0.6, 200), rep(1, 207))) #give weight to rows according to nr of rows per paper

plot(test,choix="ind",partial="all")

# solve memory issue
memory.limit(size = 999999999999)
memory.size(max=999999999999)
Sys.setenv('R_MAX_MEM_SIZE'=100000000000)
Sys.getenv('R_MAX_MEM_SIZE')

print(paste("started MFA at:", Sys.time()))
test2 = MFA(govspfoagmod[1:1000,2:ncol(govspfoagmod)], #leave document out 
           group=c(1,8, 2, 5, 6), 
           type=rep("n", 5), 
           ncp=5, #number of dimensions kept in the results (by default 5)
           name.group=c("agent","modelling", "food", "spatial", "governance")) #give weight to rows according to nr of rows per paper
print(paste("ended MFA at:", Sys.time()))

plot(test2,choix="ind",partial="all")

# make a vector that weighs each Document according to the number of rows each document has
nr_rows <- govspfoagmod %>% group_by(Document) %>% count(Document)
head(nr_rows)
max_nr_rows <- max(nr_rows$n)
max_nr_rows
min_nr_rows <- min(nr_rows$n)
min_nr_rows

nr_rows$weight <- (max_nr_rows - nr_rows$n)/(max_nr_rows - min_nr_rows)
head(nr_rows)
rowweights <- rep(nr_rows$weight, nr_rows$n)

print(paste("started MFA at:", Sys.time()))
test2 = MFA(govspfoagmod[1:2000,1:ncol(govspfoagmod)], #leave document out 
            group=c(1, 1, 8, 2, 5, 6), 
            type=rep("n", 6), 
            ncp=5, #number of dimensions kept in the results (by default 5)
            name.group=c("document", "agent","modelling", "food", "spatial", "governance"),
            row.w = rowweights[1:2000]) #give weight to rows according to nr of rows per paper
print(paste("ended MFA at:", Sys.time()))

plot(test2,choix="ind",partial="all")

fviz_mfa_ind(test2, 
             habillage = "Document", # color by groups 
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
) 

library(Factoshiny)


#test3 <- Factoshiny(govspfoagmod[1:2000,1:ncol(govspfoagmod)])

res.HCPC<-HCPC(test2,nb.clust=6,consol=FALSE,graph=TRUE)
res.HCPC<-HCPC(test2,nb.clust=-1,consol=FALSE,graph=TRUE)
