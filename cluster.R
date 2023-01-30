library(readxl)
library(corrplot)
library(ggcorrplot)
library(tidyverse)
library(magrittr)
library(cluster)
library(cluster.datasets)
library(cowplot)
library(NbClust)
library(clValid)
library(ggfortify)
library(clustree)
library(dendextend)
library(factoextra)
library(FactoMineR)
library(GGally)
library(ggiraphExtra)
library(knitr)
library(kableExtra)

data <- as.data.frame(read_excel("D:/DATA MULVAR CLUSTER ONLY.xlsx"))
row.names(data) <- c(data[,1])
data_bps <- data[,-1]
names <- c(1:13)
data_bps[,names] <- lapply(data_bps[,names] , as.numeric)
str(data_bps)

corr <- round(cor(data_bps),2)
win.graph()
ggcorrplot(corr)

#scaling the data
data_scale = scale(data_bps)
data_scale

#Hierarchiecal Cluster

require(stats)
# Distance matrix computation
res.dist = dist(x = data_scale,
                method = "euclidean")
# Print distance matrix 
output = as.matrix(res.dist)[1:nrow(data_bps), 1:nrow(data_bps)]
round(output, digits = 3)

d = as.dist(output)
# Cluster dendrogram using stats package
require(stats)
res.hc <- hclust(d = res.dist,
                 method = "median")
require(grDevices)
require(factoextra)
plot(res.hc)
rect.hclust(res.hc , k = 3, border = 2:6)
fviz_dend(x = res.hc, cex = 0.7, lwd = 0.7) 
fviz_dend(res.hc, cex = 0.8, k=2, 
          k_colors = c("blue","yellow"),
          horiz = TRUE
          )

library(factoextra)
library(ggplot2)
kmeans<-kmeans(data_scale,5,iter.max=10)
kmeans$centers
fviz_cluster(kmeans,data_scale)

fviz_nbclust(data_scale, kmeans, method = "silhouette", k.max = 5) 
+ theme_minimal() + ggtitle("The Silhouette Plot")

intern <- clValid(data_scale, nClust = 2:24, 
                  clMethods = c("hierarchical","kmeans","pam"), validation = "internal")
# Summary
summary(intern) %>% kable() %>% kable_styling()

# Compute dissimilarity matrix with euclidean distances
d <- dist(data_scale, method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "median" )
# Cut tree into 5 groups
grp <- cutree(res.hc, k = 2)
# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = 2, border = 2) # add rectangle

require(QuantPsyc)
library(energy)
data<- read.csv("D:/archive/iris.csv")
round(mult.norm(data[,c(2:5)])$mult.test,3)
mvnorm.etest(data[,c(2:5)],R=100)