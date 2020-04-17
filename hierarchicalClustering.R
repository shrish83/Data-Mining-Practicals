#heirarchicalClustering places each observation in own cluster, nad step wise merges clusters on distance basis
#distance matrix - matrix of distance b/ every point to any other
#load Data/ Read file
#install.packages("readxl")
library(readxl)
data_perfume <- read_xlsx("C:/Users/Swapnil Kumar Vaish/Documents/R/Data Mining/Q6/perfume_data.xlsx", col_names = F)
data_perfume <- as.data.frame(data_perfume)
View(data_perfume1)

#clustering is only on continuous variables so we must not include 'factor' variables
#normalize data for variables measured on different scale
data_perfume1 <- scale(data_perfume[,-1])
#data_perfume2<- data_perfume1/1000
str(data_perfume)
summary(data_perfume)


d <- dist(data_perfume1)
#d
?hclust
fitH <- hclust(d,"ward.D2") #use ward.D/complete etc methods to see different results of clustering
plot(fitH)
##dendogram - every observation labelled and clustered with most similar
#observation,higher up the tree is larger and larger clusters
#upto data scientist to decide where to cut the tree and get cluster

rect.hclust(fitH, k=5, border = "red")
cluster <- cutree(fitH,7) #we can cut off the tree at the desired number of clusters using cutree
cluster      #use different number other than 2(i used 7 after using 2) to see different results

plot(data_perfume1, col=cluster)
table(data_perfume$...1,cluster)

library(ggplot2)
ggplot(data_perfume, aes(data_perfume$...5,data_perfume$...21,color=data_perfume$...1))+
  geom_point(alpha = 0.4,size=3.5) + geom_point(col=cluster)

ggplot(data_perfume, aes(data_perfume$...5,data_perfume$...21,color=cluster))+
  geom_point(alpha = 0.4,size=3.5) + geom_point(col=cluster)
#+scale_color_manual(values = c("red","green","blue","black","tan","brown1","chilli"))



