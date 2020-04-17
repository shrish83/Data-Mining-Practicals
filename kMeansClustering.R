#kMeans Clustering
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

fitK <- kmeans(data_perfume1,5)
fitK
fitK$size
fitK$cluster

str(fitK)
par(mar = rep(2,4))
plot(data_perfume1,col=fitK$cluster) #assigns colour to different clusters based on K assigned

#choosing K
k <- list() #initialize an empty list K
for (i in 1:10) {
  k[[i]] <- kmeans(data_perfume1,i)
} # will iterate through 1 to 10 and fit kmeans model for each value and will be saved to a list called k
k

#for every k mean, calculate between sum of square/total sum of square and then plot
betweenss_totss <- list()
for (i in 1:10) {
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}
plot(1:10, betweenss_totss, type = "b", ylab = "between ss/total ss", xlab = "Cluster(k)")
for (i in 1:10) {
  plot(data_perfume1, col = k[[i]]$cluster)
}


#compare the clusters with the perfume types
#table(data_perfume,fitK$cluster)
library(ggplot2)
ggplot(data_perfume, aes(data_perfume$...3, data_perfume$...27,color = data_perfume$...1)) + geom_point()
ggplot(data_perfume, aes(data_perfume$...3, data_perfume$...27,color = fitK$cluster)) + geom_point()


