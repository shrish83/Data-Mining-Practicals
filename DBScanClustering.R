#density Based Clustering
install.packages("dbscan")
library(dbscan)

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

?kNNdistplot
kNNdistplot(data_perfume1,k=13) #to decide value of eps
abline(h=12.7,col="red",lty=2)
#abline(h=3.9,col="red",lty=2)  #try different values of minPts
fitD <- dbscan(data_perfume1,eps = 12.7,minPts = 13)
#fitD <- dbscan(data_perfume1,eps = 5,minPts = 8)
fitD
plot(data_perfume1,col=fitD$cluster)



# iris
# irisScaled <- scale(iris[,-5])
# irisScaled
# kNNdistplot(irisScaled,k=3) # to decide value of eps
# ?abline
# abline(h=0.7,col="red",lty=2)
# fitD<-dbscan(irisScaled,eps =0.7 ,minPts =5 )





















