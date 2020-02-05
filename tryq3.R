#wine
wine_dat<-data.frame(a=c(1,2,3,4,5), b=c(2,3,4,5,6))
wine_dat
summary(wine_dat)

mean(wine_dat$a)
mean(wine_dat$b)
sd(wine_dat$a)
sd(wine_dat$b)


install.packages("caret")
library(caret)

wine_dat1=wine_dat
wine_dat1
pre_wine_dat1=preProcess(wine_dat1,method=c("center","scale"))
pre_wine_dat1
summary(pre_wine_dat1)

mean_wine_dat=predict(pre_wine_dat1,wine_dat1)
mean_wine_dat

summary(mean_wine_dat)
sd(mean_wine_dat$a)



#iris
View(iris)
summary(iris)
mean(iris$Sepal.Length)
mean(iris$Sepal.Width)
mean(iris$Petal.Length)
mean(iris$Petal.Width)
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)

iris1 = iris
summary(iris1)

preProcessed_iris1 = preProcess(iris1[,1:4], method = c("center","scale"))
preProcessed_iris1
summary(preProcessed_iris1)

toPredict_iris1=predict(preProcessed_iris1,iris1)
toPredict_iris1

summary(toPredict_iris1)
sd(as.numeric(unlist(toPredict_iris1[,1:4])))  ##close to 1

round(sd(as.numeric(unlist(toPredict_iris1[,1:4])))) ##sd equals to 1


