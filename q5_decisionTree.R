real_df <- read.csv("C:/Users/Swapnil Kumar Vaish/Documents/R/Data Mining/Q5/BCW_data.csv")
View(real_df)

real_df$X <- NULL

temp_df <- real_df
temp_df$diagnosis <- as.factor(temp_df$diagnosis)


num_rows <- nrow(temp_df)

#install.packages("caTools")
library(caTools)

##Hold Out method with 75% of training set and 25% of test set
set.seed(1011)
split <- sample.split(temp_df$diagnosis, SplitRatio = 0.75)
training_set = subset(temp_df, split == T)
test_set = subset(temp_df,split == F) 
dim(training_set)
dim(test_set)

#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
#?rpart

dtm <- rpart(diagnosis~., training_set,method = "class")
#dtm       
#plot(dtm)
#for better illustration
rpart.plot(dtm)
#rpart.plot(dtm,type = 4,extra = 101 )

p<- predict(dtm,test_set,type = "class")


#install.packages("caret", dependencies = TRUE)
library(caret)
#install.packages("e1071")
#library(e1071)
confusionMatrix(test_set[,2],p)
#Accuracy = 0.9202


##Hold out method with 66.66% of training and 33.33% o tet set
set.seed(2010)
split<-sample.split(temp_df$diagnosis, SplitRatio = 0.666)
training_set<-subset(temp_df, split == T)
test_set<-subset(temp_df, split == F)
dim(training_set)
dim(test_set)
dtm<-rpart(diagnosis~.,training_set, method = "class")

p<-predict(dtm, test_set, type = "class")

confusionMatrix(test_set[,2], p)
#Accuracy = 0.9232



##Random Subsampling with 75% of training set and 25% of test set
acc <- c()
set.seed(3333)
for(i in 1:10){
  s <- sample.split(temp_df$diagnosis, SplitRatio = 0.75)
  training_set<-subset(temp_df, split == T)
  test_set<-subset(temp_df, split == F)
  dim(training_set)
  dim(test_set)
  dtm<-rpart(diagnosis~.,training_set, method = "class")
  
  p<-predict(dtm, test_set, type = "class")
  
  dti<-confusionMatrix(test_set[,2], p)
  acc<-c(acc,dti$overall['Accuracy'])
}

cat("Average of accuracy: ",mean(acc))   

#Random Subsampling with 66.6% of training set and 33.3% of test set
acc <- c()
set.seed(4799)
for(i in 1:10){
  s <- sample.split(temp_df$diagnosis, SplitRatio = 0.666)
  training_set<-subset(temp_df, split == T)
  test_set<-subset(temp_df, split == F)
  dim(training_set)
  dim(test_set)
  dtm<-rpart(diagnosis~.,training_set, method = "class")
  
  p<-predict(dtm, test_set, type = "class")
  
  dti<-confusionMatrix(test_set[,2], p)
  acc<-c(acc,dti$overall['Accuracy'])
}

cat("Average of accuracy: ",mean(acc))  
#Accuracy = 0.9421053



##Cross Validation with 75% of training set and 25% of test set
set.seed(5256)
split <- sample.split(temp_df$diagnosis, SplitRatio = 0.75)
training_set<-subset(temp_df, split == T)
test_set<-subset(temp_df, split == F)
model<-train(diagnosis~., data = training_set, method='rpart', trControl = trainControl(method = 'cv', number = 10))

p<-predict(model, test_set, method='class')

confusionMatrix(test_set[,2], p)
#Accuracy = 0.8415

#Cross Validation with 66.6% of training set and 33.3% of test set
set.seed(6806)
split <- sample.split(temp_df$diagnosis, SplitRatio = 0.666)
training_set<-subset(temp_df, split == T)
test_set<-subset(temp_df, split == F)
model<-train(diagnosis~., data = training_set, method='rpart', trControl = trainControl(method = 'cv', number = 10))

p<-predict(model, test_set, method='class')

confusionMatrix(test_set[,2], p)
#Accuracy = 0.9329