real_df <- read.csv("C:/Users/Swapnil Kumar Vaish/Documents/R/Data Mining/Q5/BCW_data.csv")
View(real_df)

real_df$X <- NULL

temp_df <- real_df
temp_df$diagnosis <- as.factor(temp_df$diagnosis)


num_rows <- nrow(temp_df)

install.packages("e1071")   # for Naive Bayes 
library(e1071)
#install.packages("caTools")
library(caTools)

##Hold Out method with 75% of training set and 25% of test set
set.seed(1011)
split <- sample.split(temp_df$diagnosis, SplitRatio = 0.75)
training_set = subset(temp_df, split == T)
test_set = subset(temp_df,split == F) 
dim(training_set)
dim(test_set)

model<-naiveBayes(training_set$diagnosis~.,data=training_set)
pred<-predict(model, test_set[,-2])

library(caret)

cfm<-confusionMatrix(pred,test_set$diagnosis)
cfm$overall['Accuracy']

##Hold out method with 66.66% of training and 33.33% o tet set
set.seed(2010)
split<-sample.split(temp_df$diagnosis, SplitRatio = 0.666)
training_set<-subset(temp_df, split == T)
test_set<-subset(temp_df, split == F)
dim(training_set)
dim(test_set)

model<-naiveBayes(training_set$diagnosis~.,data=training_set)
pred<-predict(model, test_set[,-2])

cfm<-confusionMatrix(pred,test_set$diagnosis)
cfm$overall['Accuracy']


##Random Subsampling with 75% of training set and 25% of test set
acc <- c()
set.seed(3333)
for(i in 1:10){
  s <- sample.split(temp_df$diagnosis, SplitRatio = 0.75)
  training_set<-subset(temp_df, split == T)
  test_set<-subset(temp_df, split == F)
  dim(training_set)
  dim(test_set)
  
  
  model<-naiveBayes(training_set$diagnosis~.,data=training_set)
  pred<-predict(model, test_set[,-2])
  
  cfm<-confusionMatrix(pred,test_set$diagnosis)
  acc<-c(acc,cfm$overall['Accuracy'])
}
cat("Average of accuracy: ",mean(acc))  #average of accuracy

#Random Subsampling with 66.6% of training set and 33.3% of test set
acc <- c()
set.seed(4799)
for(i in 1:10){
  s <- sample.split(temp_df$diagnosis, SplitRatio = 0.666)
  training_set<-subset(temp_df, split == T)
  test_set<-subset(temp_df, split == F)
  dim(training_set)
  dim(test_set)
  
  model<-naiveBayes(training_set$diagnosis~., data=training_set)
  pred<-predict(model, test_set[,-2])
  
  cfm<-confusionMatrix(pred,test_set$diagnosis)
  acc<-c(acc,cfm$overall['Accuracy'])
}

cat("Average of accuracy: ",mean(acc))  #average of accuracy


install.packages("naivebayes") #for naive bayes
library(naivebayes)

##Cross Validation with 75% of training set and 25% of test set
set.seed(5256)
split <- sample.split(temp_df$diagnosis, SplitRatio = 0.75)
training_set<-subset(temp_df, split == T)
test_set<-subset(temp_df, split == F)
dim(training_set)
dim(test_set)

model<-train(diagnosis~., data = training_set, method='naive_bayes', trControl = trainControl(method = 'cv', number = 10))

p<-predict(model, test_set, method='class')

cfm<-confusionMatrix(test_set[,2], p)
cfm$overall['Accuracy']


#Cross Validation with 66.6% of training set and 33.3% of test set
set.seed(6806)
split <- sample.split(temp_df$diagnosis, SplitRatio = 0.666)
training_set<-subset(temp_df, split == T)
test_set<-subset(temp_df, split == F)
dim(training_set)
dim(test_set)

model<-train(diagnosis~., data = training_set, method='naive_bayes', trControl = trainControl(method = 'cv', number = 10))

p<-predict(model, test_set, method='class')

cfm<-confusionMatrix(test_set[,2], p)
cfm$overall['Accuracy']














