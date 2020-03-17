real_df <- read.csv("C:/Users/Swapnil Kumar Vaish/Documents/R/Data Mining/Q5/BCW_data.csv", stringsAsFactors = TRUE)
#real_df <- read.csv("/home/dev_rawnie/file.csv", stringsAsFactors = TRUE) #Comment in Windows, uncomment in linux
View(real_df)
View(temp_df)
real_df$X <- NULL
real_df$id<- NULL
# real_df
#real_df<-real_df[,c(2,1,3:32)]   #reordering the diagnosis attribute   ##Not required now
temp_df <- real_df
temp_df$diagnosis <- as.factor(as.integer(temp_df$diagnosis))  # For KNN all the features must be numeric, Here diagnosis was a factor of characters, which is converted to numeric factor

num_rows <- nrow(temp_df)
#k<-round(sqrt(num_rows))  #to choose value of k

#install.packages("caTools")
library(caTools)

##Hold Out method with 75% of training set and 25% of test set
set.seed(1011)
split <- sample.split(temp_df$diagnosis, SplitRatio = 0.75)
training_set = subset(temp_df, split == T)
test_set = subset(temp_df,split == F) 

func_normalize <- function(x) { 
  return((x- min(x))/(max(x)- min(x)))
} 
num_col<-2:31  #excluding column 1 as factors shouldn't be normalized

df_new <- as.data.frame(lapply(training_set[,num_col], func_normalize))
#rm(real_df, split)
#str(df_new)
#summary(df_new)
trainingKNN<-cbind(df_new, training_set$diagnosis)
# View(trainingKNN)
names(trainingKNN)[31]<-"diagnosis"
#rm(training_set)

testKNN<- as.data.frame(lapply(test_set[,num_col], func_normalize))
testKNN<-cbind(testKNN, test_set$diagnosis)
names(testKNN)[31]<-"diagnosis"
#rm(test_set)

training_target<-trainingKNN[,31]
test_target<-testKNN[,31]

#summary(trainingKNN[,num_rows])

#install.packages("class", dependencies = TRUE)

library(class)
m1<-knn(trainingKNN, testKNN, cl = training_target, k=25)

library(caret)
cfm<-confusionMatrix(test_target, m1)
cfm$overall['Accuracy']

##Hold out method with 66.66% of training and 33.33% o tet set
set.seed(2010)
split<-sample.split(temp_df$diagnosis, SplitRatio = 0.666)
training_set<-subset(temp_df, split == T)
test_set<-subset(temp_df, split == F)
dim(training_set)
dim(test_set)

num_col<-2:31  #excluding column 1 as factors shouldn't be normalized
df_new <- as.data.frame(lapply(training_set[,num_col], func_normalize))
#rm(real_df, split)
#str(df_new)
#summary(df_new)
trainingKNN<-cbind(df_new, training_set$diagnosis)
# View(trainingKNN)
names(trainingKNN)[31]<-"diagnosis"
#rm(training_set)

testKNN<- as.data.frame(lapply(test_set[,num_col], func_normalize))
testKNN<-cbind(testKNN, test_set$diagnosis)
names(testKNN)[31]<-"diagnosis"
#rm(test_set)

training_target<-trainingKNN[,31]
test_target<-testKNN[,31]

#summary(trainingKNN[,num_rows])

#install.packages("class", dependencies = TRUE)

library(class)
m1<-knn(trainingKNN, testKNN, cl = training_target, k=25)

library(caret)
cfm<-confusionMatrix(test_target, m1)
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
  
  num_col<-2:31  #excluding column 1 as factors shouldn't be normalized
  df_new <- as.data.frame(lapply(training_set[,num_col], func_normalize))
  #rm(real_df, split)
  #str(df_new)
  #summary(df_new)
  trainingKNN<-cbind(df_new, training_set$diagnosis)
  # View(trainingKNN)
  names(trainingKNN)[31]<-"diagnosis"
  #rm(training_set)
  
  testKNN<- as.data.frame(lapply(test_set[,num_col], func_normalize))
  testKNN<-cbind(testKNN, test_set$diagnosis)
  names(testKNN)[31]<-"diagnosis"
  #rm(test_set)
  
  training_target<-trainingKNN[,31]
  test_target<-testKNN[,31]
  
  #summary(trainingKNN[,num_rows])
  
  #install.packages("class", dependencies = TRUE)
  
  library(class)
  m1<-knn(trainingKNN, testKNN, cl = training_target, k=25)
  
  library(caret)
  cfm<-confusionMatrix(test_target, m1)
  acc<-c(acc,cfm$overall['Accuracy'])
}
cat("Average of accuracy: ",mean(acc))  #average of accuracy


##Random Subsampling with 66.6% of training set and 33.3% of test set
acc <- c()
set.seed(3333)
for(i in 1:10){
  s <- sample.split(temp_df$diagnosis, SplitRatio = 0.666)
  training_set<-subset(temp_df, split == T)
  test_set<-subset(temp_df, split == F)
  dim(training_set)
  dim(test_set)
  
  num_col<-2:31  #excluding column 1 as factors shouldn't be normalized
  df_new <- as.data.frame(lapply(training_set[,num_col], func_normalize))
  #rm(real_df, split)
  #str(df_new)
  #summary(df_new)
  trainingKNN<-cbind(df_new, training_set$diagnosis)
   View(trainingKNN)
  names(trainingKNN)[31]<-"diagnosis"
  #rm(training_set)
  
  testKNN<- as.data.frame(lapply(test_set[,num_col], func_normalize))
  testKNN<-cbind(testKNN, test_set$diagnosis)
  names(testKNN)[31]<-"diagnosis"
  #rm(test_set)
  
  training_target<-trainingKNN[,31]
  test_target<-testKNN[,31]
  
  #summary(trainingKNN[,num_rows])
  
  #install.packages("class", dependencies = TRUE)
  
  library(class)
  m1<-knn(trainingKNN, testKNN, cl = training_target, k=25)
  
  library(caret)
  cfm<-confusionMatrix(test_target, m1)
  acc<-c(acc,cfm$overall['Accuracy'])
}
cat("Average of accuracy: ",mean(acc))  #average of accuracy



##Cross Validation with 75% of training set and 25% of test set
set.seed(5555)
split<-sample.split(temp_df$diagnosis, SplitRatio = 0.75)
training_set<-subset(temp_df, split==TRUE)
test_set<-subset(temp_df, split==FALSE)

model<-train(diagnosis~., data = training_set, method='knn', trControl = trainControl(method = 'cv', number = 20))

p<-predict(model, test_set, method='class')

cfm<-confusionMatrix(test_set[,1], p)
cfm$overall['Accuracy']

##Cross Validation with 66.6% of training set and 33.3% of test set
set.seed(5555)
split<-sample.split(temp_df$diagnosis, SplitRatio = 0.666)
training_set<-subset(temp_df, split==TRUE)
test_set<-subset(temp_df, split==FALSE)

model<-train(diagnosis~., data = training_set, method='knn', trControl = trainControl(method = 'cv', number = 20))

p<-predict(model, test_set, method='class')

cfm<-confusionMatrix(test_set[,1], p)
cfm$overall['Accuracy']



