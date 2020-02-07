file_1000 = read.csv("1000i.csv", header = FALSE)
file_1000

names(file_1000) <- c("Reciept Number", "Food Items", "Quantity")
View(file_1000)

df<-1:5
food <- c("Milk","Suagr","Choco","Apples", "Curd")
df1 <- data.frame(df,food)
file_1000$`Food Items` <- df1$food[match(file_1000$`Food Items`,df1$df)]
View(file_1000)

typeof(file_1000)

test_df <- file_1000[,c("Reciept Number", "Food Items", "Quantity")]  #alias
typeof(test_df)
View(test_df) 

df_trans <- as(split(test_df$`Food Items`, test_df$`Reciept Number`),"transactions")
View(df_trans)

library(arules)
rules <- apriori(test_df, parameter = list(supp = 0.015,conf = 0.9))
inspect(rules)
