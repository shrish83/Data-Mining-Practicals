#Apriori Algorithm: to apply apriori algorithm on any dataset, it must be converted to transactions
#package arules
#Groceries dataset
#confidence,support,lift

install.packages("arules")
library(arules)

data(Groceries)
View(Groceries)
summary(Groceries)

rules <- apriori(Groceries, parameter = list(supp = 0.001,conf = 0.8))
inspect(rules)
inspect(rules[1:10])


rules_sort <- sort(rules, by = "confidence",decreasing = TRUE)
inspect(rules_sort[1:10])
summary(inspect(rules_sort[1:10]))

install.packages("arulesViz", dependencies = TRUE) #dependencies i.e. any other packages related to the package mentioned will also get downloaded implicitly
library(arulesViz)
plot(rules[1:10])

