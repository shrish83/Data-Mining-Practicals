#demo-Dataframe
dat <- data.frame(a=c(1,Inf),b=c(Inf,3),d=c("a","b"))
dat
is.na(dat)
sapply(dat,is.infinite)
is.na(dat) <- sapply(dat,is.infinite)
is.na(dat)
dat

#Q2
fl = read.csv("table.csv")
fl
View(fl)

is.na(fl)
cc<-complete.cases(fl)
cc
head(fl)
class(fl)
as.numeric(complete.cases(fl))   #true/false to 1/0

a = sum(as.numeric(complete.cases(fl)))
a

# nrow() gives total no. of rows
nr_total<-nrow(fl)                              #part1
nr_total
percent_compute <- 100 * (cc/nr)           #part1
percent_compute
compute <- 100 * a/nr
compute

sapply(fl,is.infinite)
is.na(fl) <- sapply(fl,is.infinite)    #Part2
is.na(fl)
fl
View(fl)

library(editrules)
efl = editfile("ruleset.txt")     #Part3
efl


clean = violatedEdits(efl,fl)            #Part4
clean
View(clean)
summary(clean)
class(clean)
plot(clean)
density(table(clean))
plot(density(table(clean)))
barplot(table(clean))
pie(table(clean))


boxplot(fl)        #Part5
boxplot(fl$Sepal.Length)
boxplot.stats(fl$Sepal.Length)
