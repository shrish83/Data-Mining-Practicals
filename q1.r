#editrules i.e. install this package
#editset -> rules same file
#editfile -> separate file
#E <- editset(expression(Age>=0,.....)) %in% c('single','...',...)(values belong to one of these)
#if(Age<18) agegroup=='child

install.packages("editrules")
library(editrules)

filename = read.table("table.txt", header=TRUE)  #part1
filename

check = editset(expression(                     #part2 with 'check' as 'E'
  Age>=0,Age<=150,Age>yrsmarried,
  status %in% c('single', 'married','widowed'),
  if(Age<18) agegroup =='child',
  if(Age>18 && Age<=65) agegroup=='adult',
  if(Age>65) agegroup=='elderly'
))
check
summary(check)

clean =violatedEdits(check,filename)      #part3
clean
View(clean)

summary(clean)                           #part4

pie(table(clean))                           #part5
barplot(table(clean))
