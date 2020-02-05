#outliers i.e noise

rivers
class(rivers)
View(rivers)
summary(rivers)
density(table(rivers))
plot(density(table(rivers)))
hist(rivers)
boxplot(rivers)
boxplot(rivers, horizontal = TRUE)

riv1<- rivers[rivers<1250]
riv1
summary(riv1)
boxplot(riv1,horizontal = TRUE)
riv2<- rivers[rivers<1050]
riv2
summary(riv2)
boxplot(riv2,horizontal = TRUE)
