table<-read.table("C:\\Users\\ayush\\OneDrive\\Desktop\\R working directory\\T10-13.txt", header = T,sep="", skip=14 )
#skip = to skip the names of the variables in the original data file
View(table)
names(table)<-c("year","carsales","carcpi","cpi","dpi","intrate","employrate")
newcpi<-cpi*2
head(newcpi)
table$newcpi <-cpi*2 #$ is used to create a new variable
View(table)
model<-lm(carsales~cpi, data = table)
summary(model)
plot(carsales~cpi, type="line")
model<-lm(carsales~carsales+carcpi+cpi+dpi+intrate+employrate+year+newcpi, data=table)
summary(model)
#case of singularity, MCL = high correlation
plot(carsales~newcpi, type="line")
cor(table)
cor.test(cpi,dpi)
cor.test(newcpi,dpi)
pairs(table)
newcpi=cpi*rnorm(16,2,3)
pairs(table)
cor(table)
model<-lm(cpi~.-carsales, data = table)
summary(model)$r.squared
model<-lm(dpi~.-carsales, data = table)
summary(model)$r.squared
model<-lm(intrate~.-carsales, data=table)
summary(model)$r.squared
model<-lm(employrate~.-carsales, data=table)
summary(model)$r.squared 
a<-summary(model)$r.squared

k=6 #number of betas
fmodel2N<-(a/(k-2))
fmodel2D<-(1-a)/(length(year)-k+1)
f=fmodel2N/fmodel2D

ndf<-(k-2)
n<-length(year)
ddf<-(n-k+1)

1-pf(f,ndf,ddf) 

#variance inflating factor

vif1<-1/(1-cor(carcpi,cpi))
vif1
#vif1 = 1/(1-r23^2)*

#simple pie chart
# state-wise contribution of gdp

slices<-c(10, 12, 4, 16, 8)
lbls<-c("MP", "UP", "KL", "HP", "AP")
pie(slices, labels = lbls, main="state-wise contribution of gdp", col=rainbow(5))
install.packages("plotrix")
library(plotrix)
pie3D(slices, labels=lbls, explode=0.3,
main="state-wise contribution of gdp", col=rainbow(5))

?pie3D
