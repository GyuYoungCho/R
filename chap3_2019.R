# crab data
crabs <- read.table("crab.dat",header=T)
crabs$color<-as.factor(crabs$color)
crabs$spine<-as.factor(crabs$spine)

str(crabs)
summary(crabs)

# satellites 분석

# 그림 보기

# satellites vs. width
hist(crabs$satellites)
hist(crabs$width)

plot(crabs$satellites~crabs$width)
lines(lowess(crabs$satellites~crabs$width))
# smoothing line으로 전체적인 경향을 파악한다. 그림에서는 전체적으로 width가 
# 증가함에 따라 satellites도 증가하는 경향을 보인다.

# satellites vs. weight
plot(crabs$satellites~crabs$weight)
lines(lowess(crabs$satellites~crabs$weight))
# weight가 증가함에 따라 satellites도 증가하는 경향을 보인다.

# width vs. weight
plot(crabs$width~crabs$weight)
lines(lowess(crabs$width~crabs$weight))
# weight와 width는 직선의 관계로 width가 증가함에 따라 weight도 증가하는 경향을 보인다.

cor(crabs$satellites,crabs$width)
cor(crabs$satellites,crabs$weight)
cor(crabs$width,crabs$weight)

# satellites vs. color

boxplot(satellites~color,data=crabs)

hist(crabs$satellites)

# 히스토그램 비교
par(mfrow=c(4,1))
hist(crabs[which(crabs$color=="2"),"satellites"],xlim=c(0,15))
hist(crabs[which(crabs$color=="3"),"satellites"],xlim=c(0,15))
hist(crabs[which(crabs$color=="4"),"satellites"],xlim=c(0,15))
hist(crabs[which(crabs$color=="5"),"satellites"],xlim=c(0,15))

hist(log(crabs[which(crabs$color=="2"),"satellites"]+1),xlim=c(0,log(16)))
hist(log(crabs[which(crabs$color=="3"),"satellites"]+1),xlim=c(0,log(16)))
hist(log(crabs[which(crabs$color=="4"),"satellites"]+1),xlim=c(0,log(16)))
hist(log(crabs[which(crabs$color=="5"),"satellites"]+1),xlim=c(0,log(16)))

# density plot
par(mfrow=c(1,1))
d1<-density(crabs[which(crabs$color=="2"),"satellites"])
d2<-density(crabs[which(crabs$color=="3"),"satellites"])
d3<-density(crabs[which(crabs$color=="4"),"satellites"])
d4<-density(crabs[which(crabs$color=="5"),"satellites"])

plot(d1)
plot(d2)
plot(d3)
plot(d4)

plot(d1,xlim=c(-5,20),ylim=c(0,0.25),col="blue")
lines(d2,col="purple")
lines(d3,col="cyan")
lines(d4,col="red")
# color가 어두워질수록 부수체의 개수가 적어지는 경향이 있다.


# satellites vs. spine

boxplot(satellites~spine,data=crabs)

# density plot
d1<-density(crabs[which(crabs$spine=="1"),"satellites"])
d2<-density(crabs[which(crabs$spine=="2"),"satellites"])
d3<-density(crabs[which(crabs$spine=="3"),"satellites"])

plot(d1)
plot(d2)
plot(d3)

plot(d1,xlim=c(-5,20),ylim=c(0,0.20),col="blue")
lines(d2,col="purple")
lines(d3,col="cyan")
# spine 2와 3은 비슷하고 1에서는 나머지보다 커 보인다.
summary(crabs)


# glm
poi <- glm(satellites~width,family=poisson(link="log"),data=crabs)
# poi <- glm(crabs$satellites~crabs$width,family=poisson(link=log))
summary(poi)

poi2 <- glm(satellites~width,family=gaussian(link = "identity"),data=crabs)
summary(poi2)

# negative binomial
library(MASS)
nb <- glm.nb(satellites~width,link=log,data=crabs)
summary(nb)
str(nb)
nb$terms
nb$model
nb$contrast

# Y에 대해서 (logistic regression)

crabs$color<-as.factor(crabs$color)
crabs$spine<-as.factor(crabs$spine)
crabs$y<-as.factor(crabs$y)

mosaicplot(table(crabs[,c("color","y")]))

boxplot(width~y,data=crabs)

# density plot
d1<-density(crabs[which(crabs$y=="0"),"width"])
d2<-density(crabs[which(crabs$y=="1"),"width"])

plot(d1)
plot(d2)
plot(d1,xlim=c(15,35),ylim=c(0,0.25),col="red")
lines(d2,col="blue")

# glm

bi1<-glm(y~width,family=binomial(link=logit),data=crabs)
summary(bi1)

bi2<-glm(y~width,family=binomial(link=probit),data=crabs)
summary(bi2)


anova(bi1,test="Chisq")
anova(bi2,test="Chisq")

AIC(bi1,bi2)
BIC(bi1,bi2)


x= c(1,2,3)
y= c(2,3,4)
z= c(3,4,5)
var(x)
d = cbind.data.frame(x,y+x,z+y+x)
cov(d)
