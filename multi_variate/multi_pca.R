
rm(list=ls())

data<-read.table("T8-6.DAT")
colnames(data)<-c("100m","200m","400m","800m","1500m","5000m","10000m","Marathon","Country")

head(data)

scdata<-scale(data[,-9])
V<-cor(scdata)

PCres<-eigen(V)

PCres$vectors[,1:2]

PCres$values[1:4]

cumsum(PCres$values)[1:4]/sum(PCres$values)


plot(PCres$values)


plot(cumsum(PCres$values)/sum(PCres$values))

### detecting outlier via PCA

score1<-scdata%*%PCres$vectors[,1]
score2<-scdata%*%PCres$vectors[,2]

qqnorm(score1);qqline(score1)

plot(score1,score2,cex=0)
text(score1,score2,label=as.character(c(1:55)))
