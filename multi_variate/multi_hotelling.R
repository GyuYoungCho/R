data<-as.matrix(read.table("T5-1.DAT"))
colnames(data)<-c("Sweat rate","Sodium","Potassium")
head(data)


## No serious outlier and quite close to normal distribution
op<-par(mfrow=c(2,2))
boxplot(data,range=1.5)
qqnorm(data[,1]);qqline(data[,1])
qqnorm(data[,2]);qqline(data[,2])
qqnorm(data[,3]);qqline(data[,3])
par(op)

## compute mean and sample cov
v1<-rep(1,20)
n<-20
p<-3

mean<-t(data)%*%v1/n
cov<- (t(data)%*%(diag(v1)-v1%*%t(v1)/n)%*%data)/(n-1)   


mu<-c(4,50,10)

T2=n*t(mean-mu)%*%solve(cov)%*%(mean-mu)

T2

cr.value<-(n-1)*p/(n-p)*qf(0.90,p,n-p)   #?qf



### 95% confidence region

data1<-as.matrix(read.table("T4-1.DAT"))
data2<-as.matrix(read.table("T4-5.DAT"))

#### Few outlier and not close to reference in QQnorm
op<-par(mfrow=c(2,2))
boxplot(data1,range=1.5)
qqnorm(data1);qqline(data1)
boxplot(data2,range=1.5)
qqnorm(data2);qqline(data2)
par(op)

### proper transformation is x^{1/4}
op<-par(mfrow=c(2,2))
boxplot(data1^{1/4},range=1.5)
qqnorm(data1^{1/4});qqline(data1^{1/4})
boxplot(data2^{1/4},range=1.5)
qqnorm(data2^{1/4});qqline(data2^{1/4})
par(op)


data<-cbind(data1^{1/4},data2^{1/4})
colnames(data)<-c("radation(close)","radiation(open)")
head(data)




n<-42
p<-2
v1<-rep(1,42)

mean<-t(data)%*%v1/n
cov<- t(data)%*%(diag(v1)-v1%*%t(v1)/n)%*%data/(n-1)


res<-eigen(cov)
res$values
res$vectors

## plot of 2dim data (with 95% confidence region)

plot(data[,1],data[,2],xlim=c(0.3,0.9),ylim=c(0.3,0.9))
points(mean[1],mean[2],cex=2,col="red")

c1_095<-sqrt(res$values[1]*p*(n-1)/(n*(n-p))*qf(0.95,p,n-p))
c2_095<-sqrt(res$values[2]*p*(n-1)/(n*(n-p))*qf(0.95,p,n-p))

segments(mean[1],mean[2],mean[1]+c1_095*res$vectors[1,1],mean[2]+c1_095*res$vectors[2,1],col="red")
segments(mean[1],mean[2],mean[1]-c1_095*res$vectors[1,1],mean[2]-c1_095*res$vectors[2,1],col="red")
segments(mean[1],mean[2],mean[1]+c2_095*res$vectors[1,2],mean[2]+c2_095*res$vectors[2,2],col="blue")
segments(mean[1],mean[2],mean[1]-c2_095*res$vectors[1,2],mean[2]-c2_095*res$vectors[2,2],col="blue")



## lec8
data<-read.table("T6-1.DAT")

colnames(data)<-c("BOD1","SS1","BOD2","SS2")

d1<-data[,1]-data[,3]
d2<-data[,2]-data[,4]

diff_data<-cbind(d1,d2)


## You shoud be careful about the normality assumption.
## You may consider some transformation before the following analysis.
op<-par(mfrow=c(1,2))
qqnorm(diff_data[,1]);qqline(diff_data[,1])
qqnorm(diff_data[,2]);qqline(diff_data[,2])
par(op)

## compute mean and sample cov
n<-11
p<-2

mean<-c(mean(d1),mean(d2))
cov<- cov(cbind(d1,d2))


mu<-c(0,0)

T2=n*t(mean-mu)%*%solve(cov)%*%(mean-mu)

T2

cr.value<-(n-1)*p/(n-p)*qf(0.95,p,n-p)   #?qf
cr.value

## which variable has nonzero difference ?

c(mean[1]-sqrt((n-1)*p/(n-p)*qf(0.95,p,n-p)*cov[1,1]/n),mean[1]+sqrt((n-1)*p/(n-p)*qf(0.95,p,n-p)*cov[1,1]/n))

c(mean[2]-sqrt((n-1)*p/(n-p)*qf(0.95,p,n-p)*cov[2,2]/n),mean[2]+sqrt((n-1)*p/(n-p)*qf(0.95,p,n-p)*cov[2,2]/n))