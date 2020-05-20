A<-matrix(c(1,2,3,4),2,2)
B<-matrix(c(2,1,1,1),2,2)
C<-matrix(c(1,2,3,4),2,2)
A%*%B%*%C
solve(C)%*%solve(B)%*%solve(A)
qr(A)
GS<-qr.Q(qr(A))
crossprod(GS[,1],GS[,2])


A<-matrix(c(1,2,0,5),2,2)
#A<-matrix(c(1,2,2,5),2,2)
res<-eigen(A)
solve(A)

lambda1<-res$values[1]
lambda2<-res$values[2]
v1<-res$vectors[,1]
v2<-res$vectors[,2]

cov(A)
res$vectors%*%diag(res$values)%*%t(res$vectors)

lambda1*v1%*%t(v1)+lambda2*v2%*%t(v2)


# Using matrix representation to compute mean and cov
rx<-rnorm(1000,0,1);n=200;p=5
X<-matrix(rx,n,p)

v1<-rep(1,n)


mean<- t(X)%*%v1/n 

covM<-  t(X)%*%(diag(v1)-v1%*%t(v1)/n)%*%X/(n-1)
cov(X)
D<-diag(diag(covM)^{-1/2})

corM<-D%*%covM%*%D


## Lec 6

set.seed(123)
# lets first simulate a bivariate normal sample
library(MASS)
bivn <- mvrnorm(100000, mu = c(0, 0), Sigma = matrix(c(1, .5, .5, 1), 2))

# a kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50) ## n is the number of grid points in each axis

# now plot your results
contour(bivn.kde)

image(bivn.kde)

persp(bivn.kde, phi = 45, theta = 30)

# a fancy contour image
image(bivn.kde); contour(bivn.kde, add = T)

# a fancy perspective
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA)


## checking the major axis

eigen(solve(A))
eigen(A)

contour(bivn.kde)
arrows(0,0,5*0.7071,5*0.7071,col="red")
arrows(0,0,5*0.7071,-5*0.7071,col="red")


### QQ plot (radiation data when door is closed )

data<-scan("T4-1.dat")

qqnorm(data)
qqline(data)

## QQ plot (multivariate case)
## Q-Q plot for Chi^2 data against true theoretical distribution:

data<-read.table("T4-3.dat")
mode(data)
data<-as.matrix(data)

data2<-data[,1:4]
mean<-apply(data2,2,mean)
S<-cov(data2)

t(data2[1,]-mean)%*%solve(S)%*%(data2[1,]-mean) ## compare the last column in data


chiq<-function(p) qchisq(p, df=4)

qqplot(chiq(seq(1/30,1,length=30)-0.5/30), data[,5])


### QQ plot
### Some transformation is helpful to get near normality.
set.seed(1234)

p<-rbeta(100,2,2.5)

op<-par(mfrow=c(1,2))
qqnorm(p)
qqline(p)
qqnorm(0.5*log(p/(1-p)))
qqline(0.5*log(p/(1-p)))
par(op)

