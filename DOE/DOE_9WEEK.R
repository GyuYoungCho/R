rm(list= ls())

A = c(-1,1)
B = c(-1,1)
times = 1:3

dat6_2 = expand.grid(times=times,A=A,B=B)
dat6_2$y = c(28,25,27,36,32,32,18,19,23,31,30,29)

cont = model.matrix(~A+B+A:B-1,data=dat6_2)
eff_vec = dat6_2$y %*% cont/(2*3)

ss_vec = (dat6_2$y %*% cont)^2 / (4*3)
summary(aov(y~A*B,data = dat6_2))

fit_6_2 = lm(y~A+B,data=dat6_2)
summary(fit_6_2)

x11()
par(mfrow=c(2,2))
plot(fit_6_2)
par(mfrow=c(1,1))


install.packages("rsm")
library(rsm)
persp(fit_6_2,B~A,zlim = c(15,35))
contour(fit_6_2,B~A,levels=seq(23,33,2))




A = c(-1,1)
B = c(-1,1)
C = c(-1,1)
times = 1:2
dat6_4 = expand.grid(times=times,A=A,B=B,C=C)
dat6_4$y = c(550,604,669,650,633,601,642,635,1037,1052,749,868,1075,1063,729,860)
cont = model.matrix(~ -1 + A*B*C,data=dat6_4)
eff_vec = dat6_4$y %*% cont/(2^3)

ss_vec = (dat6_4$y %*% cont)^2 / (2^4)

summary(aov(y~A*B*C,data = dat6_4))
summary(aov(y~A*C,data = dat6_4))

fit_6_4 = lm(y~A*C,data=dat6_4)

par(mfrow=c(2,2))
plot(fit_6_4)
par(mfrow=c(1,1))

persp(fit_6_4,C~A,zlim=c(500,1057))
contour(fit_6_4,C~A,levels=seq(673.625,980.123,length.out = 5))

summary(fit_6_4)


install.packages("qpcR")
library(qpcR)
res = PRESS(fit_6_4)
sum(res$residuals^2)

