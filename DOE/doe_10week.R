rm(list=ls())

A = c(-1,1);B = c(-1,1);C = c(-1,1);D = c(-1,1)
dat6_2 = expand.grid(A=A,B=B,C=C,D=D)
dat6_2$y = c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)

cont = model.matrix(~ -1 + A*B*C*D,data = dat6_2)

eff_vec = dat6_2$y %*%cont/8
ss_vec = (dat6_2$y %*% cont)^2 /16

qvalue = qqnorm(eff_vec, ylim = c(-20,25))
qqline(eff_vec)
text(x = qvalue$x, y =eff_vec+2,labels = gsub(':','',colnames(eff_vec)))

subdat6_2 = tapply(dat6_2$y, dat6_2$A, mean)
plot(x=c(-1,1), y=subdat6_2, type='b',xlab="A",
     ylab="Average filtration rate (gal/h)", xlim = c(-1.5,1.5),
     ylim=c(50,90))
interaction.plot(dat6_2$A, dat6_2$C, dat6_2$y, legend=F, 
                 ylim=c(40,100), main = "AC interacion", xlab="A",
                 ylab = "Average filtration rate (gal/h)",
                 col = c('blue','red'),xaxt="n")
axis(1,at=c(1,2),labels = c("-","+"),lwd.ticks = 0.5)
legend("topleft",legend = c("C = -","C = +"),col = c("blue","red")
       , lty = 2:1)
text(locator(1), labels="C = -",col="blue")
text(locator(1), labels="C = +",col="red")


summary(aov(y~A*C*D,data = dat6_2))
fit = lm(y~A+C+D+A:C+A:D,data = dat6_2)
coef(fit)[-1]
eff_vec[c(1,3,4,6,8)]/2

x11()
par(mfrow=c(2,2))
plot(fit)

library(rsm)
contour(lm(y~A*C,data = dat6_2, subset = D==1),C~A, levels=seq(50,90,10))
contour(lm(y~C*D,data = dat6_2, subset = A==1),D~C, levels=seq(65,100,5))
