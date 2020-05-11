rm(list=ls())
carb = factor(c(10,12,14))
psi = factor(c(25,30))
speed = factor(c(200,250))
times=factor(1:2)

dat_5_3 = expand.grid(times=times,speed=speed,psi=psi,carb = carb)
dat_5_3$values = c(-3,-1,-1,0,-1,0,1,1
                  ,0,1,2,1,2,3,6,5,
                  5,4,7,6,7,9,10,11)
fit_5_3 = aov(values~speed*psi*carb,data=dat_5_3)
summary(fit_5_3)

dat_carb = tapply(dat_5_3$values, dat_5_3$carb,mean)

plot(dat_carb, type="b",col="blue",xaxt = "n",
     main = "Main effects for Percent carbonation (A)", xlab="Percent carbonation (A)", ylab = "Average fill deviation", xlim = c(0,4),ylim = c(-2,8))
axis(1,at=c(1,2,3),labels=c(10,12,14), lwd.ticks = 2)

x11()
par(mfrow=c(2,2))
for( i in 4:1){
    if(i!=1){
        fct_name = colnames(dat_5_3)[i]
        x_num = as.numeric(levels(dat_5_3[,i]))
        n = length(x_num)
        
        tmp_dat = tapply(dat_5_3$values, dat_5_3[,i],mean)
        plot(tmp_dat, type="b",col=i,xaxt = "n",
             main = paste("Main effects for",fct_name), xlab=fct_name, ylab = "Average fill deviation", xlim = c(0,n+1),ylim = c(-2,8))
        axis(1,at=seq(from=1,by=1,length.out = n),labels=x_num, lwd.ticks = 2)
    }
    else{
        tmp_dat = tapply(dat_5_3$values, list(carb = dat_5_3$carb,psi = dat_5_3$psi),mean)
        interaction.plot(dat_5_3$carb,dat_5_3$psi, response = dat_5_3$values, ylim= c(-2,10),xaxt="n",
                         xlab = "Carbonation-pressure interaction", 
                         ylab = "Response",main="Carbonation-pressure interaction",
                         legend = F, col=c("blue","red"))
        axis(1,at=c(1,2,3),labels=c("10","12","14"),lwd.ticks = 2)
        legend("topleft",c("25 psi", "30 psi"),lty=2:1, col=c("blue","red"))
        clist = c("blue","blue","blue","red","red","red")
        fct_list = rep(1:3,2)
        for(i in 1:6){
            points(fct_list[i],tmp_dat[i],cex=1.3,pch=19,col=clist[i])
        }
    }
    
}

Mtype = factor(1:3)
temp = factor(c(15,70,125))                   
times = factor(1:4)

dat_5_4 = expand.grid(times = times, temp = temp, Mtype = Mtype)
dat_5_4$values =c(130,155,74,180,  34,40,80,75,  20,70,82,58,
                  150,188,159,126, 136,122,106,115,  25,70,58,45,
                  138,110,168,160, 174,120,150,139, 96,104,82,60)
fit_origin = aov(values~Mtype*temp, data = dat_5_4)
summary(fit_origin)

str(dat_5_4)
dat_5_4$temp = as.numeric(as.character(dat_5_4$temp))
str(dat_5_4)

fit_5_4 = aov(values~Mtype*temp + Mtype*I(temp^2), data = dat_5_4)
summary(fit_5_4)

fit_mtype = list()
for(i in 1:3){
    fit_mtype[[i]] = lm(values~temp + I(temp^2),data=dat_5_4, subset=Mtype==i)
    print(summary(fit_mtype[[i]]))
}

par(mfrow=c(1,1))
plot(dat_5_4$temp, dat_5_4$values, pch=16, ylim = c(min(dat_5_4$values),max(dat_5_4$values)),
     xaxt="n", yaxt="n",ylab = "Life", xlab = "Temperature")
axis(1,seq(15,125,by=27.5))
axis(2,seq(20,188,by=42))

for(i in 1:3){
    lines(seq(15,125),predict(fit_mtype[[i]], data.frame(temp = seq(15,125))))
}
text(locator(1),"Material type 1")
text(locator(1),"Material type 2")
text(locator(1),"Material type 3")

noise = factor(c("Low","Medium","High"))
f_type = factor(1:2)
blk = factor(1:4)
dat_5_6 = expand.grid(f_type = f_type, blk= blk, noise = noise)
dat_5_6$value = c(90,86,96,84,100,92,92,81,
                  102,87,106,90,105,97,96,80,
                  114,93,112,91,108,95,98,83)

library(lme4)
fit_blk = lmer(value~ noise*f_type+(1|blk),data = dat_5_6)
summary(fit_blk)
anova(fit_blk)

fit_blk2 = aov(value~ noise*f_type+blk,data = dat_5_6)
summary(fit_blk2)

par(mfrow=c(1,1))
plot(dat_5_6$value~fitted.values(fit_blk),
     ylim=c(75,115),xlim=c(75,115))
abline(a=0,b=1)
