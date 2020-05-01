A =factor(0:1)
B = factor(0:1)

dat_5_1 = expand.grid(A=A,B=B)
dat_5_1$values = c(20,40,30,52)

me_A = mean(dat_5_1$values[dat_5_1$A==1]) -  mean(dat_5_1$values[dat_5_1$A==0])
me_B =  mean(dat_5_1$values[dat_5_1$B==1]) -  mean(dat_5_1$values[dat_5_1$B==0])

idx1 = dat_5_1$A==dat_5_1$B
idx2 = dat_5_1$A!=dat_5_1$B
ie_AB = mean(dat_5_1$values[idx1]) - mean(dat_5_1$values[idx2])

interaction.plot(dat_5_1$A, dat_5_1$B, response = dat_5_1$values,
                 ylim = c(10,60), xaxt = "n", xlab = "Factor A",ylab = "Response", main = "Interaction Plot in Figure 5.3", legend = F,col = c("blue", "red"))
axis(1,at = c(1,2), labels = c("-", "+"), lwd.ticks = 2)
legend("topleft", c("B-","B+"),lty = 2:1, col=c("blue","red"))
clist = c("blue","blue","red","red")
for(i in 1:4){
    points(dat_5_1$A[i],dat_5_1$values[i],cex=1.3,pch=19,col=clist[i])
}
identify(dat_5_1$A, dat_5_1$values, labels="B-")
identify(dat_5_1$A, dat_5_1$values, labels="B+")
# 점 근처에서 쓰기

Mtype = factor(1:3)
temp = factor(c(15,70,125))                   
times = factor(1:4)

dat_5_3 = expand.grid(times = times, temp = temp, Mtype = Mtype)
dat_5_3$values =c(130,155,74,180,  34,40,80,75,  20,70,82,58,
                  150,188,159,126, 136,122,106,115,  25,70,58,45,
                  138,110,168,160, 174,120,150,139, 96,104,82,60)

fit_fac = aov(values ~ Mtype + temp + Mtype:temp, data = dat_5_3)
summary(fit_fac)

install.packages("latex2exp")
library(latex2exp)
interaction.plot(dat_5_3$temp, dat_5_3$Mtype, response = dat_5_3$values)

interaction.plot(dat_5_3$temp, dat_5_3$Mtype, response = dat_5_3$values,
                 ylim = c(10,175), xaxt = "n", xlab = expression(paste("Temperature (",degree,"F)")),ylab = TeX("Average life $\\bar{y}_{i}.}$"), main = "Interaction between Material type and Temperature", legend = F,col = c("blue", "red","green"))

text(locator(1),"Type 1" , col="blue")
text(locator(1),"Type 2" , col="red")
text(locator(1),"Type 3" , col="green")



library(ggplot2)
library(dplyr)
obs_mean1 = tapply(dat_5_3$values, list(dat_5_3$Mtype, dat_5_3$temp),FUN = mean)
data.frame(expand.grid(Mtype=factor(1:3), temp = factor(c(15,70,125))), values = as.vector(obs_mean1))


rep(sum(dat_5_3$values),3)
dat_5_3$values %>% sum() %>% rep(3)

dat_5_3 %>% 
    group_by(temp,Mtype) %>%
    summarise(mean_value = mean(values)) -> obs_mean2
obs_mean2

obs_mean2 %>%
    ggplot(aes(x=temp, y=mean_value)) + 
    geom_line(size = 1.5, aes(group = Mtype, color=Mtype)) + 
    geom_point(size = 2.5, aes(color = Mtype),shape=15) + 
    labs(
        title = "Interaction betweem Material type and Temperature",
        x = expression(paste("Temperature (",degree,"F)")),
        y = TeX("Average life \t $\\bar{y}_{i}.}$")
    )


fit_noint = aov(values~ Mtype + temp , data=dat_5_3)  
summary(fit_noint)

par(mfrow=c(2,2))
plot(fit_noint)
