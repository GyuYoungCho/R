#4.1
rm(list=ls())
power_lev = factor(c(160,180,200,220))
times = factor(c(1,2,3,4,5))
dat = expand.grid(times=times,power = power_lev);dat

etch = c(575,542,530,539,570,565,593,590,579,610,600,651,610,637,629,725,700,715,685,710)
dat$etch = etch
x11()

fit_etch = aov(etch~power,data=dat)
anova(fit_etch)

par(mfrow=c(1,1))
qqnorm(fit_etch$residuals)
qqline(fit_etch$residuals)

par(mfrow=c(2,2))
plot_fit = plot(fit_etch)
shapiro.test(fit_etch$residuals)

install.packages("lawstat")
library(lawstat)
# 등분산 가정 만족
levene.test(dat$etch, dat$power, location = "mean")

set.seed(1993)
spl_idx = sample(seq(1:20),20);spl_idx
fit_etch$residuals
fit_etch$residuals[spl_idx]
par(mfrow=c(1,1))
plot(fit_etch$residuals[spl_idx],main = " Plot of residuals vs run order or time",ylab = "residual", xlab = "Run order")
abline(h=0, col="red")

dat$num_power = as.numeric(as.character(dat$power))
as.numeric(dat$power)
fit_reg = lm(etch ~ num_power, data = dat)
plot(dat$etch ~ dat$num_power, main = "Example 3.1 for Regression",
     xlab = "Power", ylab = "Etch~rate")
abline(fit_reg)

fit_reg2 = lm(etch ~ num_power + I(num_power^2), data = dat)
fit_reg2 
lines(dat$num_power, fit_reg2$fitted.values, type="l",col="red")
legend("topleft",legend = c("Linear", "Quadratic"), col=c("black","red"),lty = c(1,1))
abline(fit_reg2)



#4.2
rm(list=ls())
power = factor(c(160,180,200,220))
times = 1:5
dat = expand.grid(times=times,power = power)
dat$value = c(575,542,530,539,570,565,593,590,579,610,600,651,610,637,629,725,700,715,685,710)

?contrasts
c_mat = matrix(c(1,-1,0,0,1,1,-1,-1,0,0,1,-1),ncol=3)
contrasts(dat$power,3) = c_mat
attributes(dat$power)
fit_cont = aov(value~ power, data=dat)
?summary.aov

power_list = list("c1: mu_1 = mu_2"=1,"c2: mu_1 + mu2 = mu_3 + mu4"=2,"c3: mu_3 = mu_4"=3 )
summary.aov(fit_cont,split = list(power = power_list))

install.packages("agricolae")
library(agricolae)
res_scheffe = scheffe.test(fit_cont,trt = "power", console = F)
plot(res_scheffe)

res_LSD = LSD.test(fit_cont,trt = "power", console=F)
plot(res_LSD)
res_Tukey = TukeyHSD(fit_cont,"power",conf.level = 0.95)
res_Tukey2 = HSD.test(fit_cont,trt = "power")
plot(res_Tukey)
plot(res_Tukey2)

pairwise.t.test(dat$value, dat$power, alternative = "two.sided")
install.packages("DescTools")
library(DescTools)
DunnettTest(dat$value, dat$power, control = "160")

install.packages("lme4")
library(lme4)
looms = factor(1:4)
times = factor(1:4)
dat_3_11 = expand.grid(times=times,looms=looms)
dat_3_11$value = c(98,97,99,96,91,90,93,92,96,95,97,95,95,96,99,98)
res_re = lmer(value ~ (1|looms),data = dat_3_11)
typeof(res_re)

res_re@call
sigma_tau2 = 2.638^2
sigma2 = 1.377^2
plot(res_re)
confint(res_re,oldNames=F)