rm(list=ls())
dat_2_1 = read.table("DOE_2/example_2_1.txt",sep="\t",header = T)


attach(dat_2_1)

y1_bar = mean(y1)
y2_bar = mean(y2)

y1_sig2 = var(y1)
y2_sig2 = var(y2)

y1_sig = sqrt(y1_sig2)
y2_sig = sqrt(y2_sig2)

n1= length(y1)
n2 = length(y2)

# case 1 : n>30 -> z-test
Z0 = (y1_bar - y2bar) / sqrt(y1_sig2/n1 + y2_sig2/n2);z0
qnorm(0.025);2*pnorm(z0)

# case 2 : n<30  var1 = var2
t_res = t.test(y1,y2,alternative = "two.sided",var.equal = T);t_res
t_res$statistic
t_res$conf.int[1:2]
t_res$p.value

diff = y1_bar - y2_bar
n = n1+n2 -2
t = qt(0.025,df=n,lower.tail = F)
s_pool2 = ((n1-1)*y1_sig2 + (n2-1)*y2_sig2)/n
interval =  t*sqrt(s_pool2)*sqrt(1/n1 + 1/n2)
c(diff-interval,diff+interval)

t_res = t.test(y1,y2,alternative = "two.sided",var.equal = F);t_res
V = ((y1_sig2/n1)+(y2_sig2/n2))^2 / (((y1_sig2/n1)^2 / (n1-1)) + ((y2_sig2/n2)^2 / (n2-1)))

# one sample test
install.packages("EnvStats")
library(EnvStats)
y1_sig2
varTest(y1,alternative = "less", sigma.squared = 0.15)

y2_sig2
res = varTest(y2,alternative = "greater", sigma.squared = 0.05)
res$statistic
res$p.value

# two sample test
# H0 : sigma2^2 = sigma1^2 *2 vs H1 : sigma2^2 != sigma1^2 *2
y1_sig2;y2_sig2
var.test(y1,y2,ratio = 2,alternative = "two.sided")

x1 = rnorm(100,0,1)
x2 = rnorm(100,0,sqrt(2))
var.test(x2,x1,ratio = 2,alternative = "two.sided")


# 3.2
rm(list=ls())
power_lev = factor(c(160,180,200,220))
times = factor(c(1,2,3,4,5))
dat = expand.grid(times=times,power = power_lev);dat

etch = c(575,542,530,539,570,565,593,590,579,610,600,651,610,637,629,725,700,715,685,710)
dat$etch = etch
dat

y_idot = tapply(dat$etch,dat$power,function(x) sum(x))
y_idotbar = tapply(dat$etch,dat$power,function(x) mean(x))


y_idot2 = rep(0,4)
y_idotbar2 = rep(0,4)
for (i in 1:4){
    y_idot2[i] = sum(dat$etch[dat$power==levels(dat$power)[i]])
    y_idotbar2[i] = mean(dat$etch[dat$power==levels(dat$power)[i]])
}
y_idot2;y_idotbar2

x11()
par(mfrow=c(1,2))
plot(dat$etch ~ dat$power,main = "example 3.1 by boxplot",
     xlab = "power", ylab = "etch-rate")
plot(dat$etch ~ as.character(dat$power),main = "example 3.1 by boxplot",
     xlab = "power", ylab = "etch-rate")

fit_etch = aov(etch~power,data=dat)
anova(fit_etch)

fit_etch2 = lm(etch~power,data=dat)
anova(fit_etch2)  # same result

fit_etch3 = lm(etch~as.numeric(as.character(power)),data=dat)
anova(fit_etch3)

ss_total = sum(anova(fit_etch)$'Sum Sq')
ss_treat = anova(fit_etch)$'Sum Sq'[1]
ss_error = anova(fit_etch)$'Sum Sq'[2]
ss_total;ss_treat;ss_error

ss_vec = anova(fit_etch)$'Sum Sq'
ss_total = sum(ss_vec)
ss_treat = ss_vec[1]
ss_error = ss_vec[1]
ss_total;ss_treat;ss_error
