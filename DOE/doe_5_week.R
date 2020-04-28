rm(list=ls())

psi = factor(c(8500,8700,8900,9100))
block = factor(1:6)

dat4_1 = expand.grid(block=block, psi=psi)
dat4_1$value = c(90.3,89.2,98.2,93.9,87.4,97.9
                 ,92.5,89.5,90.6,94.7,87.0,95.8,
                 85.5,90.8,89.6,86.2,88.0,93.4,
                 82.5,89.5,85.6,87.4,78.9,90.7)
trt_total = tapply(dat4_1$value, dat4_1$psi, function(x) sum(x))
block_total = tapply(dat4_1$value, dat4_1$block, function(x) sum(x))
total = sum(trt_total)

# RCBD
fit_RGBD = aov(value ~ psi + block, dat4_1)
summary(fit_RGBD)

x11()
par(mfrow=c(2,2))
plot(fit_RGBD)
Resid = fit_RGBD$residuals
par(mfrow=c(1,2))

plot(Resid ~ as.character(dat4_1$psi),xlab = "Treatments",ylab = "Residuals", main = "Residuals by Treatments", pch=4)
abline(h=0,col="red")
plot(Resid ~ as.character(dat4_1$psi),xlab = "Blocks",ylab = "Residuals", main = "Residuals by Blocks",pch=2)
abline(h=0,col="blue")

par(mfrow=c(1,1))
plot(0,type="n",xlim = c(1,25))
for(i in 1:25){
    points(x=i,y=0,pch=i)
}


fit_CRD = aov(value ~ psi, dat4_1)
summary(fit_CRD)

library(lme4)
fit_rblock = lmer(value~psi + (1|block), data= dat4_1)

vc = VarCorr(fit_rblock)
var_block = vc$block[1]

var_resid = (fit_rblock@devcomp$cmp[10])^2

# missing value
dat4_1$value[10] = NA

impute_missing = function(value,trt)
{
    n = ncol(trt)
    mis_idx = which(is.na(value))
    coef_trt = rep(0,n)
    for (k in 1:n){
        coef_trt[k] = length(levels(trt[,k]))
    }
    
    trt_sum = list()
    for(i in 1:n){
        trt_sum[i] = 0
        trt_sum[[i]] = tapply(value,trt[,i],function(x) sum(x, 
                                                            na.rm = T))
    }
    total_sum = sum(trt_sum[[1]])
    tmp_sum = rep(0,n)
    for(j in 1:length(mis_idx)){
        for(i in 1:n){
            tmp_idx = which(trt[mis_idx,i]==levels(trt[,i]))
            tmp_sum[i] = trt_sum[[i]][tmp_idx]
        }
        value[mis_idx[j]] = (sum(tmp_sum*coef_trt) - total_sum)/ prod((
            coef_trt-1))
    }
    data = data.frame(trt,value)
    return(data)
}

impute_data = impute_missing(dat4_1$value, dat4_1[,1:2])


dat4_1$value[10] = NA

mis_lev = dat4_1[is.na(dat4_1$value),]
a = length(levels(dat4_1$psi))
b = length(levels(dat4_1$block))

yi._prime = sum(dat4_1[dat4_1$psi == mis_lev[,"psi"],"value"],na.rm = T)
y.j_prime = sum(dat4_1[dat4_1$block == mis_lev[,"block"],"value"],na.rm = T)
y.._prime = sum(dat4_1$value,na.rm = T)

x = (a*yi._prime + b*y.j_prime - y.._prime)/ ((a-1)*(b-1))

impute_dat = dat4_1
impute_dat[which(is.na(dat4_1$value)),"value"] = x

fit_impute = aov(value~ psi + block, data=impute_dat)
res_impute = anova(fit_impute)

ss_vec = res_impute$'Sum Sq'
df_vec= res_impute$Df

ms_trt = ss_vec[1]/df_vec[1]
ms_r = ss_vec[3]/(df_vec[3]-1)

f_value = ms_trt/ ms_r;f_value
pf(f_value, 3, 14, lower.tail = F)

RM = factor(1:5)
Op = factor(1:5)

design1 = factor(c("A","B","C","D","E","B","C","D","E","A"
                   ,"C","D","E","A","B","D","E","A","B","C","E","A","B","C","D"))

rp_fac = c("A","B","C","D","E")
rp = c(rp_fac,rp_fac[1:4])
design2 = rep(0,25)

for(i in 1:5){
    for(j in 1:5){
        design2[5*(i-1)+1:5] = rp[1:5+i-1]
    }
}
design2

design3 = rep(0,25)
for(i in 1:5){
    design3[5*(i-1)+1:5] = rp[1:5+i-1]
}
design3 = as.factor(design3)

dat_4_9 = expand.grid(Op = Op, RM=RM)
dat_4_9$latin = design1
dat_4_9$value = c(24,20,19,24,24,17,24,30,27,36,18,38,26,27,21,26,31,26,23,22,22,30,20,29,31)

fit_latin = aov(value ~ latin+RM+Op,data = dat_4_9)
summary(fit_latin)



trt = factor(1:4)
block = factor(1:4)

dat_4_22 = expand.grid(block = block, trt=trt)
dat_4_22$value = c(73,74,NA,71,NA,75,67,72,73,75,68,NA,75,NA,72,75)

install.packages("ibd")
install.packages("multcompView")
library(ibd)
library(multcompView)

fit_bibd = aov.ibd(value ~ trt+block,specs = "trt",data = dat_4_22)
fit_bibd
?aov.ibd
