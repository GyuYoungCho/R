rm(list=ls())
ex19 = matrix(c(871,444,873,302,80,43),nrow=2,byrow = T)
ex19
dimnames(ex19) = list(person=c("white","black"),  surpport = c("Democratic","Independent","Republic"))
install.packages("vcdExtra")
library(vcdExtra)
mosaic(ex19)
#인종에 따른 정당지지도가 많이 달라 보인다.

chisq.test(ex19)
chisq.test(ex19,correct =FALSE)
library(MASS)
loglm(~person+surpport,data=ex19)
# p값이 0.05보다 작게 나오므로 유의수준 5%에 인종과 정당지지도는 독립이라 할 수 없음.

summary(chisq.test(ex19))
chi_test = chisq.test(ex19)
chi_test$observed
chi_test$expected
chi_test$residuals
chi_test$stdres
mosaic(ex19,residuals=chi_test$stdres,gp=shading_Friendly)
# 무소속에 대한 지지도는 인종에 따라 다르지 않아 독립성에 영향을 주지 않는다.
# 흑인 공화당 지지자와 백인 민주당 지지자는 큰 양의 잔차값을 얻고
# (독립성 가정 하 기대한 것보다 지지자가 많음)
# 흑인 민주당 지지자와 백인 공화당 지지자는 큰 음의 잔차값을 얻었다.
# (독립성 가정 하 기대한 것보다 지지자가 적음)
install.packages("fmsb")
library(fmsb)
oddsratio(871,873,302,43)
# 공화당보다 민주당을 지지하는 오즈비가 백인에서보다 흑인에서 약 7배 높게 나옴

ex191 = ex19[,-3]
x1 = loglm(~person+surpport,data=ex191)

ex192 = matrix(c(1315,873,382,43),nrow=2,byrow = T)
ex192
dimnames(ex192) = list(person=c("white","black"),  surpport = c("Demo+Independent","Republic"))
x2 = loglm(~person+surpport,data=ex192)

x1$lrt + x2$lrt
x3 = loglm(~person+surpport,data=ex19)
x3$lrt
# 가능도비 통계량의 부분합이 원래 표의 가능도비 통계량의 값과 같음.
# 공화당과 민주당/무소속을 지지하는 데 있어 인종의 차이가 있음을 반영함.