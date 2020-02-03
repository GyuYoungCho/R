# 유형에 따른 분석기법

# 평균에 대한 구간 추정     : T-test                t.test(x)
# 한 그룹의 평균            : One Sample T-test     t.test(x, mu = , alternative = )
# 한 그룹의 두 변수 
# 연관성 높은 두 그룹       : Paired T-test         t.test(x2 - x1)
# 두 그룹의 평균            : Two Sample T-test     t.test(y ~ x, var.equal = , data = )
# 세 그룹 이상의 평균       : ANOVA / ANCOVA        lm / anova / glht("Dunnett") / interaction.plot
# 하나의 비율               : Binomial test         binom.test
# 한 그룹의 비율 구간 추정
# 두 그룹 이상의 비율       : Proportions test      prop.test(x, n, p)
# 정규성 검정               : Shapiro-Wilk test     shapiro.test(x)
# 등분산 검정               : Variance test         var.test(y ~ x, data = )
# 독립성 검정               : Chi-squard test       chisq.test(x)
# 상관계수                  : Correlation test      cor.test
# 예측과 추정               :                       predict
# 회귀분석 모델 선택        :                       step / regsubsets

# 비모수적 방법 (정규분포를 따르지 않는 경우)
# One Sample / Paired T-test 일 경우    : Wilcoxon Signed-Rank Test     wilcox.test(x)
# Two Sample T-test 일 경우             : Wilcoxon Rank-Sum Test        wilcox.test(y ~ x, data = )
# ANOVA일 경우                          : Kurskal-Wallis Test           kruskal.test

# %>% dataframe %>% group_by() %>% select() %>% summarise() %>% filter()
#xtab(~그룹변수1+그룹변수2,DATA)
#aes(x축, y축)

Sys.Date()
s1 <- c(91, 87, 95, 96, 89, 87, 86, 85, 92, 93)
s2 <- c(89, 86, 85, 92, 93, 91, 90, 95, 87, 89)
s3 <- c(89, 86, 78, 99, 95, 87, 89, 86, 85, 92)
rm(list = ls())
df = data.frame(score = c(s1,s2,s3))
df$name = c("a","b","c")
df
aggregate(score~name,data=df,mean)


library(reshape2)
attach(tips)
summary(tips)
install.packages("psych")
library(psych)
describe(tips)
tips2 = tips
tips2$size = factor(tips2$size)
summary(tips2)
quantile(tips2$tip)
tips2$tip[1]=100
hist(tip, 20, probability = T, xlim = c(0,10), ylim = c(0,0.5)) 
lines(density(tip),col="blue")

require(ggplot2)
sales <- read.csv("data/emp_monthly_score.csv", header=T)


# function
# x : 표현할 값, angle1 : 좌측 게이지 각도, angle3 : 우측 게이지 각도, title : Dashboard 제목

# matrix(c(-1,0,0,1,1,0), 3, 2, byrow=T)
#       [,1] [,2]
# [1,]   -1    0
# [2,]    0    1
# [3,]    1    0

# dash_t <- function(x, angle1, angle3, title) {
#   i <- matrix(c(-1,0,0,1,1,0), 3, 2, byrow=T)
#   plot(i, xlab="", ylab="", axes=F, type="n")
#   x.cir <- cos(seq(0,180,1)*pi/180)
#   y.cir <- sin(seq(0,180,1)*pi/180)
#   
#   #첫번째 polygon
#   cir <- rbind(cbind(x.cir[(181-angle1):181], y.cir[(181-angle1):181]) ,
#                cbind(0.8*x.cir[181:(181-angle1)], 0.8*y.cir[181:(181-angle1)]),
#                col="green")
#   polygon(cir, col="green", border="white")
#   
#   #두번째 polygon
#   cir <- rbind(cbind(x.cir[(angle3+1):(181-angle1)], y.cir[(angle3+1):(181-angle1)]) ,
#                cbind(0.8*x.cir[(181-angle1):(angle3+1)], 0.8*y.cir[(181-angle1):(angle3+1)]),
#                col="blue")
#   polygon(cir,col="blue", border="white")
# #세번째 polygon
# cir <- rbind(cbind(x.cir[1:(angle3+1)], y.cir[1:(angle3+1)]) ,
#              cbind(0.8*x.cir[(angle3+1):1], 0.8*y.cir[(angle3+1):1]),
#              col="red")

# polygon(cir,col="red", border="white")   
# 
# for (i in 0:36) segments(0,0,cos(i*pi/36),sin(i*pi/36),col="white")
# 
# arrows(0,0.1,0.75*cos(pi-x*pi),0.75*sin(pi-x*pi),lwd=3,length=0.2)  # 화살표
# text(0,0.1,"o",cex=3)  # 가운데 동그라미
# title(title)
# }
# 
# empNames <- sales$name
# 
# windows(height=4, width=6)
# # oma : 각 plot간의 좌,우,상,하 간격.
# par(mfrow=c(3,3), oma=c(3,3,3,3), mar=c(1,1,1,2))
# 
# for (i in 1:nrow(sales)) {
#   dash_t(sales[i,5], 60, 30, empNames[i])
#   text(0,0.5, paste(sales[i,5]*100,"%"), cex=1.5)
# }

prop.test(c(60,120),c(150,250),alternative = "less")
# 자료가 순서형/순위자료일 경우.
cor(x, y, method = "kendall")

attach(attitude)
cov(attitude)
pairs.panels(attitude)  #상관테이블쓰.
cor.test(rating,complaints)  # 상관분석쓰.
cor.test(tt)

# model <- lm(y ~ x, data)  : 회귀분석
# plot(model)               : 회귀분석 관련 그래프 출력
#                           (plot, residuals vs fitted, Normal QQ, scale-location, residuals vs leverage)
# summary(model)            : 회귀분석 결과 출력
# abline(model)             : 그래프에 직선 추가
x11()
head(cars)
out = lm(dist~speed,data=cars) # 회귀분석(종속변수~설명변수)
plot(out)
anova(out)
plot(dist ~ speed, data = cars, col = "blue")
abline(out, col = "red")
out = lm(dist~speed + 0 ,data=cars)#intercept 없는모델.
out

# 오차항(e, 잔차)이 추세를 보인다면 무언가 중요 정보가 모형에 포함되지 않았다는 의미.


par(mfcol=c(1,2))
plot(log(dist) ~ speed + 0, data = cars)
plot(sqrt(dist) ~ speed + 0, data = cars)
out2 <- lm(sqrt(dist) ~ speed + 0, data = cars)
summary(out2)
shapiro.test(out2$residuals)    # 잔차가 정규분포를 따른다 (p-value > 0.05)
# 최종 모형으로 추정된 회귀식
# sqrt(dist) =  0.397 * speed

ns = data.frame("speed"=c(10,30))
predict(out2,ns)  #결과값은 sqrt(dist) 임 실제값은 변환 필요.

# "평균" 제동거리의 95% 신뢰구간
predict(out2, ns, interval = "confidence")

# 새로운 한 차량에 대한 95% 예측구간
predict(out2, ns, interval = "prediction")
(fitted(out2)[7])^2     
pred.dist = fitted(out2)^2
plot(cars$speed,pred.dist)
# 관측치 속도의 최대값 25 --> 데이터 범위 밖에서 예측하는 것은 주의 해야한다!


# Outlier
# - 측정상/실험상의 과오로 모집단에 속하지 않는다고 의심이 될 정도로 정상범위 밖에 떨어진 점
# - 대개 큰 잔차를 가짐

# Influential Points
# 소수의 관측치이지만 통계량에 큰 영향을 미침
# Leverage plot 에서 점선 영역 밖에 위치 (Cook' distance)


# 다중회귀 : 경력 증가시 적성검사 점수 증가로 인한 연봉 증가까지 포함된 관계
# experience ~ score 의 cor() = 0.34
#model <- lm(salary ~ experience + score, data = df)
#  pairs  PLOT 여러 개 그림.


# 다중 공선성.
# 독립변수들이 서로 높은 상관관계를 가지면 회귀계수의 정확한 추정이 어렵다.
# ---> 모형 선택 방법론을 적용하여 가장 적절한 변수를 선택할 수 있다.

pairs.panels(attitude)
plot(attitude[ , c("rating", "complaints", "learning")])

a <- lm(rating ~ complaints + learning, data = attitude)
summary(a)


# learning의 t-test p-value 값을 보면 유의하지 않다. 
# 하지만 rating과 상관관계가 없는 것이 아니다. 
# complaints 와의 상관관계도 있기 때문에 rating 변수에 대한 역할이 작아보일 뿐이다.

 # 모형 선택법.
# *** 해당 업무분야에서 반드시 들어가야 하는 변수는 고정 !!!
# (1) Forward selection
# --- 가장 유의한 변수부터 하나씩 추가 (R-sq 기준)
# --- 변수값의 작은 변동에도 결과가 크게 달라져 안정성 부족

# (2) Backward selection
# --- 모든 변수를 넣고 가장 기여도가 낮은 것부터 하나씩 제거
# --- 전체 변수 정보를 이용하는 장점
# --- 변수의 갯수가 많은 경우 사용 어려움. 안정성 부족.

# (3) Stepwise selection
# --- Forward selection과 backward selection을 조합
# --- 새로운 변수 추가 후에 기존 변수의 중요도가 약화되면 그 변수 제거

# (4) All Subsets Regression
# --- 모든 가능한 모형을 비교하여 최적의 모형선택
# --- 여러 모형 중 최소 AIC, BIC, Mallow’s Cp 또는 최대 adjusted R-sq를 갖는 모형을 선택
# --- 모형의 복잡도에 벌점을 주는 방법. AIC (Akaike information criterion), BIC (Bayesian ...)

# Backward selection

out <- lm(rating ~ ., attitude)
summary(out) 
anova(out)# 각 회귀계수 t검정 p-value 기준 선별. critical 제거.
out2 <- lm(rating ~ . - critical, data = attitude)
summary(out2)

# Backward selection 자동화
backward <- step(out, direction = "backward", trace = T)

# Stepwise selection

both <- step(out, direction = "both", trace = F)
both

# All Subsets Regression

install.packages("leaps")
library(leaps)

leap <- regsubsets(rating ~ ., attitude, nbest = 5)   # size당 5개의 최적 모형 저장
plot(leap)
plot(leap, scale = "adjr2")  # adjusted r-squred 기준
out3 <- lm(rating ~ critical+learning, data = attitude)
summary(out3)
