# 다차원 척도법 Multi-Dimensional Scaling
# 개체들 사이의 유사성/비유사성을 측정하여 2차원 또는 3차원 공간상에 점으로 표현하는 분석 방법.
# 개체들간의 근접성(proximity)을 시각화하여 데이터 속에 잠재해 있는 패턴이나 구조를 찾아내는 통계 기법.
# 개체들간의 거리 계산은 유클리드 거리 행렬을 사용한다.
# 상대적 거리의 정확도를 높이기 위해 적합한 정도를 스트레스 값(stress value)으로 나타낸다.
# 
# 1. 계량적 MDS
# 
# 데이터가 연속형 변수(구간척도, 비율척도)인 경우 사용.
# 각 개체들간의 유클리드 거리 행렬을 계산하고 개체들간의 비유사성을 공간상에 표현한다.
rm(loc)
library(MASS)
data(eurodist)    
loc = cmdscale(eurodist)
x <- loc[ , 1]
y <- -loc[ , 2]

x11()
plot(x, y, type = "n", asp = 1, main = "Metric MDS")   # asp : y/x aspect ratio
text(x, y, rownames(loc), cex = 0.8)
abline(v = 0, h = 0, lty = 2, lwd = 1)
data(swiss)
swissA <- as.matrix(swiss)
dist <- dist(swissA)        # make distance matrix
mds <- isoMDS(dist)

df= heptathlon


library(ggplot2)
head(diamonds)
aggregate(price~cut+color,diamonds,mean)
diamonds %>% group_by(cut,color) %>% summarise(price=mean(price))  
# 위의 두줄이 같슴다.

install.packages("reshape")
library(reshape)
id <- c(1, 1, 1, 1, 2, 2, 2)
site <- c("a", "b", "c", "a", "a", "b", "b")
pageview <- c(1, 2, 3, 4, 5, 6, 7)
dwelltime <- c(7, 6, 5, 4, 3, 2, 1)

mydata <- data.frame(id, site, pageview, dwelltime)
tx <- melt(mydata, id = c("id", "site"))
cast(tx, id ~ site, sum, subset = variable == "pageview")
cust <- tr %>% group_by(custid) %>% summarise(visits = n())


head(iris)
nrow(iris)

plot(1:150,sort(iris$Sepal.Length,decreasing = T))
hist()
iris2 = log(iris[,1:4])
cor(iris2)
ir = prcomp(iris2)
plot(ir,type="l")
summary(ir)
PRC <- as.matrix(iris2) %*% ir$rotation



