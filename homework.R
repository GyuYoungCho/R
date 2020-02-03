library(data.table) 
library(randomForest)
library(glmnet)
setwd("C:/Users/Gyu/Downloads/homework")

dat_train = fread("dat_hw.csv")
dat_test = fread("dat_pred.csv")

MSE = function(real,pred){mean((real-pred)^2)}

dat = dat_train %>% bind_rows(dat_test) # 데이터 합침

colnames(dat) = c("date","medi_num","temp_avg","temp_max","temp_min","humi_avg","humi_min","gangsu","wind","wind_max","sun","solar","atmos","sulfur","dust","dioxide","ozone","monoxide","blog","twitter","news")

dat[is.na(dat$gangsu),"gangsu"] = 0 # NA값 0으로

dat$temp_range = dat$temp_max - dat$temp_min #일교차
dat$air_pol = dat$sulfur + dat$dioxide + dat$monoxide + dat$ozone + dat$dust #대기오염정ㄷ
dat$social_score = dat$blog +dat$twitter +dat$news #소셜점수 
dat$sunsoloar = dat$sun + dat$solar # 햇빛량
dat$ultra = dat$solar / dat$ozone /10 #자외선

datcha = as.character(dat$date)
dat$day <- weekdays(as.Date(paste0(substr(datcha[],1,4),"-",substr(datcha[],5,6),"-",substr(datcha[],7,8)))) #요일변수

dat1_train_train = dat[1:668] # train 데이터 
dat1_train_test = dat[670:698] # test 데이터

form1 = as.formula(paste0("medi_num~",paste0(colnames(dat)[c(3,6,8,9,13,22:26)],collapse = "+"))) #필요한 변수 적합 


fit1 = lm(form1,data=dat1_train_train) # 선형 회귀분석 적합
pred1 = predict(fit1,newdata=dat1_train_test,type="response") # 예측
MSE(dat1_train_test$medi_num,pred1)

fit2 = glm(form1,data=dat1_train_train,family=Gamma(link="log"))
pred2 = predict(fit2,newdata=dat1_train_test,type="response")
MSE(dat1_train_test$medi_num,pred2)

train_x = model.matrix(form1,data=dat1_train_train) # 적합
train_x = data.matrix(train_x[,-1]) #1을 제거. 
train_y = dat1_train_train$medi_num
test_x = model.matrix(form1,data=dat1_train_test) # 적합
test_x = data.matrix(test_x[,-1]) #1을 제거. 
test_y = dat1_train_test$medi_num

lambda1 = 10^seq(-4,4,by=0.1)
fit_rid = cv.glmnet(x=train_x,y=train_y,alpha=0)

best_lamb1 = fit_rid$lambda.min
best_lamb2 = fit_rid$lambda.1se

fit3 = glmnet(x=train_x,y=train_y,alpha=0,lambda=best_lamb1)
fit4 = glmnet(x=train_x,y=train_y,alpha=0,lambda=best_lamb2)


pred3 = predict(fit3,newx=test_x,type="response")
pred4 = predict(fit4,newx=test_x,type="response")

MSE(dat1_train_test$medi_num,pred3)
MSE(dat1_train_test$medi_num,pred4)

llll = coef(fit3)
llll@i
#변수선택으로 새로운데이터.
coef_num1 = coef(fit3)@i[-1]
coef_num2 = coef(fit4)@i[-1]
#lasso는 결과가 잘 안나오나바.
train_x1 = train_x[,coef_num1]
train_x2 = train_x[,coef_num2]

test_x1 = test_x[,coef_num1]
test_x2 = test_x[,coef_num2]

fit5 = lm(pm10_avg~.,data=data.frame(train_x1,pm10_avg = train_y)) # 적합
fit6 = glm(pm10_avg~., family=Gamma(link="log"),data=data.frame(train_x1,pm10_avg = train_y))

pred5 = predict(fit5,newdata=as.data.frame(test_x1),type="response")
pred6 = predict(fit6,newdata=as.data.frame(test_x1),type="response")

MSE(dat1_train_test$medi_num,pred5)
MSE(dat1_train_test$medi_num,pred6)

fit7 = randomForest(form1,
                   data=dat1_train_train,
                   ntree=500,
                   mtry=5,
                   importance=T)

importance(fit7)

fit8 = randomForest(form1,
                    data=dat1_train_train,
                    ntree=500,
                    importance=T)

importance(fit8)

pred7 = predict(fit7,newdata=dat1_train_test,type="response")
pred8 = predict(fit8,newdata=dat1_train_test,type="response")

MSE(dat1_train_test$medi_num,pred7)
MSE(dat1_train_test$medi_num,pred8)

# pred8의 mse가 가장 작으므로 이 모형을 선택 

dat2_train = dat[1:698]
dat2_test = dat[699:728]

final_fit = randomForest(form1,
                         data=dat2_train,
                         ntree=500,
                         importance=T)
importance(final_fit)
pred = predict(final_fit, newdata = dat2_test, type="response")

dat2_test$medi_num = pred

write.csv(dat2_test,"dat_pred1.csv", row.names = FALSE)

