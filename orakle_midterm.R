library(MuMIn)

temps <- midterm_all$temperature_data
midterm_all_data <- midterm_all$midterm
#midterm_all_data[,37:56]<- temps[,2:21]
#colnames(midterm_all_data)<-gsub(' ','_',colnames(midterm_all_data))
#midterm_all_data<- midterm_all_data[-c(1461:1825),]
training_set_ratio=0.2

month_list=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Nov","Dec")

for (i in 1:length(month_list)){
midterm_all_data[month_list[i]]=0
midterm_all_data[midterm_all_data$month==i,month_list[i]]<- 1
}

midterm_all_data$wday<- lubridate::wday(midterm_all_data$date,label = T)
weekday_list=as.character(unique(midterm_all_data$wday))



for (i in 1:length(weekday_list)){
  midterm_all_data[weekday_list[i]]=0
  midterm_all_data[midterm_all_data$wday==weekday_list[i],weekday_list[i]]<- 1
}
midterm_all_data$HD <-0
midterm_all_data$CD <-0
for (i in 1:nrow(midterm_all_data)){
  if (midterm_all_data$weighted_temperature[i] < 18){  
  midterm_all_data$HD[i]<- 18-midterm_all_data$weighted_temperature[i]
  }else{  midterm_all_data$CD[i]<- midterm_all_data$weighted_temperature[i] -18
}
}

midterm_all_data$HD2 <- midterm_all_data$HD^2
midterm_all_data$HD3 <- midterm_all_data$HD^3
midterm_all_data$CD2 <- midterm_all_data$CD^2
midterm_all_data$CD3 <- midterm_all_data$CD^3
midterm_all_data$weighted_temperature2 <- midterm_all_data$weighted_temperature^2
midterm_all_data$weighted_temperature3 <- midterm_all_data$weighted_temperature^3

midterm_all_data$HDlag1 <- dplyr::lag(midterm_all_data$HD, n = 1)
midterm_all_data$HDlag1[1]<- midterm_all_data$HD[1]
midterm_all_data$HDlag2 <- dplyr::lag(midterm_all_data$HD, n = 2)
midterm_all_data$HDlag2[1:2]<- midterm_all_data$HDlag1[1:2]

midterm_all_data$CDlag1 <- dplyr::lag(midterm_all_data$CD, n = 1)
midterm_all_data$CDlag1[1]<- midterm_all_data$CD[1]
midterm_all_data$CDlag2 <- dplyr::lag(midterm_all_data$CD, n = 2)
midterm_all_data$CDlag2[1:2]<- midterm_all_data$CDlag1[1:2]

midterm_all_data$weighted_temperaturelag1 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 1)
midterm_all_data$weighted_temperaturelag1[1]<- midterm_all_data$weighted_temperature[1]
midterm_all_data$weighted_temperaturelag2 <- dplyr::lag(midterm_all_data$weighted_temperature, n = 2)
midterm_all_data$weighted_temperaturelag2[1:2]<- midterm_all_data$weighted_temperaturelag1[1:2]


training_set=nrow(midterm_all_data)- round(nrow(midterm_all_data)*training_set_ratio)
training_data=midterm_all_data[1:training_set,]
test_data=midterm_all_data[(training_set+1):nrow(midterm_all_data),]


variables3 <- colnames(midterm_all_data)[c(9:(ncol(midterm_all_data)))]


f3 <- as.formula(  paste("seasonal_avg_hourly_demand",paste(variables3, collapse = " + "), 
        sep = " ~ "))

globalmodel3 <- lm(f3 , data=training_data, na.action = "na.omit")
MuMIn::AICc(globalmodel3)
summary(globalmodel3)

library(glmnet)
#define response variable
y <- training_data$seasonal_avg_hourly_demand
y_all <- midterm_all_data$seasonal_avg_hourly_demand
y_test <- test_data$seasonal_avg_hourly_demand
x <- data.matrix(training_data[, 9:ncol(midterm_all_data)])
x_all <- data.matrix(midterm_all_data[, 9:ncol(midterm_all_data)])
x_test<- data.matrix(test_data[, 9:ncol(training_data)])
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

a<-predict(best_model, s = best_lambda, newx = x)
testlasso<-predict(best_model, s = best_lambda, newx = x_all)
testlm<-predict(globalmodel3, newdata=midterm_all_data)

onlytestlasso<-predict(best_model, s = best_lambda, newx = x_test)
onlytestlm<-predict(globalmodel3, newdata=test_data)
#library(MLmetrics)
MLmetrics::RMSE(a,y)
MLmetrics::RMSE(globalmodel3$fitted.values,y)
MLmetrics::RMSE(testlasso,y_all)
MLmetrics::RMSE(testlm,y)
MLmetrics::RMSE(onlytestlm,y_test)



## Ridge----

grid = 10^seq(10, -2, length = 100)
ridge_mod = glmnet(x, y, alpha = 0, lambda = grid)

dim(coef(ridge_mod))
plot(ridge_mod)  


cv.out = cv.glmnet(x, y, alpha = 0)
bestlam = cv.out$lambda.min
bestlam
ridge_pred = predict(ridge_mod, s = bestlam, newx = x)

MLmetrics::RMSE(ridge_pred,y)



library(ggplot2)


ggplot()+ geom_line(aes(1:length(y_all),y_all,color="actual"))+
  geom_line(aes(1:length(y_all),testlm,color="lm"))+
  geom_line(aes(1:length(y_all),testlasso,color="lasso"))+
  geom_vline(xintercept = 1460,linetype=2)

ggplot()+ geom_line(aes(1:length(y_test),y_test,color="actual"))+
  geom_line(aes(1:length(y_test),onlytestlm,color="lm"))#+
  geom_line(aes(1:length(y_test),onlytestlasso,color="lasso"))


# residuals ------

res<- midterm_all_data$seasonal_avg_hourly_demand-predict(globalmodel3, newdata=midterm_all_data)

write.csv(res,"resforNN.csv",row.names = F)
write.csv(midterm_all_data,"midterm_nn.csv",row.names = F)


