library(tseries)
library(forecast)
library(MuMIn)


country="PL"
short_term_data <- read.csv(paste0("./",country,"./data/short_term_data.csv"))

#
if (! file.exists(paste0("./",country,"./models/"))){ 
  dir.create(paste0("./",country,"./models/"))}
if (! file.exists(paste0("./",country,"./data"))){ 
  dir.create(paste0("./",country,"./data"))}
if (! file.exists(paste0("./",country,"./models/shortterm_stochastic"))){ 
  dir.create(paste0("./",country,"./models/shortterm_stochastic"))}

# calculate residuals
short_term_data$residuals <- short_term_data$hourly_demand_trend_and_season_corrected - short_term_data$short_term_lm_model_predictions 

# split data into training and test set 
training_set_ratio=0.2
training_set=nrow(short_term_data)- round(nrow(short_term_data)*training_set_ratio)
training_data=short_term_data[1:training_set,]
test_data=short_term_data[(training_set+1):nrow(short_term_data),]

month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

p_limit=30
q_limit=30

no_cores <- parallel::detectCores(logical = TRUE)
used_cores <- floor(no_cores*0.75)
if (used_cores > 32) {used_cores=32}

cl <- parallel::makeCluster(used_cores)  
doParallel::registerDoParallel(cl) 


for (m in 4:12){
  
  month_name=month[m]
  print(month_name)
  res <- training_data$residuals[training_data$month == m]
  testset <- test_data$residuals[test_data$month == m]
  forecast_vector <- vector(length=length(testset))
  
  a<-adf.test(res)
  b<-kpss.test(res)

  if (a$p.value <= 0.05 & b$p.value >= 0.05){ 
    d=0}else{d=1}
  

  parallel::clusterExport(cl,list("res","testset","forecast_vector","d"))
  
  ar1_fct<-function(p,q_limit){
    ar2_fct <- function(q){
      tryCatch({ 
        grid_model <- forecast::Arima(res,order=c(p,d,q))
        forecasted_short_res <-forecast::forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
        forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
        res_predicted <- testset - forecast_vector
        print(c(p,0,q))
        return(c(MuMIn::AICc(grid_model),p,q,sum(abs(res_predicted)) / sum(abs(testset))))},
        error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    x<-lapply(1:q_limit,ar2_fct)
    x_results <- as.data.frame(do.call(rbind, x)) 
    return(x_results)
  }
  

  results_list <-  parallel::clusterMap(cl,ar1_fct,1:p_limit,q_limit)
  
  results <-as.data.frame(do.call(rbind, results_list))
  colnames(results) <- c("AICC","p","q","res_ratio")
  
  write.csv(results,paste0("./",country,"./data/short_term_ARIMA_models_",month_name,".csv"),row.names = F)
  results<- results[is.na(results$res_ratio)==F,]
  p_order <- results$p[results$res_ratio==min(results$res_ratio,na.rm = T)]
  q_order <- results$q[results$res_ratio==min(results$res_ratio,na.rm = T)]
  
  best_model <- Arima(res, order = c(p_order, d, q_order))
  save(best_model,file=paste0("./",country,"./models/shortterm_stochastic/fit_",month_name,".Rdata"))
  
}







####### TESTING ----
no_cores <- parallel::detectCores(logical = TRUE)

used_cores <- floor(no_cores*0.75)
if (used_cores > 32) {used_cores=32}

cl <- parallel::makeCluster(used_cores)  
doParallel::registerDoParallel(cl) 

res <- training_data$residuals[training_data$month == 1]
testset <- training_data$residuals[test_data$month == 1]
forecast_vector <- vector(length=length(testset))

ar1_fct<-function(p,q_order){
   ar2_fct <- function(q){
   tryCatch({ 
   grid_model <- forecast::Arima(res,order=c(p,d,q))
   forecasted_short_res <-forecast::forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
   forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
   res_predicted <- testset - forecast_vector
   print(c(p,0,q))
   return(c(MuMIn::AICc(grid_model),p,q,sum(abs(res_predicted)) / sum(abs(testset))))},
   error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
   }
  x<-lapply(1:q_order,ar2_fct)
  x_results <- as.data.frame(do.call(rbind, x)) 
  return(x_results)
}

system.time(
lapply(1:p_order,ar1_fct,q_order)
)

q_order=10
p_order=10

clusterExport(cl,list("res","testset","forecast_vector","d"))

system.time(
results_list <-  clusterMap(cl,ar1_fct,1:p_order,q_order)
)
results <-as.data.frame(do.call(rbind, results_list))



doParallel::stopImplicitCluster()
parallel::stopCluster(cl)


### Legacy ----

for (k in 1:q_limit){
  q=k
  for (i in 1:p_limit){
    tryCatch({
      # cat(paste('processing ARIMA:',i,d,q,"\n\nModel:",(k-1)*q_limit+i,"of",p_limit*q_limit,
      #           "for month:",month_name,"\n\nResidual improvement by",
      #           round(1-min(test_df$res_ratio,na.rm = T),digits = 4)*100,"%"))
      grid_model <- Arima(res, order = c(i, d, q))
      test_df$AICC[((k-1)*q_limit+i)] <- AICc(grid_model)  
      test_df$p[((k-1)*q_limit+i)] <- i
      test_df$q[((k-1)*q_limit+i)] <- k
      forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
      forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
      res_predicted <- testset - forecast_vector
      test_df$res_ratio[((k-1)*q_limit+i)] <- sum(abs(res_predicted)) / sum(abs(testset))
      
      # if (i*k == q_limit*p_limit ){ cat('\nDone')}
      # else{cat('\014')}
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }}








########## Arima tests ----
library(forecast)

res1 <- training_data$residuals[training_data$month == 1 & training_data$wday!="Sun" &
                                  training_data$wday !="Sat"]
testset1 <- test_data$residuals[test_data$month == 1]
res2 <- training_data$residuals[training_data$month == 2]
testset2 <- test_data$residuals[test_data$month == 2]

model1 <- Arima(res2, order = c(26,0,26))
model2 <- auto.arima(res1)
checkresiduals(res1)

refit <- Arima(res1[1:2184], model=model1)  
plot(forecast(refit,h = 24))

ggplot()+ geom_line(aes(2924:3000,c(refit$fitted,forecast(refit,h = 24)$mean),color="fit"))+
  geom_line(aes(2924:3000,c(res1[2924:2976],testset2[1:24]),color="actual"))

sum(abs(testset2[1:24]-forecast(refit,h = 24)$mean))/
sum(abs(testset2[1:24]))

a <- vector(length = length(testset2))
a[1:672]<- forecast(refit,h = 24)$mean


ggplot()+ geom_line(aes(1:length(a),a,color="fit"))+
  geom_line(aes(1:length(a),testset2,color="actual"))

sum(abs(testset2-a))/
  sum(abs(testset2))


ggplot()+ geom_line(aes(1:nrow(short_term_data[short_term_data$month==2,]),short_term_data$residuals[short_term_data$month==2],color="fit"))#+
  geom_line(aes(1:length(a),testset2,color="actual"))

  

