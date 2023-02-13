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

for (m in 4:12){
  month_name=month[m]
  res <- training_data$residuals[training_data$month == m]
  testset <- test_data$residuals[test_data$month == m]
  
  a<-adf.test(res)
  a$p.value
  b<-kpss.test(res)
  b$p.value
  if (a$p.value <= 0.05 & b$p.value >= 0.05){ 
    d=0}else{d=1}
  
  test_df<- data.frame(matrix(nrow=(p_limit*q_limit),ncol = 4))
  colnames(test_df)<- c("AICC","p","q","res_ratio")
  forecast_vector <- vector(length=length(testset))
  test_df$res_ratio <-1
  
  for (k in 1:q_limit){
    q=k
    for (i in 1:p_limit){
      tryCatch({
        cat(paste('processing ARIMA:',i,d,q,"\n\nModel:",(k-1)*q_limit+i,"of",p_limit*q_limit,
                  "for month:",month_name,"\n\nResidual improvement by",
                  round(1-min(test_df$res_ratio,na.rm = T),digits = 4)*100,"%"))
        grid_model <- Arima(res, order = c(i, d, q))
        test_df$AICC[((k-1)*q_limit+i)] <- AICc(grid_model)  
        test_df$p[((k-1)*q_limit+i)] <- i
        test_df$q[((k-1)*q_limit+i)] <- k
        forecasted_short_res <-forecast(grid_model, h=24,biasadj=TRUE,bootstrap = TRUE)
        forecast_vector[1:length(forecast_vector)] <- forecasted_short_res$mean
        res_predicted <- testset - forecast_vector
        test_df$res_ratio[((k-1)*q_limit+i)] <- sum(abs(res_predicted)) / sum(abs(testset))
        
        if (i*k == q_limit*p_limit ){ cat('\nDone')}
        else{cat('\014')}
      },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      
    }}
  
  write.csv(test_df,paste0("./",country,"./data/short_term_ARIMA_models_",month_name,".csv"),row.names = F)
  test_df<- test_df[is.na(test_df$res_ratio)==F,]
  p_order <- test_df$p[test_df$res_ratio==min(test_df$res_ratio,na.rm = T)]
  q_order <- test_df$q[test_df$res_ratio==min(test_df$res_ratio,na.rm = T)]
  
  best_model <- Arima(res, order = c(p_order, d, q_order))
  save(best_model,file=paste0("./",country,"./models/shortterm_stochastic/fit_",month_name,".Rdata"))
  
}






