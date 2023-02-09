
orakle.long_term_lm<- function(longterm_all_data,training_set_ratio=0.1,testquant = 500){


training_set=nrow(longterm_all_data)- round(nrow(longterm_all_data)*training_set_ratio)
training_data=longterm_all_data[1:training_set,]
test_data=longterm_all_data[(training_set+1):nrow(longterm_all_data),]
variables <- colnames(longterm_all_data)[4:(ncol(longterm_all_data))]

f <- as.formula(
  paste("avg_hourly_demand", 
        paste(variables, collapse = " + "), 
        sep = " ~ "))


globalmodel <- lm(f , data=training_data, na.action = "na.fail")

message("Getting all possible combinations, this might take a while.")

suppressMessages(combinations <- MuMIn::dredge(globalmodel))

for (i in 1:nrow(combinations)){
  if (all(is.na(combinations[i,2:(ncol(combinations)-5)]))){
    combinations<-combinations[-i,]
  }}
row.names(combinations) <- NULL


rdm=421 
ctrl <- caret::trainControl(method = "repeatedcv", number = 5, repeats = 5)


results <- data.frame(matrix(nrow = testquant,ncol=4))
colnames(results)= c("RMSE_k_fold","Rsquare_k_fold","MAE_k_fold","index")

message(paste("Cross-validating the best",testquant,"models."))

for (i in 1:testquant){
  tryCatch({
  cat(paste0("Cross Validation\n",round(i / testquant * 100), '% completed.  (Reduce testquant argument if it takes too long.)'))  
  predictor_names=combinations[i,2:(ncol(combinations)-5)] 
  results$index[i] <- i
  model_variables=colnames(predictor_names)[complete.cases(t(predictor_names))] 
  lm_formula <- as.formula(paste("avg_hourly_demand", paste(model_variables, collapse = " + "), 
                                 sep = " ~ "))
  
  set.seed(rdm) 
  Kfold <- caret::train(lm_formula, data = training_data, 
                 trControl = ctrl, method = "lm")
  
  results$RMSE_k_fold[i] <- Kfold$results$RMSE
  results$Rsquare_k_fold[i] <- Kfold$results$Rsquared
  results$MAE_k_fold[i] <- Kfold$results$MAE
  if (i == testquant) cat(': Done')
  else cat('\014')
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

best_index_RMSE = results$index[results["RMSE_k_fold"] == min(results["RMSE_k_fold"],na.rm = T)]
best_value_RMSE = results[best_index_RMSE, "RMSE_k_fold"]
limit_RMSE = best_value_RMSE * 1.5 
message(paste("Lowest RMSE of",round(best_value_RMSE),"for model no.",best_index_RMSE,". RMSE limit is set to"
      ,round(limit_RMSE)))

best_index_MAE = results$index[results["MAE_k_fold"] == min(results["MAE_k_fold"],na.rm = T)]
best_value_MAE = results[best_index_MAE, "MAE_k_fold"]
limit_MAE = best_value_MAE * 1.5 
message(paste("Lowest MAE of",round(best_value_MAE),"for model no.",best_index_MAE,". MAE limit is set to"
      ,round(limit_MAE)))

best_index_Rsquare = results$index[results["Rsquare_k_fold"] == max(results["Rsquare_k_fold"],na.rm = T)]

best_value_Rsquare = results[best_index_Rsquare, "Rsquare_k_fold"]
limit_Rsquare = best_value_Rsquare / 1.3 
paste("Highest Rsquare of",round(best_value_Rsquare,3),"for model no.",best_index_Rsquare,". Rsquare limit is set to"
      ,round(limit_Rsquare,3))

mask_RMSE = results["RMSE_k_fold"] <= limit_RMSE     
mask_MAE = results["MAE_k_fold"] <= limit_MAE 
mask_Rsquare = results["Rsquare_k_fold"] >= limit_Rsquare

candidates = results[mask_Rsquare & mask_RMSE & mask_MAE,]
dist <- data.frame(matrix(nrow=nrow(candidates),ncol=3))
colnames(dist) <- c("distance","model_no","max_single_distance")

for (i in (1:nrow(candidates))){
  tryCatch({
  ind=candidates$index[i]  
  x=combinations[ind,2:(ncol(combinations)-5)]
  variables=colnames(x)[complete.cases(t(x))]
  
  
  f <- as.formula(paste("avg_hourly_demand",paste(variables, collapse = " + "), 
          sep = " ~ "))
  
  model <- lm(f, data = training_data)
  
  LT <- predict(model,longterm_all_data)
  
  dist[i,1]<- sum(abs(test_data$avg_hourly_demand- LT[(training_set+1):nrow(longterm_all_data)]))
  dist[i,2]<- ind
  dist[i,3]<- max(abs(longterm_all_data$avg_hourly_demand- LT))
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }


ordered_dist <- dist[order(dist$distance),]

if (testquant>500){
best_model_num <- ordered_dist$model_no[which.min(ordered_dist$max_single_distance[1:50])]
}else{
best_model_num <- ordered_dist$model_no[which.min(ordered_dist$max_single_distance[1:5])]
}

x=combinations[best_model_num,]
x=x[,2:(ncol(combinations)-5)]
variables=colnames(x)[complete.cases(t(x))]
f <- as.formula(paste("avg_hourly_demand", paste(variables, collapse = " + "), 
        sep = " ~ "))

best_lm_model<- lm(f,data=training_data)
results<- predict(best_lm_model,longterm_all_data)

lt_plot <- ggplot(longterm_all_data)+geom_line(aes(year,avg_hourly_demand,color="actual"))+
  geom_line(aes(year,results,color="fitted"))+xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
  geom_vline(xintercept=longterm_all_data$year[training_set],linetype=2)+
  ggthemes::theme_foundation(base_size=14, base_family="sans")+
  xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
  ggtitle(paste("Long Term Model Results -",country,"\n"))+
  theme(plot.title = element_text(face = "bold",
                                  size = rel(1.2), hjust = 0.5),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = NA),
        axis.title = element_text(face = "bold",size = rel(1)),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(), 
        axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size= unit(0.2, "cm"),
        plot.margin=unit(c(10,5,5,5),"mm"),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold"))+
  theme(legend.title = element_blank())

lt_plot2 <- ggplot(longterm_all_data)+geom_line(aes(year,avg_hourly_demand,color="actual"))+
  geom_line(aes(year,results,color="fitted"))+xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
  geom_vline(xintercept=longterm_all_data$year[training_set],linetype=2)+
  ggthemes::theme_foundation(base_size=14, base_family="sans")+
  xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
  ggtitle(paste("Long Term Model Results -",country,"\n"))+
  theme(plot.title = element_text(face = "bold",
                                  size = rel(1.2), hjust = 0.5),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = NA),
        axis.title = element_text(face = "bold",size = rel(1)),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(), 
        axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size= unit(0.2, "cm"),
        plot.margin=unit(c(10,5,5,5),"mm"),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold"))+
  theme(legend.title = element_blank())+
  theme(axis.title=element_text(size=23))+
  theme(legend.text=element_text(size=23))+
  theme(axis.text=element_text(size=20))+
  theme(plot.title = element_text(size=26))


  print(lt_plot)
  
  if (! file.exists(country)){ 
    dir.create(country)} 
  if (! file.exists(paste0("./",country,"/models"))){ 
    dir.create(paste0("./",country,"/models"))}  
  if (! file.exists(paste0("./",country,"/plots"))){ 
    dir.create(paste0("./",country,"/plots"))}
  if (! file.exists(paste0("./",country,"/models/longterm"))){ 
    dir.create(paste0("./",country,"/models/longterm"))}  
  save(best_lm_model,file=paste0("./",country,"/models/longterm/best_lm_model.Rdata"))

  ggsave(file=paste0("./",country,"/plots/Long_term_results.png"), plot=lt_plot2, width=12, height=8)
  
  longterm_all_data$longterm_model_predictions <- results
  return(longterm_all_data)
}
a<- orakle.long_term_lm(longterm_all_data)

summary(a)
#Lasso ----
library(glmnet)
#define response variable
y <- training_data$avg_hourly_demand
x <- data.matrix(training_data[, 4:13])
x_test <-data.matrix(longterm_all_data[,4:13])


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

predict(best_model, s = best_lambda, newx = x_test)
longterm_all_data[,3]
