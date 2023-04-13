library(ggplot2)

# Setting working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# load orakly library
source('./library/orakle.R')

#### Testing ----

# Get and prepare intial Data
demand_data = orakle.get_entsoE_data(2017,2021,"Luxembourg")
demand_data_filled = orakle.fill_missing_entsoE_data(demand_data)
decomposed_data = orakle.decompose_load_data(demand_data_filled)
# Longterm model
longterm <- orakle.get_historic_load_data(decomposed_data$longterm)
longterm_all_data <- orakle.get_macro_economic_data(longterm)
longterm_all_data_predicted <- orakle.long_term_lm(longterm_all_data)
# Midterm model
midterm = orakle.add_holidays_mid_term(decomposed_data$midterm)
midterm_all = orakle.get_weather_data(midterm)
midterm_all_data_predicted = orakle.mid_term_lm(midterm_all$midterm)
# Shortterm model
shortterm= orakle.add_holidays_short_term(decomposed_data$shortterm)
short_term_data <- orakle.shortterm_lm_model(shortterm)





#### Different tests, this can be ignored ---- 


system.time(
longterm_all_data_predicted <- orakle.long_term_lm(longterm_all_data)
)

## all countries registered by entsoE
for (countryname in domain_df$countrynames){
  tryCatch({
  print(countryname)
  demand_data = orakle.get_entsoE_data(2017,2021,countryname)
  demand_data_filled = orakle.fill_missing_entsoE_data(demand_data)
  decomposed_data = orakle.decompose_load_data(demand_data_filled)
  longterm <- orakle.get_historic_load_data(decomposed_data$longterm)
  longterm_all_data <- orakle.get_macro_economic_data(longterm)
  longterm_all_data_predicted <- orakle.long_term_lm(longterm_all_data)},
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


# Colombia ----
library(lubridate)
load("./CO/models/longterm/best_lm_model.Rdata")
colombian_data <- read.csv("C:/Users/Konstantin/Documents/water4whom/processed_data/Demanda_Energia_all_data.csv", fileEncoding="latin1")

entsoe_data<- colombian_data
colnames(entsoe_data)[1:2] <- c("Date","load")
entsoe_data <- entsoe_data[!is.na(entsoe_data$Date),]
entsoe_data$year <- year(entsoe_data$Date)
entsoe_data$time_interval <- "day"

entsoe_data$Date <- as.POSIXct(entsoe_data$Date,tz="UTC")
entsoe_data$country <- "CO"

summary(best_lm_model)

complete_data <- complete_data[complete_data$Date %in% entsoe_data$Date,]
hourly_data <- complete_data

write.csv(longterm,"./colombia_longterm.csv",row.names=F)
longterm <- read.csv("./colombia_longterm.csv")
longterm$country <- "CO"
midterm<- all_data
colnames(midterm)[2]<- "avg_hourly_demand"
midterm$seasonal_avg_hourly_demand <-0
for (i in min(all_data$year):max(all_data$year)){
  midterm$seasonal_avg_hourly_demand[midterm$year==i] <- midterm$avg_hourly_demand[midterm$year==i]-
    longterm$avg_hourly_demand[longterm$year==i]  
}

trend_plot<- ggplot(longterm)+geom_line(aes(year,avg_hourly_demand, color="Average hourly demand"),linewidth=1.1)+
  theme(legend.title = element_blank()) +ggtitle('Long term trend \n')+
  theme(plot.title = element_text(hjust = 0.5))+ylab("MW")

midterm_seasonality_plot <-  ggplot(midterm)+geom_line(aes(1:nrow(midterm),seasonal_avg_hourly_demand, color="Average hourly demand"),linewidth=1.1)+
  theme(legend.title = element_blank()) +ggtitle('Mid-term seasonality \n')+
  theme(plot.title = element_text(hjust = 0.5))+ylab("MW")+xlab("Day")


write.csv(midterm,"./colombia_midterm.csv",row.names=F)

###
longterm$avg_hourly_demand[12]=mean(demand_data$load[demand_data$year==2021])

mean(demand_data$load[demand_data$year==2021])
longterm_all_data2<-longterm_all_data[-12,]
library(MLmetrics)
MAE(short_term_data$lm_model_fit[1:35016] , short_term_data$hourly_demand_trend_and_season_corrected[1:35016])
short_term_data[short_term_data$hourly_demand_trend_and_season_corrected==0,]

ggplot(short_term_data)+ geom_line(aes(1:nrow(short_term_data),hourly_demand_trend_and_season_corrected,color="actual"),alpha=0.5)+
  geom_line(aes(1:nrow(short_term_data),short_term_lm_model_predictions,color="fit"))



short_term_data2<-decomposed_data$shortterm[decomposed_data$shortterm$year!=2021,]

test_df = short_term_data[1:35016,]
res=test_df$hourly_demand_trend_and_season_corrected  - test_df$lm_model_fit
sum(abs(res))/sum(abs(test_df$hourly_demand_trend_and_season_corrected))

ggplot(short_term_data[20000:20100,])+ geom_line(aes(1:101,hourly_demand_trend_and_season_corrected,color="actual"))+
  geom_line(aes(1:101,lm_model_fit,color="fit"))



longterm_all_data$avg_hourly_demand*8760

### legacy ----
unique(historic_data$coverage_ratio[historic_data$year==2017])

testdf<-longterm_all_data
for (i in 1:14){
  
  o<-ggplot(testdf)+geom_line(aes(year,testdf[,i],color='t'),lwd=1.8)+ggtitle(colnames(testdf[i]))
  print(o)
  
}



all_countries<-unique(entsodata$country[4:nrow(entsodata)])
old_names<- c("CS","DK_W","UA_W","NI")
all_countries<-all_countries[-c("CS")]
all_countries %in% old_names
all_countries <- all_countries [! all_countries %in% old_names]

for (i in 1:length(all_countries)){
  country=countrycode(all_countries[i],"iso2c", "country.name")
  print(paste("currently processing:",country,"Country",i,"of",length(all_countries)))  
  historic_data <- orakle.get_historic_load_data(country)
  testdecompose <- orakle.decompose_load_data(historic_data)
   }


all_countries_dataset2 <-unique(entsodata2$CountryCode) 

all_countries[! all_countries %in% all_countries_dataset2] 
all_countries_dataset2[! all_countries_dataset2 %in% all_countries] 






######### function in fucntion testing ----

t_func1<- function(a){
  print(a)
  t_func2 <- function(b){
    print(b)
    return(b-1)
  }
  print(t_func2(a))
}

t_func1(7)








