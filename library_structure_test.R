library(ggplot2)

# Setting working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# load orakly library
source('./library/orakle.R')

#### Testing ----

demand_data = orakle.get_entsoE_data(2017,2021,"Poland")
demand_data_filled = orakle.fill_missing_entsoE_data(demand_data)
decomposed_data = orakle.decompose_load_data(demand_data_filled)
midterm = orakle.add_holidays_mid_term(decomposed_data$midterm)
midterm_all = orakle.get_weather_data(midterm)
shortterm= orakle.add_holidays_short_term(decomposed_data$shortterm)

short_term_data <- orakle.shortterm_lm_model(shortterm)

longterm <- orakle.get_historic_load_data(decomposed_data$longterm)
longterm_all_data= orakle.get_macro_economic_data(longterm)
longterm_all_data_predicted = orakle.long_term_lm(longterm_all_data)

longterm$avg_hourly_demand[12]=mean(demand_data$load[demand_data$year==2021])

mean(demand_data$load[demand_data$year==2021])
longterm_all_data2<-longterm_all_data[-12,]
library(MLmetrics)
MAE(short_term_data$lm_model_fit[1:35016] , short_term_data$hourly_demand_trend_and_season_corrected[1:35016])
short_term_data[short_term_data$hourly_demand_trend_and_season_corrected==0,]

ggplot(short_term_data)+ geom_line(aes(1:nrow(short_term_data),hourly_demand_trend_and_season_corrected,color="actual"),alpha=0.5)+
  geom_line(aes(1:nrow(short_term_data),lm_model_fit,color="fit"))



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


