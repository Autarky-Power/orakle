
Sys.setlocale("LC_TIME", "English")



# get_historic_load_data ----  
orakle.get_historic_load_data <- function(longterm){
  
  
  country=unique(longterm$country)
  if (country=="UK"){country="GB"}
  suppressMessages(  
    entsodata <-readxl::read_excel("./Data/Monthly-hourly-load-values_2006-2015.xlsx")
  )
  colnames(entsodata)[1:5]<- c("country","year","month","day","coverage_ratio")
  
 
  data <- entsodata[entsodata$country== country,]
  
  y <- vector(mode = "list", length = 0)
  x_year <- vector(mode = "list", length = 0)
  year_list <- vector(mode = "list", length = 0)
  x_coverage <- vector(mode = "list", length = 0)
  coverage_list <- vector(mode = "list", length = 0)
  
  
  for (i in 1:nrow(data)){
    x<- t(data[i,6:29])
    y<- c(y,x)
    x_year[1:24]<- data[i,2]
    year_list <- c(year_list,x_year)
    x_coverage[1:24]<- data[i,5]
    coverage_list <- c(coverage_list,x_coverage)  
  }
  
  data1 <- as.data.frame(t(y))
  data1 <- as.data.frame(as.numeric(t(data1)))
  
  colnames(data1)<- "load"
  data1$year <- as.numeric(year_list)
  data1$coverage_ratio <- as.numeric(coverage_list)
  data1$load_scaled_with_coverage_ratio <- data1$load /(data1$coverage/100)
  
  year_list <- unique(data1$year)
  for (year in year_list){
    if (nrow(data1[data1$year==year,])<8000){
      data1 <- data1[!data1$year==year, ]
    }
  }
  year_list <- unique(data1$year)
  
  entsodata2 <-readxl::read_excel("./Data/MHLV_data-2015-2017.xlsx")
  
  data2 <- entsodata2[entsodata2$CountryCode== country,]
  data2$year <- lubridate::year(data2$DateUTC)
  
  year_list2 <- unique(data2$year)
  for (year in year_list2){
    
    if (nrow(data2[data2$year==year,])<8000){
      data2 <- data2[!data2$year==year, ]
    }
  }
  year_list2 <- unique(data2$year)
  
  if (2015 %in% year_list & 2015 %in% year_list2){
    year_list <- year_list[year_list != 2015]
  }
  
  yearly_load_df <- data.frame(c(year_list,year_list2))
  colnames(yearly_load_df) <- "year"
  yearly_load_df$load <-0
  yearly_load_df$load_scaled_with_coverage_ratio <-0
  
  
  for (year in year_list){
    yearly_load_df$load[yearly_load_df$year==year] <- mean(data1$load[data1$year==year],na.rm = TRUE)
    yearly_load_df$load_scaled_with_coverage_ratio[yearly_load_df$year==year] <- mean(data1$load_scaled_with_coverage_ratio[data1$year==year],na.rm = TRUE)
  }
  for (year in year_list2){
    yearly_load_df$load[yearly_load_df$year==year] <- mean(data2$Value[data2$year==year],na.rm = TRUE)
    yearly_load_df$load_scaled_with_coverage_ratio[yearly_load_df$year==year] <- mean(data2$Value_ScaleTo100[data2$year==year],na.rm = TRUE)
  }
  
  if (2017 %in% longterm$year){
    distance1 = sqrt((longterm$avg_hourly_demand[longterm$year==2017]-
                       yearly_load_df$load[yearly_load_df$year==2017])^2)
    distance2 =sqrt((longterm$avg_hourly_demand[longterm$year==2017]-
                       yearly_load_df$load_scaled_with_coverage_ratio[yearly_load_df$year==2017])^2)
    all_years <- c(unique(yearly_load_df$year[!yearly_load_df$year==2017]),unique(longterm$year))  
    
    if (distance1 <= distance2){
      all_demand <- c(yearly_load_df$load[!yearly_load_df$year==2017],longterm$avg_hourly_demand)  
    }else{all_demand <- c(yearly_load_df$load_scaled_with_coverage_ratio[!yearly_load_df$year==2017],longterm$avg_hourly_demand)}
  }else{
    all_years <- c(unique(yearly_load_df$year),unique(longterm$year))  
    all_demand <- c(yearly_load_df$load,longterm$avg_hourly_demand)
  }
  longterm_full <- data.frame(all_years,all_demand)
  colnames(longterm_full) <- c("year","avg_hourly_demand")
  longterm_full<- data.frame(country = country, longterm_full)
  return(longterm_full)
}
# decompose_load_data ----
orakle.decompose_load_data <- function(all_data){
     
  library(ggplot2)
  #library(patchwork)
  
  timepoint <- seq(as.POSIXct(paste0(as.character(min(unique(all_data$year))),'-01-01 00:00')),
                   as.POSIXct(paste0(as.character(max(unique(all_data$year))),'-12-31 23:00')),by="hour")
  hourly_data <- as.data.frame(timepoint) 
  colnames(hourly_data)<- "Date"
  hourly_data$year <- lubridate::year(hourly_data$Date)
  hourly_data$month <- lubridate::month(hourly_data$Date)
  hourly_data$day <- lubridate::day(hourly_data$Date)
  hourly_data$hour <- lubridate::hour(hourly_data$Date)
  hourly_data$wday <- lubridate::wday(hourly_data$Date,label = T,locale = "English")
  if (all_data$time_interval[1] == "15 mins"){
  hourly_data$load <- colMeans(matrix(all_data$load, nrow=4))
  } else if (all_data$time_interval[1] == "30 mins"){
    hourly_data$load <- colMeans(matrix(all_data$load, nrow=2))
  }else{
    hourly_data$load <- all_data$load
  }
  
  hourly_data$unit <- unique(all_data$unit)
  hourly_data$country <- unique(all_data$country)
  
  all_data <- hourly_data

  all_data <- all_data[! (all_data$month==2 & all_data$day==29),]

  length(unique(all_data$year))
  longterm <- data.frame(matrix(nrow=length(unique(all_data$year)),ncol=3))
  colnames(longterm)<- c("country","year","avg_hourly_demand")
  longterm$year <- unique(all_data$year)
  country=unique(all_data$country)
  longterm$country<- country
  for (i in (min(longterm$year):max(longterm$year))){
    
    longterm$avg_hourly_demand[longterm$year==i]<- mean(all_data$load[all_data$year==i],na.rm = T)
  }

  midterm <- data.frame(matrix(nrow=(length(unique(all_data$year))*365),ncol=7))
  colnames(midterm)<- c("country","date","year","month","day","wday","avg_hourly_demand")
  
  for (i in 1:length(unique(all_data$year))){
    midterm$year[((i-1)*365+1):(i*365)] <- unique(all_data$year)[i]  
  }
  
  for (i in 1:nrow(midterm)){
    midterm$date[i] <- all_data$Date[((i-1)*24+1)]
    midterm$month[i] <- all_data$month[((i-1)*24+1)]
    midterm$day[i] <- all_data$day[((i-1)*24+1)]
    midterm$wday[i] <- all_data$wday[((i-1)*24+1)]
    midterm$avg_hourly_demand[i] <- mean(all_data$load[((i-1)*24+1):(i*24)],na.rm = T)
  }
  midterm$date <- as.POSIXct(midterm$date, format="%Y-%m-%d",origin = "1970-01-01")
  midterm$date <-as.Date(midterm$date, format="%Y-%m-%d","CET")
  midterm$seasonal_avg_hourly_demand <-0
  for (i in min(all_data$year):max(all_data$year)){
    midterm$seasonal_avg_hourly_demand[midterm$year==i] <- midterm$avg_hourly_demand[midterm$year==i]-
      longterm$avg_hourly_demand[longterm$year==i]  
  }
  midterm$country<- country
  
  shortterm <- data.frame(matrix(nrow=(nrow(all_data)),ncol=1))
  colnames(shortterm) <- "country"
  
  shortterm$country <- country
  shortterm$date <- all_data$Date
  shortterm$year <- all_data$year
  shortterm$month <- all_data$month
  shortterm$day <- all_data$day
  shortterm$wday <- all_data$wday
  shortterm$hour <- all_data$hour
  shortterm$hourly_demand <- all_data$load
  shortterm$hourly_demand_trend_corrected <- 0
  
  for (i in min(all_data$year):max(all_data$year)){
    shortterm$hourly_demand_trend_corrected[shortterm$year==i] <- shortterm$hourly_demand[shortterm$year==i]-
      longterm$avg_hourly_demand[longterm$year==i]  
  }
  
  shortterm$hourly_demand_trend_and_season_corrected <- 0
  
  
  for (i in 1:(nrow(midterm)-1)){
    shortterm$hourly_demand_trend_and_season_corrected[((i-1)*24+1):(i*24)] <- 
      shortterm$hourly_demand_trend_corrected[((i-1)*24+1):(i*24)]- midterm$seasonal_avg_hourly_demand[i]   
  }
  
  
  trend_plot<- ggplot(longterm)+geom_line(aes(year,avg_hourly_demand, color="Average hourly demand"),linewidth=1.1)+
    theme(legend.title = element_blank()) +ggtitle('Long term trend \n')+
    theme(plot.title = element_text(hjust = 0.5))+ylab("MW")
  
  midterm_seasonality_plot <-  ggplot(midterm)+geom_line(aes(1:nrow(midterm),seasonal_avg_hourly_demand, color="Average hourly demand"),linewidth=1.1)+
    theme(legend.title = element_blank()) +ggtitle('Mid-term seasonality \n')+
    theme(plot.title = element_text(hjust = 0.5))+ylab("MW")+xlab("Day")
  
  shortterm_seasonality_plot <-  ggplot(shortterm)+geom_line(aes(1:nrow(shortterm),hourly_demand_trend_and_season_corrected, color="Average hourly demand"),linewidth=1.1)+
    theme(legend.title = element_blank()) +ggtitle('Short-term seasonality \n')+
    theme(plot.title = element_text(hjust = 0.5))+ylab("MW")+xlab("Hour")
  
  all_plots <- trend_plot / midterm_seasonality_plot / shortterm_seasonality_plot
  
  print(all_plots)
  
  return(list("longterm"=longterm, "midterm"=midterm, "shortterm"=shortterm))
}
# get_macro_economic_data ----
orakle.get_macro_economic_data <- function(longterm){
  
  country=unique(longterm$country)
  start_year= min(longterm$year)
  end_year= max(longterm$year)

  
  res_pop = httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/SP.POP.TOTL?date=",start_year,":",end_year,"&format=json"))
  
  data_pop=jsonlite::fromJSON(rawToChar(res_pop$content))
  df_pop<- as.data.frame(data_pop[2])
  df_pop<- df_pop[order(df_pop$date),]
  longterm$population<-df_pop$value
  
  res_gdp= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NY.GDP.MKTP.KD?date=",start_year,":",end_year,"&format=json"))
  
  data_gdp=jsonlite::fromJSON(rawToChar(res_gdp$content))
  df_gdp<- as.data.frame(data_gdp[2])
  df_gdp<- df_gdp[order(df_gdp$date),]
  longterm$gdp<- df_gdp$value
  
  res_ind= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NV.IND.TOTL.ZS?date=",start_year,":",end_year,"&format=json"))
  
  data_ind=jsonlite::fromJSON(rawToChar(res_ind$content))
  df_ind<- as.data.frame(data_ind[2])
  df_ind<- df_ind[order(df_ind$date),]
  longterm$industrial<- longterm$gdp*df_ind$value/100
  
  
  res_man= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NV.IND.MANF.ZS?date=",start_year,":",end_year,"&format=json"))
  
  data_man=jsonlite::fromJSON(rawToChar(res_man$content))
  df_man<- as.data.frame(data_man[2])
  df_man<- df_man[order(df_man$date),]
  longterm$manuf<- longterm$gdp*df_man$value/100
  
  
  res_gro= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NY.GDP.MKTP.KD.ZG?date=",start_year,":",end_year,"&format=json"))
  
  data_gro=jsonlite::fromJSON(rawToChar(res_gro$content))
  df_gro<- as.data.frame(data_gro[2])
  df_gro<- df_gro[order(df_gro$date),]
  longterm$growth<- df_gro$value
  
  
  
  res_gdp_defl= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NY.GDP.DEFL.KD.ZG?date=",start_year,":",end_year,"&format=json"))
  
  data_gdp_defl=jsonlite::fromJSON(rawToChar(res_gdp_defl$content))
  df_gdp_defl<- as.data.frame(data_gdp_defl[2])
  df_gdp_defl<- df_gdp_defl[order(df_gdp_defl$date),]
  longterm$gdp_defl<- df_gdp_defl$value
  
  
  res_serv= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NV.SRV.TOTL.ZS?date=",start_year,":",end_year,"&format=json"))
  
  data_serv=jsonlite::fromJSON(rawToChar(res_serv$content))
  df_serv<- as.data.frame(data_serv[2])
  df_serv<- df_serv[order(df_serv$date),]
  
  longterm$service<- longterm$gdp*   df_serv$value/100
  
  
  res_gni= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NY.GNP.MKTP.KD?date=",start_year,":",end_year,"&format=json"))
  
  data_gni=jsonlite::fromJSON(rawToChar(res_gni$content))
  df_gni<- as.data.frame(data_gni[2])
  df_gni<- df_gni[order(df_gni$date),]
  
  longterm$GNI <- df_gni$value
  
  
  res_hou= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/NE.CON.PRVT.ZS?date=",start_year,":",end_year,"&format=json"))
  
  data_hou=jsonlite::fromJSON(rawToChar(res_hou$content))
  df_hou<- as.data.frame(data_hou[2])
  df_hou<- df_hou[order(df_gro$date),]
  
  longterm$household_con <- longterm$gdp*df_hou$value/100
  
  
  res_rural= httr::GET(paste0("http://api.worldbank.org/v2/country/",country,"/indicator/SP.RUR.TOTL?date=",start_year,":",end_year,"&format=json"))
  
  data_rural=jsonlite::fromJSON(rawToChar(res_rural$content))
  df_rural<- as.data.frame(data_rural[2])
  df_rural<- df_rural[order(df_rural$date),]
  
  longterm$rural <- df_rural$value
  
  return(longterm)
}
# get_entsoE_data ----
orakle.get_entsoE_data <- function(start_year,end_year,country){
  
  # Convert country names to iso2c code ----
  if (country != "United Kingdom"){
  country=countrycode::countrycode(country, "country.name","iso2c")  
  }else{country="UK"}
  start=start_year
  end=end_year
  
  
  # Generate dataframe with country name, country code and the respective API Domain code ----
  
  domain_codes<-c("Albania","AL","10YAL-KESH-----5","Estonia","EE","10Y1001A1001A39I","Denmark","DK","10Y1001A1001A65H","Germany","DE","10Y1001A1001A83F",
                  "United Kingdom","UK","10Y1001A1001A92E",
                  "Malta","MT","10Y1001A1001A93C",
                  "Moldova","MD","10Y1001A1001A990",
                  "Armenia","AM","10Y1001A1001B004",
                  "Georgia","GE","10Y1001A1001B012",
                  "Azerbaidjan","AZ","10Y1001A1001B05V",
                  "Ukraine","UA","10Y1001C--00003F",
                  "Kosovo","XK","10Y1001C--00100H",
                  "Austria","AT","10YAT-APG------L",
                  "Bosnia and Herz.","BA","10YBA-JPCC-----D",
                  "Belgium","BE","10YBE----------2",
                  "Bulgaria","BG","10YCA-BULGARIA-R",
                  "Switzerland","CH","10YCH-SWISSGRIDZ",
                  "Montenegro","ME","10YCS-CG-TSO---S",
                  "Serbis","RS","10YCS-SERBIATSOV",
                  "Cyprus","CY","10YCY-1001A0003J",
                  "Czech Republic","CZ","10YCZ-CEPS-----N",
                  "Spain","ES","10YES-REE------0",
                  "Finland","FI","10YFI-1--------U",
                  "France","FR","10YFR-RTE------C",
                  "Greece","GR","10YGR-HTSO-----Y",
                  "Croatia","HR","10YHR-HEP------M",
                  "Hungary","HU","10YHU-MAVIR----U",
                  "Ireland","IE","10YIE-1001A00010",
                  "Lithuania","LT","10YLT-1001A0008Q",
                  "Luxembourg","LU","10YLU-CEGEDEL-NQ",
                  "Latvia","LV","10YLV-1001A00074",
                  "North Macedonia","MK","10YMK-MEPSO----8",
                  "Netherlands","NL","10YNL----------L",
                  "Norway","NO","10YNO-0--------C",
                  "Poland","PL","10YPL-AREA-----S",
                  "Portugal","PT","10YPT-REN------W",
                  "Romania","RO","10YRO-TEL------P",
                  "Sweden","SE","10YSE-1--------K",
                  "Slovenia","SI","10YSI-ELES-----O",
                  "Slovakia","SK","10YSK-SEPS-----K",
                  "Turkey","TR","10YTR-TEIAS----W")
  
  countrynames <- domain_codes[seq(1,length(domain_codes),3)]
  country_codes <-domain_codes[seq(2,length(domain_codes),3)]
  domains <- domain_codes[seq(3,length(domain_codes),3)]
  domain_df <- data.frame(countrynames,country_codes,domains)
  
  
  domain=domain_df$domains[domain_df$country_codes==country]
  
  
  # API call ----
  # Loop over every year 
  
  data_list <- list()
  for (i in start:end){
    starting_year=i
    print(paste("Getting data for",i))
    entso_response = httr::GET(paste0("https://web-api.tp.entsoe.eu/api?securityToken=5ca5937c-7eae-4302-b444-5042ab55d8ef&documentType=A65&processType=A16&outBiddingZone_Domain=",domain,"&periodStart=",starting_year,"01010000&periodEnd=",(starting_year+1),"01010000"))
    
    entso_content <- httr::content(entso_response, encoding = "UTF-8")
    entso_content_list <- xml2::as_list(entso_content)
    
    entso_timeseries <- entso_content_list$GL_MarketDocument[names(entso_content_list$GL_MarketDocument) == "TimeSeries"]
    
    # The response sends unpredictable numbers of time series, therefore we have to loop
    # over each one.
    
    ts_list <- list()
    for (j in 1:length(entso_timeseries)){
      
      ts<-entso_timeseries[j]
      load <- as.numeric(unlist(purrr::map(ts$TimeSeries$Period, "quantity")))
      start_date <- lubridate::ymd_hm(ts$TimeSeries$Period$timeInterval$start[[1]], tz = "UTC")
      end_date <- lubridate::ymd_hm(ts$TimeSeries$Period$timeInterval$end[[1]], tz = "UTC")
      time_resolution <- ts$TimeSeries$Period$resolution
      time_resolution_minutes <- paste(as.integer(substr(time_resolution, 3, 4)),"mins")
      
      ts_data <- as.data.frame(seq(start_date,end_date,by=time_resolution_minutes))
      colnames(ts_data) <- "Date" 
      ts_data <- ts_data[-length(ts_data$Date),,drop=F]
      ts_data$load <- load
      
      ts_list[[j]] <- ts_data
    }
    
    # Combine all Timeseries data.
    all_ts_data = do.call(what = rbind, args = ts_list)
    all_ts_data$unit <- entso_timeseries$TimeSeries$quantity_Measure_Unit.name[[1]]
    
    data_list[[(i-start+1)]] <- all_ts_data 
    
  }
  
  # Combine all years
  all_data = do.call(what = rbind, args = data_list)
  
  
  all_data$year <- lubridate::year(all_data$Date)
  all_data$time_interval <- time_resolution_minutes
  all_data$country <- country
  
  # check number of observations per year
  for (i in start:end){
    print(paste("year:",i,"number of datapoints:",nrow(all_data[all_data$year==i,])))
    
    
  }
  if (country=="UK"){
    all_data$country <- "GB"
  }
  return(all_data)
}
# fill_missing_entsoE_data ----
orakle.fill_missing_entsoE_data <- function(entsoe_data){
  
  timepoint <- seq(as.POSIXct(paste0(as.character(min(unique(entsoe_data$year))),'-01-01 00:00'),tz="UTC"),
                   as.POSIXct(paste0(as.character(max(unique(entsoe_data$year))),'-12-31 23:00'),tz="UTC"),by=unique(entsoe_data$time_interval),)
  
  complete_data <- as.data.frame(timepoint) 
  colnames(complete_data)<- "Date"
  
  complete_data$load <- 0
  complete_data$load[complete_data$Date %in% entsoe_data$Date] <- entsoe_data$load
  missing_data_index <- as.numeric(row.names(complete_data[which(complete_data$load==0),]))
  interval_minutes <- as.integer(substr(unique(entsoe_data$time_interval), 1, 2))
  interval_one_week_ago <- 60/interval_minutes*24*7
  complete_data$load[missing_data_index]<- complete_data$load[missing_data_index - interval_one_week_ago]
  complete_data$unit <- unique(entsoe_data$unit)
  complete_data$year <- lubridate::year(complete_data$Date)
  complete_data$time_interval <- unique(entsoe_data$time_interval)
  complete_data$country <- unique(entsoe_data$country)
  country <- unique(entsoe_data$country)
  if (! file.exists(country)){ 
    dir.create(country)} 
  if (! file.exists(paste0("./",country,"/data"))){ 
    dir.create(paste0("./",country,"/data"))}  
  
  write.csv(complete_data,paste0("./",country,"/data/entsoE_load_data.csv"),row.names = F)
  
  
  return (complete_data)
}
# shortterm_lm_model ----
orakle.shortterm_lm_model <- function(short_term_data, training_set_ratio=0.2){

  
  columns_original_df <- ncol(short_term_data)
  short_term_data[,c((columns_original_df+1):(columns_original_df+24))] <- 0
  for (i in (0:23)){
    colnames(short_term_data)[(columns_original_df+1+i)] <- paste0("Hour",i) 
    short_term_data[short_term_data$hour==i,(columns_original_df+1+i)] <- 1
  }
  
  country=unique(short_term_data$country)
  wday <- as.character(unique(short_term_data$wday))
  # preparation of matrix, where model is stored
  model_st <- data.frame(matrix(ncol = 14, nrow = 168))
  colnames(model_st) <- c("wday","hour","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  model_st[,1] <- c(rep(wday[1],24),rep(wday[2],24),rep(wday[3],24),rep(wday[4],24),rep(wday[5],24),rep(wday[6],24),rep(wday[7],24))
  model_st[,2] <- 1:24
  
  
  # data frame for adding all model fits together:
  model.st <- data.frame(matrix(ncol = 4, nrow = 2016)) # 2016 = 24 hours*7 days*12 month
  colnames(model.st) <- c("month","wday","hour","modelfit")
  model.st[,1] <- c(rep(1,168), rep(2,168),rep(3,168),rep(4,168),rep(5,168),rep(6,168),
                    rep(7,168),rep(8,168),rep(9,168),rep(10,168),rep(11,168),rep(12,168))
  
  model.st[,2] <- c(rep(wday[1],24),rep(wday[2],24),
                    rep(wday[3],24),rep(wday[4],24),
                    rep(wday[5],24),rep(wday[6],24),
                    rep(wday[7],24))
  model.st[,3] <- 1:24
  
  # define training and test set
  training_set=nrow(short_term_data)- round(nrow(short_term_data)*training_set_ratio)
  training_data=short_term_data[1:training_set,]
  test_data=short_term_data[(training_set+1):nrow(short_term_data),]
  
  # compute models and store results
  
  
  if (! file.exists(paste0("./",country,"./models/"))){ 
    dir.create(paste0("./",country,"./models/"))}
  if (! file.exists(paste0("./",country,"./models/shortterm_lm"))){ 
    dir.create(paste0("./",country,"./models/shortterm_lm"))}
  
  k = 1
  l = 1
  for (i in 1:12){
    for (j in 1:7){
      cat(paste("Processing model:",7*(i-1)+j,"of",12*7))  
      
      x <- training_data[which(training_data$month == i & training_data$wday == wday[j]),]
      xreg <- as.matrix(x[,c((columns_original_df):(columns_original_df+24))])
      
      fit1 <- lm(hourly_demand_trend_and_season_corrected ~ xreg[,1:24], data=x)
      
      name=paste0("month",i,wday[j])
      modelname=paste0("./",country,"/models/shortterm_lm/",name,".Rdata")
      save(fit1,file=modelname)
      fv <- data.frame(fit1$fitted.values)
      model_st[k:(k+23),(i+2)] <- fv[1:24,]
      model.st[l:(l+23),4] <- fv[1:24,]
      k = k+24
      l = l+24
      if (7*(i-1)+j == 12*7) cat('\n Done')
      else cat('\014')
    }
    
    k = 1
  }
  

  

  
  ## combine the results
  short_term_data$short_term_lm_model_predictions <- vector(length=nrow(short_term_data))
  
  suppressWarnings(
  for (i in 1:12){
    for(j in 1:7){
      short_term_data$short_term_lm_model_predictions[which(short_term_data$month == i & short_term_data$wday == wday[j])] <- 
        model.st[which(model.st$month == i & model.st$wday == wday[j]),4]
    }
  })
  
  if (! file.exists(paste0("./",country,"./data/"))){ 
    dir.create(paste0("./",country,"./data/"))}
  write.csv(short_term_data,paste0("./",country,"./data/short_term_data.csv"),row.names = F)
  return (short_term_data)
}
# add holidays midterm ----
orakle.add_holidays_mid_term<- function(midterm){
  
  years=unique(midterm$year)
  country= (unique(midterm$country))
  
  holiday_list <- list()
  for (i in 1:length(years)){
    year= years[i]
    response = jsonlite::fromJSON(paste0("https://date.nager.at/api/v3/publicholidays/"
                                         ,year,"/",country) )
    holiday_list[[i]] <- response$date
  }
  
  holidays = unlist(holiday_list)
  holidays = as.Date(holidays)
  
  midterm$holiday <- 0
  midterm$holiday[midterm$date %in% holidays] <- 1
  
  
  return(midterm)
}
# add holidays shortterm ----
orakle.add_holidays_short_term<- function(shortterm){
  
  years=unique(shortterm$year)
  country= (unique(shortterm$country))
  
  holiday_list <- list()
  for (i in 1:length(years)){
    year= years[i]
    response = jsonlite::fromJSON(paste0("https://date.nager.at/api/v3/publicholidays/"
                                         ,year,"/",country) )
    holiday_list[[i]] <- response$date
  }
  
  holidays = unlist(holiday_list)
  holidays = as.Date(holidays)
  
  shortterm$place_holder <- as.Date(shortterm$date,tz="CET")
  shortterm$holiday <- 0
  shortterm$holiday[shortterm$place_holder %in% holidays] <- 1
  shortterm <- subset(shortterm, select = -c(place_holder))                  
  
  return(shortterm)
}
# add weather data midterm ----
orakle.get_weather_data <- function(midterm){
  
  country=unique(midterm$country)
  start_year=min(unique(midterm$year))
  end_year=max(unique(midterm$year))
  
  cities1<- httr::GET(paste0("https://wft-geo-db.p.rapidapi.com/v1/geo/cities?countryIds=",country,"&sort=-population&offset=0&limit=10&types=CITY"),
                      httr::accept_json(),
                      httr::add_headers("x-rapidapi-host" = "wft-geo-db.p.rapidapi.com",
                                        "x-rapidapi-key" = "3a789ffa80msha4a27ded5ad6bf6p17156ajsn65dde6edcd82"))
  Sys.sleep(2)
  cities2<- httr::GET(paste0("https://wft-geo-db.p.rapidapi.com/v1/geo/cities?countryIds=",country,"&sort=-population&offset=10&limit=10&types=CITY"),
                      httr::accept_json(),
                      httr::add_headers("x-rapidapi-host" = "wft-geo-db.p.rapidapi.com",
                                        "x-rapidapi-key" = "3a789ffa80msha4a27ded5ad6bf6p17156ajsn65dde6edcd82"))
  
  
  big_cities <- rbind(jsonlite::fromJSON(rawToChar(cities1$content))$data,jsonlite::fromJSON(rawToChar(cities2$content))$data)
  big_cities$weather_station <- 0
  
  
  
  for (i in 1:nrow(big_cities)){
    lon=round(big_cities$longitude[i],digits=4 )
    lat=round(big_cities$latitude[i],digits=4)
    stations <- httr::GET(paste0("https://meteostat.p.rapidapi.com/stations/nearby?lat=",lat,"&lon=",lon), 
                          httr::accept_json(), 
                          httr::add_headers("x-rapidapi-host" = "meteostat.p.rapidapi.com",
                                            "x-rapidapi-key" = "3a789ffa80msha4a27ded5ad6bf6p17156ajsn65dde6edcd82"))
    
    stations_list <- jsonlite::fromJSON(rawToChar(stations$content))$data
    big_cities$weather_station[i] <- stations_list$id[1]
    Sys.sleep(0.5)
  }
  
  ts_date <- seq(as.Date(paste0(start_year,"-01-01")),as.Date(paste0(end_year,"-12-31")),by="d")
  temp_df <- data.frame(matrix(nrow=length(ts_date),ncol=(1+nrow(big_cities))))
  colnames(temp_df)[1] <- "date"
  temp_df$date <- ts_date
  temp_df$weighted_mean_temperature <- 0
  for (i in 1:nrow(big_cities)){
    tryCatch({
      station_id <- big_cities$weather_station[i]
      download.file(paste0("https://bulk.meteostat.net/v2/daily/",station_id,".csv.gz"),
                    destfile = "temp.csv.gz")
      
      R.utils::gunzip("temp.csv.gz")
      temp_data <- read.csv("temp.csv")
      colnames(temp_data)[c(1,2)] <- c("date","daily_avg_temp")
      temp_data$date <- as.Date(temp_data$date, format="%Y-%m-%d")
      temp_data <- temp_data[(lubridate::year(temp_data$date)>=start_year)&
                               (lubridate::year(temp_data$date)<=end_year),1:2]
      temp_df[,(i+1)][temp_df$date %in% temp_data$date]<- temp_data[,2]
      file.remove("temp.csv")
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  population <- big_cities$population
  
  for (i in 1:nrow(temp_df)){
    weighted_vector <- population[!is.na(temp_df[i,2:(ncol(temp_df)-1)])]/sum(population[!is.na(temp_df[i,2:(ncol(temp_df)-1)])])
    temp_df$weighted_mean_temperature[i]<- sum(temp_df[i,2:(ncol(temp_df)-1)][!is.na(temp_df[i,2:(ncol(temp_df)-1)])]*weighted_vector)
  }
  
  colnames(temp_df)[2:(ncol(temp_df)-1)] <- big_cities$name
  midterm$weighted_temperature <- 0
  
  midterm$weighted_temperature<- temp_df$weighted_mean_temperature[temp_df$date %in% midterm$date]
  
  if (! file.exists(country)){ 
    dir.create(country)} 
  if (! file.exists(paste0("./",country,"/data"))){ 
    dir.create(paste0("./",country,"/data"))}  
  write.csv(temp_df,paste0("./",country,"/data/temperatures.csv"),row.names = F)
  write.csv(midterm,paste0("./",country,"/data/midterm_data.csv"),row.names = F)
  
  return(list("midterm"=midterm, "temperature_data"=temp_df))
  
}
# Long Term Model ----
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
  
  
  message(paste("Cross-validating the best",testquant,"models."))
  cross_val <- function(i){
    tryCatch({
      predictor_names=combinations[i,2:(ncol(combinations)-5)] 
      model_variables=colnames(predictor_names)[complete.cases(t(predictor_names))] 
      lm_formula <- as.formula(paste("avg_hourly_demand", paste(model_variables, collapse = " + "), 
                                     sep = " ~ "))
      
      set.seed(rdm) 
      Kfold <- caret::train(lm_formula, data = training_data, 
                            trControl = ctrl, method = "lm")
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    return(c(Kfold$results$RMSE,Kfold$results$Rsquared,Kfold$results$MAE,i))
  }
  # Parallel Processing
  no_cores <- parallel::detectCores(logical = TRUE)
  
  used_cores <- floor(no_cores*0.75)
  if (used_cores > 10) {used_cores=10}

  cl <- parallel::makeCluster(used_cores)  
  doParallel::registerDoParallel(cl) 
  
  parallel::clusterExport(cl,list("combinations","ctrl","training_data","rdm"),envir=environment())
  
  results_list <-  parallel::parLapply(cl,1:testquant,cross_val)
  
  doParallel::stopImplicitCluster()
  parallel::stopCluster(cl)

  
  results <-as.data.frame(do.call(rbind, results_list))
  colnames(results)= c("RMSE_k_fold","Rsquare_k_fold","MAE_k_fold","index")
  
  
  best_index_RMSE = results$index[results["RMSE_k_fold"] == min(results["RMSE_k_fold"],na.rm = T)]
  best_value_RMSE = results[best_index_RMSE, "RMSE_k_fold"]
  limit_RMSE = best_value_RMSE * 1.5 
  message(paste("\n\nLowest RMSE of",round(best_value_RMSE),"for model no.",best_index_RMSE,". RMSE limit is set to"
                ,round(limit_RMSE)))
  
  best_index_MAE = results$index[results["MAE_k_fold"] == min(results["MAE_k_fold"],na.rm = T)]
  best_value_MAE = results[best_index_MAE, "MAE_k_fold"]
  limit_MAE = best_value_MAE * 1.5 
  message(paste("Lowest MAE of",round(best_value_MAE),"for model no.",best_index_MAE,". MAE limit is set to"
                ,round(limit_MAE)))
  
  best_index_Rsquare = results$index[results["Rsquare_k_fold"] == max(results["Rsquare_k_fold"],na.rm = T)]
  
  best_value_Rsquare = results[best_index_Rsquare, "Rsquare_k_fold"]
  limit_Rsquare = best_value_Rsquare / 1.3 
  message(paste("Highest Rsquare of",round(best_value_Rsquare,3),"for model no.",best_index_Rsquare,". Rsquare limit is set to"
                ,round(limit_Rsquare,3)))
  
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
  country<- unique(longterm_all_data$country)
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
  if (! file.exists(paste0("./",country,"/data"))){ 
    dir.create(paste0("./",country,"/data"))}  
  if (! file.exists(paste0("./",country,"/plots"))){ 
    dir.create(paste0("./",country,"/plots"))}
  if (! file.exists(paste0("./",country,"/models/longterm"))){ 
    dir.create(paste0("./",country,"/models/longterm"))}  
  save(best_lm_model,file=paste0("./",country,"/models/longterm/best_lm_model.Rdata"))
  
  ggsave(file=paste0("./",country,"/plots/Long_term_results.png"), plot=lt_plot2, width=12, height=8)
  longterm_all_data$longterm_model_predictions <- results
  write.csv(longterm_all_data,paste0("./",country,"/data/long_term_all_data.csv"),row.names = F)
  return(longterm_all_data)
}
# Mid Term Model ----
orakle.mid_term_lm <- function(midterm_all_data, training_set_ratio=0.2){
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
  
  midterm_all_data$end_of_year <- 0
  midterm_all_data$end_of_year[midterm_all_data$month==12 & midterm_all_data$day>22] <-1
  
  training_set=nrow(midterm_all_data)- round(nrow(midterm_all_data)*training_set_ratio)
  training_data=midterm_all_data[1:training_set,]
  test_data=midterm_all_data[(training_set+1):nrow(midterm_all_data),]
  
  
  variables <- colnames(midterm_all_data)[c(9:(ncol(midterm_all_data)))]
  
  
  f <- as.formula(  paste("seasonal_avg_hourly_demand",paste(variables, collapse = " + "), 
                          sep = " ~ "))
  
  globalmodel <- lm(f , data=training_data, na.action = "na.omit")
  
  
  y <- training_data$seasonal_avg_hourly_demand
  y_all <- midterm_all_data$seasonal_avg_hourly_demand
  y_test <- test_data$seasonal_avg_hourly_demand
  x <- data.matrix(training_data[, 9:ncol(midterm_all_data)])
  x_all <- data.matrix(midterm_all_data[, 9:ncol(midterm_all_data)])
  x_test<- data.matrix(test_data[, 9:ncol(training_data)])
  
  cv_model <- glmnet::cv.glmnet(x, y, alpha = 1)
  
  best_lambda <- cv_model$lambda.min
  best_model <- glmnet::glmnet(x, y, alpha = 1, lambda = best_lambda)
  
  
  
  testlasso<-predict(best_model, s = best_lambda, newx = x_test)
  testlm<-predict(globalmodel, newdata=test_data)
  
  country = unique(midterm_all_data$country)
  if (! file.exists(country)){ 
    dir.create(country)} 
  if (! file.exists(paste0("./",country,"/models"))){ 
    dir.create(paste0("./",country,"/models"))}  
  if (! file.exists(paste0("./",country,"/data"))){ 
    dir.create(paste0("./",country,"/data"))}  
  if (! file.exists(paste0("./",country,"/plots"))){ 
    dir.create(paste0("./",country,"/plots"))}
  if (! file.exists(paste0("./",country,"/models/midterm"))){ 
    dir.create(paste0("./",country,"/models/midterm"))} 
  
  if(MLmetrics::RMSE(testlasso,y_test) < MLmetrics::RMSE(testlm,y_test)){
    midterm_all_data$midterm_model_fit <- predict(best_model, s = best_lambda, newx = x_all)
    save(best_model,file=paste0("./",country,"/models/midterm/best_model.Rdata"))
  }else{
    midterm_all_data$midterm_model_fit <- predict(globalmodel, newdata = test_data)
    save(globalmodel,file=paste0("./",country,"/models/midterm/best_model.Rdata"))
  }
  
  years <- unique(midterm_all_data$year)
  index <- 1:length(years)
  for (i in 1:length(years)){
    index[i] <- min(as.numeric(rownames(midterm_all_data[midterm_all_data$year==years[i],])))
  }
  
  
  mt_plot <- ggplot(midterm_all_data)+geom_line(aes(1:nrow(midterm_all_data),seasonal_avg_hourly_demand,color="actual"))+
    geom_line(aes(1:nrow(midterm_all_data),midterm_model_fit,color="fitted"))+xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
    geom_vline(xintercept=training_set,linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nDay")+ylab("Avg Hourly Demand\n [MW]\n")+
    ggtitle(paste("Mid Term Model Results -",country,"\n"))+
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
    scale_x_continuous(breaks = index,labels = years)
  
  
  mt_plot2 <- ggplot(midterm_all_data)+geom_line(aes(1:nrow(midterm_all_data),seasonal_avg_hourly_demand,color="actual"))+
    geom_line(aes(1:nrow(midterm_all_data),midterm_model_fit,color="fitted"))+xlab("\nYear")+ylab("Avg Hourly Demand\n [MW]\n")+
    geom_vline(xintercept=training_set,linetype=2)+
    ggthemes::theme_foundation(base_size=14, base_family="sans")+
    xlab("\nDay")+ylab("Avg Hourly Demand\n [MW]\n")+
    ggtitle(paste("Mid Term Model Results -",country,"\n"))+
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
    theme(plot.title = element_text(size=26))+
    scale_x_continuous(breaks = index,labels = years)
  
  
  
  ggsave(file=paste0("./",country,"/plots/Mid_term_results.png"), plot=mt_plot2, width=12, height=8)
  
  print(mt_plot)
  
  return(midterm_all_data)
}
