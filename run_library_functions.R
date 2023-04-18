library(ggplot2)

# Setting working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# load orakle library
source('./library/orakle.R')

#### Testing ----

# Get and prepare intial Data
demand_data = orakle.get_entsoE_data(2017,2021,"Ireland")
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
short_term_data_predicted <- orakle.shortterm_lm_model(shortterm)
# Combine all models
combined_model_results <- orakle.combine_models(longterm_all_data_predicted,midterm_all_data_predicted,short_term_data_predicted)








