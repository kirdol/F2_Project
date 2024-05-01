library(reshape2)
library(dplyr)
#import data
# data <- read.csv("/Users/stefanfavre/Documents/GitHub/F2_Project2/data/data_export_20240424.csv", sep = ";")
# 
# #changing date format
# data$Month <- as.Date(data$Month, format = "%d.%m.%y")
# 
# 
# #some columns have "not available" instead of NAs, replacing them by 0s 
# data$Solar_Energy_Production <- replace(data$Solar_Energy_Production, data$Solar_Energy_Production == "Not Available", 0)
# data$Solar_Energy_Production %>% as.numeric(data$Solar_Energy_Production)
# typeof(data$Solar_Energy_Production)
# 
# data$Solar_Energy_Production <- ifelse(data$Solar_Energy_Production == "Not Available", 0, data$Solar_Energy_Production)
# data$Wind_Energy_Production <- ifelse(data$Wind_Energy_Production == "Not Available", 0, data$Wind_Energy_Production)
# 
# data$Solar_Energy_Production <- as.numeric(data$Solar_Energy_Production)
# data$Wind_Energy_Production <- as.numeric(data$Wind_Energy_Production)

# run the setup.R script to load the packages
source(here::here("scripts", "setup.R"))

# Load the packages
data <- read.csv(here("data", "data_export_20240424.csv"), sep = ";")

# replace the NA with 0
data <- data %>%
  mutate_all(~ ifelse(. == "Not Available",0, .))

# Convert the 'Month' column to a date type
data$Month <- as.Date(data$Month, format = "%d.%m.%y")

# Check for non-numeric columns
non_numeric_cols <- sapply(data, function(x) !is.numeric(x))
names(data)[non_numeric_cols]

# Transform the non-numeric columns to numeric
data <- data %>%
  mutate_all(as.numeric)

# Convert the data frame to a tsibble
data_tsibble <- as_tsibble(data, index = Month)

# Check the class of the data
sapply(data, class)
# We have only "numeric" class

# Evolution through time variables 
ggplot(data, aes(x = Month, y = Coal_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Coal Production") +
  theme_minimal()

ggplot(data, aes(x = Month, y = Natural_Gas_Dry_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()

ggplot(data, aes(x = Month, y = Crude_Oil_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()

ggplot(data, aes(x = Month, y = Natural_Gas_Plant_Liquids_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()

ggplot(data, aes(x = Month, y = Total_Fossil_Fuels_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()

ggplot(data, aes(x = Month, y = Nuclear_Electric_Power_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()

ggplot(data, aes(x = Month, y = Hydroelectric_Power_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()

ggplot(data, aes(x = Month, y = Geothermal_Energy_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()

# data transformation Solar_Energy_Production
ggplot(data, aes(x = Month, y = Solar_Energy_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()

ggplot(data, aes(x = Month, y = Wind_Energy_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()

ggplot(data, aes(x = Month, y = Biomass_Energy_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()

ggplot(data, aes(x = Month, y = Total_Renewable_Energy_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()

#----------- FORECASTS ------------#


# --- coal production --- #

#plot 
ggplot(data, aes(x = Month, y = Coal_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Coal Production") +
  theme_minimal()


#creates tsibble 
coal_production_monthly.ts <- data %>% 
  mutate(Year_Month = yearmonth(Month)) %>%
  as_tsibble(index = Year_Month) %>%
  select(Year_Month, Coal_Production)


#check gaps 
coal_production_monthly.ts.gaps <- has_gaps(coal_production_monthly.ts) #no gaps


#check for seasonality, trend, residuals - decomposition
coal_production_monthly.ts %>%
  model(classical_decomposition(Coal_Production, type = "multiplicative")) %>%
  components() %>%
  autoplot() + xlab("Year")


# -- Mean + Naive + SNaive model -- #
coal_mean_naive_snaive.fit <- coal_production_monthly.ts %>% model(
  Mean = MEAN(Coal_Production),
  Naive = NAIVE(Coal_Production),
  S_Naive = SNAIVE(Coal_Production))
coal_mean_naive_snaive.fit %>% forecast(h = 15) %>% autoplot(coal_production_monthly.ts, level = NULL) +
  guides(colour=guide_legend(title = "Forecast"))


# -- ETS model -- #

#model when trying to tune hyperparameters
coal_ets_tuned <- coal_production_monthly.ts %>% 
  model(ETS_tuned = ETS(Coal_Production ~ error("A") + trend("A", alpha = 0.9, beta = 0.0645) + season("M"), opt_crit = "mse"))


#Forecast
coal_ets_tuned %>% forecast(h = 12) %>% autoplot(coal_production_monthly.ts)


#model when letting the function tune hyperparameters
coal_ets.fit <- coal_production_monthly.ts %>% 
  model(ETS = ETS(Coal_Production))


#Forecast
coal_ets.fit %>% forecast(h = 12) %>% autoplot(coal_production_monthly.ts)


#to check best alpha and beta
coefficients(coal_ets.fit)
coefficients(coal_ets.fit2)


##############################
# when using the ETS() function without explicitly specifying 
# how to model the trend, seasonality, and error type, the software will 
# generally employ an automatic model selection process based on some 
# criteria (like AIC or BIC) to choose the best fit among the available
# types (additive or multiplicative) for each component. 

# Source: ChatGPT
##############################

#same for the remaining time series


#general tsibble
data_tsibble <- data %>% 
  mutate(Year_Month = yearmonth(Month)) %>%
  as_tsibble(index = Year_Month) %>%
  select(-c(Month))



# Fit ETS models for all the type of production
ets_model_coal <- data_tsibble %>% model(ETS(data_tsibble$Coal_Production))


arima_model_natural_gas_dry <- data_tsibble %>% model(ETS = ETS(Natural_Gas_Dry_Production))


arima_model_crude_oil <- auto.arima(data_tsibble$Crude_Oil_Production)
arima_model_natural_gas_plant_liquids <- auto.arima(data_tsibble$Natural_Gas_Plant_Liquids_Production)
arima_model_total_fossil_fuels <- auto.arima(data_tsibble$Total_Fossil_Fuels_Production)
arima_model_nuclear_electric_power <- auto.arima(data_tsibble$Nuclear_Electric_Power_Production)
arima_model_hydroelectric_power <- auto.arima(data_tsibble$Hydroelectric_Power_Production)
arima_model_geothermal_energy <- auto.arima(data_tsibble$Geothermal_Energy_Production)
arima_model_solar_energy <- auto.arima(data_tsibble$Solar_Energy_Production)
arima_model_wind_energy <- auto.arima(data_tsibble$Wind_Energy_Production)
arima_model_biomass_energy <- auto.arima(data_tsibble$Biomass_Energy_Production)
arima_model_total_renewable_energy <- auto.arima(data_tsibble$Total_Renewable_Energy_Production)
arima_model_total_primary_energy <- auto.arima(data_tsibble$Total_Primary_Energy_Production)

# Forecast using the ETS model
forecast_result_natural_gas_dry <- forecast(arima_model_natural_gas_dry, h = 24)
forecast_result_crude_oil <- forecast(arima_model_crude_oil, h = 24)
forecast_result_natural_gas_plant_liquids <- forecast(arima_model_natural_gas_plant_liquids, h = 24)
forecast_result_total_fossil_fuels <- forecast(arima_model_total_fossil_fuels, h = 24)
forecast_result_nuclear_electric_power <- forecast(arima_model_nuclear_electric_power, h = 24)
forecast_result_hydroelectric_power <- forecast(arima_model_hydroelectric_power, h = 24)
forecast_result_geothermal_energy <- forecast(arima_model_geothermal_energy, h = 24)
forecast_result_solar_energy <- forecast(arima_model_solar_energy, h = 24)
forecast_result_wind_energy <- forecast(arima_model_wind_energy, h = 24)
forecast_result_biomass_energy <- forecast(arima_model_biomass_energy, h = 24)
forecast_result_total_renewable_energy <- forecast(arima_model_total_renewable_energy, h = 24)
forecast_result_total_primary_energy <- forecast(arima_model_total_primary_energy, h = 24)

# Plot the forecast
p_coal <- autoplot(forecast_result_coal) +
  labs(title = "ARIMA Forecast of Coal Production",
       y = "Coal Production",
       x = "Time")
p_natural_gas_dry <- autoplot(forecast_result_natural_gas_dry) +
  labs(title = "ARIMA Forecast of Natural Gas Dry Production",
       y = "Natural Gas Dry Production",
       x = "Time")
p_crude_oil <- autoplot(forecast_result_crude_oil) +
  labs(title = "ARIMA Forecast of Crude Oil Production",
       y = "Crude Oil Production",
       x = "Time")
p_natural_gas_plant_liquids <- autoplot(forecast_result_natural_gas_plant_liquids) +
  labs(title = "ARIMA Forecast of Natural Gas Plant Liquids Production",
       y = "Natural Gas Plant Liquids Production",
       x = "Time")
p_total_fossil_fuels <- autoplot(forecast_result_total_fossil_fuels) +
  labs(title = "ARIMA Forecast of Total Fossil Fuels Production",
       y = "Total Fossil Fuels Production",
       x = "Time")
p_nuclear_electric_power <- autoplot(forecast_result_nuclear_electric_power) +
  labs(title = "ARIMA Forecast of Nuclear Electric Power Production",
       y = "Nuclear Electric Power Production",
       x = "Time")
p_hydroelectric_power <- autoplot(forecast_result_hydroelectric_power) +
  labs(title = "ARIMA Forecast of Hydroelectric Power Production",
       y = "Hydroelectric Power Production",
       x = "Time")
p_geothermal_energy <- autoplot(forecast_result_geothermal_energy) +
  labs(title = "ARIMA Forecast of Geothermal Energy Production",
       y = "Geothermal Energy Production",
       x = "Time")
p_solar_energy <- autoplot(forecast_result_solar_energy) +
  labs(title = "ARIMA Forecast of Solar Energy Production",
       y = "Solar Energy Production",
       x = "Time")
p_wind_energy <- autoplot(forecast_result_wind_energy) +
  labs(title = "ARIMA Forecast of Wind Energy Production",
       y = "Wind Energy Production",
       x = "Time")
p_biomass_energy <- autoplot(forecast_result_biomass_energy) +
  labs(title = "ARIMA Forecast of Biomass Energy Production",
       y = "Biomass Energy Production",
       x = "Time")
p_total_renewable_energy <- autoplot(forecast_result_total_renewable_energy) +
  labs(title = "ARIMA Forecast of Total Renewable Energy Production",
       y = "Total Renewable Energy Production",
       x = "Time")
p_total_primary_energy <- autoplot(forecast_result_total_primary_energy) +
  labs(title = "ARIMA Forecast of Total Primary Energy Production",
       y = "Total Primary Energy Production",
       x = "Time")

final_plot <- p_coal + p_natural_gas_dry + p_crude_oil + p_natural_gas_plant_liquids +
  p_total_fossil_fuels + p_nuclear_electric_power + p_hydroelectric_power +
  p_geothermal_energy + p_solar_energy + p_wind_energy + p_biomass_energy +
  p_total_renewable_energy + p_total_primary_energy +
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")

# Affichage du résultat
print(final_plot)



