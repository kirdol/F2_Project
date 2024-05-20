#import data

data <- read.csv("/Users/stefanfavre/Documents/GitHub/F2_Project2/data/data_export_20240424.csv", sep = ";")

#changing date format
data$Month <- as.Date(data$Month, format = "%d.%m.%y")


#some columns have "not available" instead of NAs, replacing them by 0s
data$Solar_Energy_Production <- replace(data$Solar_Energy_Production, data$Solar_Energy_Production == "Not Available", 0)
data$Solar_Energy_Production %>% as.numeric(data$Solar_Energy_Production)

data$Solar_Energy_Production <- ifelse(data$Solar_Energy_Production == "Not Available", 0, data$Solar_Energy_Production)
data$Wind_Energy_Production <- ifelse(data$Wind_Energy_Production == "Not Available", 0, data$Wind_Energy_Production)

data$Solar_Energy_Production <- as.numeric(data$Solar_Energy_Production)
data$Wind_Energy_Production <- as.numeric(data$Wind_Energy_Production)


# Evolution through time variables 
ggplot(data, aes(x = Month, y = Coal_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Coal Production Over Time",
       x = "Month",
       y = "Coal Production") +
  theme_minimal()
ggplot(data, aes(x = Month, y = Natural_Gas_Dry_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Natural_Gas_Dry_Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()
ggplot(data, aes(x = Month, y = Crude_Oil_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Crude_Oil_Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()
ggplot(data, aes(x = Month, y = Natural_Gas_Plant_Liquids_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Natural_Gas_Plant_Liquids_Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()
ggplot(data, aes(x = Month, y = Total_Fossil_Fuels_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Total_Fossil_Fuels_Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()
ggplot(data, aes(x = Month, y = Nuclear_Electric_Power_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Nuclear_Electric_Power_Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()
ggplot(data, aes(x = Month, y = Hydroelectric_Power_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Hydroelectric_Power_Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()
ggplot(data, aes(x = Month, y = Geothermal_Energy_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Geothermal_Energy_Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()
ggplot(data, aes(x = Month, y = Solar_Energy_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Solar_Energy_Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()
ggplot(data, aes(x = Month, y = Wind_Energy_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Wind_Energy_Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()
ggplot(data, aes(x = Month, y = Biomass_Energy_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Biomass_Energy_Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()
ggplot(data, aes(x = Month, y = Total_Renewable_Energy_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Total_Renewable_Energy_Production Over Time",
       x = "Month",
       y = "Units") +
  theme_minimal()


#----------- FORECASTS ------------#

# --- coal production example --- #

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


# -- same for the remaining time series -- ##

#general tsibble
data_tsibble <- data %>% 
  mutate(Year_Month = yearmonth(Month)) %>%
  as_tsibble(index = Year_Month) %>%
  select(-c(Month)) %>%
  select(Year_Month, everything())


# Fit ETS models for all the type of production


#coal_production 
ets_model_coal <- data_tsibble %>% model(ETS = ETS(Coal_Production))
forecast_result_coal <- ets_model_coal %>% forecast(h = 24) %>% autoplot(data_tsibble)

#natural gas dry production 
ets_model_natural_gas_dry <- data_tsibble %>% model(ETS = ETS(Natural_Gas_Dry_Production))
forecast_result_natural_gas_dry <- ets_model_natural_gas_dry %>% forecast(h = 24) %>% autoplot(data_tsibble)

#crude oil production 
ets_model_crude_oil <- data_tsibble %>% model(ETS = ETS(Crude_Oil_Production))
ets_model_crude_oil <- ets_model_crude_oil %>% forecast(h = 24) %>% autoplot(data_tsibble)

#natural gas plant liquids production
##here as the regime has totally changed from around 2009, it is probably better to filter the data##
data_NGPL_filtered <- data_tsibble %>%
    select(Year_Month, Natural_Gas_Plant_Liquids_Production) %>%
    filter(year(Year_Month) >= 2009)
    
ets_model_natural_gas_plant_liquids <- data_NGPL_filtered %>% model(ETS = ETS(Natural_Gas_Plant_Liquids_Production))
ets_model_natural_gas_plant_liquids <- ets_model_natural_gas_plant_liquids %>% forecast(h = 24) %>% autoplot(data_NGPL_filtered)

#comparison 
# ets_model_natural_gas_plant_liquids <- data_tsibble %>% model(ETS = ETS(Natural_Gas_Plant_Liquids_Production))
# ets_model_natural_gas_plant_liquids %>% forecast(h = 24) %>% autoplot(data_tsibble)

#Total_Fossil_Fuels_Production
ets_model_total_fossil_fuels <- data_tsibble %>% model(ETS = ETS(Total_Fossil_Fuels_Production))
ets_model_total_fossil_fuels <- ets_model_total_fossil_fuels %>% forecast(h = 24) %>% autoplot(data_tsibble)

#Nuclear_Electric_Power_Production
ets_model_nuclear_electric_power <- data_tsibble %>% model(ETS = ETS(Nuclear_Electric_Power_Production))
ets_model_nuclear_electric_power <- ets_model_nuclear_electric_power %>% forecast(h = 24) %>% autoplot(data_tsibble)

#Hydroelectric_Power_Production
ets_model_hydroelectric_power <- data_tsibble %>% model(ETS = ETS(Hydroelectric_Power_Production))
ets_model_hydroelectric_power <- ets_model_hydroelectric_power %>% forecast(h = 24) %>% autoplot(data_tsibble)

#Geothermal_Energy_Production
ets_model_geothermal_energy <- data_tsibble %>% model(ETS = ETS(Geothermal_Energy_Production))
ets_model_geothermal_energy <- ets_model_geothermal_energy %>% forecast(h = 24) %>% autoplot(data_tsibble)

#Solar_Energy_Production
##here as the regime has totally changed from around 2011, it is probably better to filter the data##
data_SE_filtered <- data_tsibble %>%
  select(Year_Month, Solar_Energy_Production) %>%
  filter(year(Year_Month) >= 2011)

ggplot(data_SE_filtered, aes(x = Year_Month, y = Solar_Energy_Production)) +   geom_line() +  # Use geom_line() for a time series plot
  labs(title = "Solar_Energy_Production Over Time Filtered",
       x = "Month",
       y = "Units") +
  theme_minimal()


ets_model_solar_energy <- data_SE_filtered %>% model(ETS = ETS(Solar_Energy_Production))
ets_model_solar_energy <- ets_model_solar_energy %>% forecast(h = 24) %>% autoplot(data_SE_filtered)

#comparison
# ets_model_solar_energy <- data_tsibble %>% model(ETS = ETS(Solar_Energy_Production))
# ets_model_solar_energy %>% forecast(h = 24) %>% autoplot(data_tsibble)

#Wind_Energy_Production
##here as the regime has totally changed from around 2003, it is probably better to filter the data##
data_WE_filtered <- data_tsibble %>%
  select(Year_Month, Wind_Energy_Production) %>%
  filter(year(Year_Month) >= 2003)

ets_model_wind_energy <- data_WE_filtered %>% model(ETS = ETS(Wind_Energy_Production))
ets_model_wind_energy <- ets_model_wind_energy %>% forecast(h = 24) %>% autoplot(data_WE_filtered)

#comparison
# ets_model_wind_energy <- data_tsibble %>% model(ETS = ETS(Wind_Energy_Production))
# ets_model_wind_energy %>% forecast(h = 24) %>% autoplot(data_tsibble)

#Biomass_Energy_Production
ets_model_biomass_energy <- data_tsibble %>% model(ETS = ETS(Biomass_Energy_Production))
ets_model_biomass_energy <- ets_model_biomass_energy %>% forecast(h = 24) %>% autoplot(data_tsibble)

#Total_Renewable_Energy_Production
ets_model_total_renewable_energy <- data_tsibble %>% model(ETS = ETS(Total_Renewable_Energy_Production))
ets_model_total_renewable_energy <- ets_model_total_renewable_energy %>% forecast(h = 24) %>% autoplot(data_tsibble)

#Total_Primary_Energy_Production
ets_model_total_primary_energy <- data_tsibble %>% model(ETS = ETS(Total_Primary_Energy_Production))
ets_model_total_primary_energy <- ets_model_total_primary_energy %>% forecast(h = 24) %>% autoplot(data_tsibble)

#plot
final_plot <- forecast_result_coal + forecast_result_natural_gas_dry + ets_model_crude_oil +
  ets_model_natural_gas_plant_liquids + ets_model_total_fossil_fuels + ets_model_nuclear_electric_power +
  ets_model_hydroelectric_power + ets_model_geothermal_energy + ets_model_solar_energy +
  ets_model_wind_energy + ets_model_biomass_energy + ets_model_total_renewable_energy +
  ets_model_total_primary_energy +
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")

 print(final_plot)



