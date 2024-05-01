library(reshape2)
library(dplyr)
#import data
data <- read.csv("/Users/stefanfavre/Documents/GitHub/F2_Project2/data/data_export_20240424.csv", sep = ";")

#changing date format
data$Month <- as.Date(data$Month, format = "%d.%m.%y")


#some columns have "not available" instead of NAs, replacing them by 0s 
data$Solar_Energy_Production <- replace(data$Solar_Energy_Production, data$Solar_Energy_Production == "Not Available", 0)
data$Solar_Energy_Production %>% as.numeric(data$Solar_Energy_Production)
typeof(data$Solar_Energy_Production)

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

#model
coal_ets.fit <- coal_production_monthly.ts %>% 
  model(ETS = ETS(Coal_Production ~ error("A") + trend("A", alpha = 0.9, beta = 0.0645) + season("M"), opt_crit = "mse"))


#to check best alpha and beta
coefficients(coal_ets.fit)


#Forecast
coal_ets.fit %>% forecast(h = 12) %>% autoplot(coal_production_monthly.ts)


