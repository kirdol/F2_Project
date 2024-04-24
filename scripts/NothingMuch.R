library(readxl)
library(ggplot2) 
library(reshape2)
library(dplyr)
#import data
data <- read.csv("/Users/stefanfavre/Documents/GitHub/F2_Project2/data/data_export_20240424.csv", sep = ";")

## cleaning

#changing date format
data$Month <- as.Date(data$Month, format = "%d.%m.%y")

#some columns have "not available" instead of NAs, replacing them by 0s 
data$Solar_Energy_Production <- replace(data$Solar_Energy_Production, data$Solar_Energy_Production == "Not Available", 0)
data$Solar_Energy_Production %>% as.numeric(data$Solar_Energy_Production)
typeof(data$Solar_Energy_Production)

data$Solar_Energy_Production <- ifelse(data$Solar_Energy_Production == "Not Available", 0, data$Solar_Energy_Production)
data$Wind_Energy_Production <- ifelse(data$Wind_Energy_Production == "Not Available", 0, data$Wind_Energy_Production)

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

