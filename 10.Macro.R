# This code imports and analyzes the regression independent variables: macroeconomic metrics.
# It also offers a variety of descriptive plots highlighting their trends.

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Import from Excel
data_xlsx <- "EC_Prices.xlsx" 
EC_macro <- read_excel(data_xlsx, sheet = 3)
EC_macro$Date <- as.Date(EC_macro$Date)
print(EC_macro, n = 72)

# Descriptive Plots
# US - CH Inflation Comparison & Trend
ggplot(EC_macro, aes(x = Date, y = `Inflation (%)`, color = Company)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Trend of Inflations (US ; CH) over Time", x = "Time", y = "Monthly ∆%CPI") +
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# MSCI Banking Index Trend
ggplot(EC_macro, aes(x = Date, y = `MSCI ACWI Banks`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Trend of MSCI Banking Index over Time", x = "Time", y = "Monthly Return") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# USD/EUR - CHF/USD Exhange Rates Comparison & Trend
ggplot(EC_macro, aes(x = Company, y = `ForEx`, fill = Company)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "USD/EUR vs CHF/USD", x = "", y = "Daily Exchange Rate")

# 10Y Government Bonds issued by FED and SNB, Comparison & Trend
ggplot(EC_macro, aes(x = Date, y = `10Y (%)`, color = Company)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Trend of 10Y Gov. Bonds (US ; CH) over Time", x = "Time", y = "10Y Gov.Bonds Yields") +
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
