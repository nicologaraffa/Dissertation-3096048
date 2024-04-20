# This code imports and analyzes the regression dependent variables: price deltas.
# It also defines the three dimensions of price variation. Such extraction has been done on a separate Excel file.

library(tidyquant)
library(quantmod)
library(data.table)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(zoo)


# Import from Excel
data_xlsx <- "EC_Prices.xlsx"
EC_prices <- read_excel(data_xlsx, sheet = 1)
EC_prices <- EC_prices %>%
  select(-`Company Ticker`, -`Preceding Workday`, -`Following Workday`)
EC_prices$Date <- as.Date(EC_prices$Date)
print(EC_prices, n = 72)

# Calculate three dimensions of price deltas and create the EC_pdelta dataframe
EC_pdelta <- EC_prices %>%
  mutate(
    Delta_Day = ((Closing - Opening) / Opening) * 100,
    Delta_HL = ((High - Low) / Low) * 100,
    Delta_OP_CP = ((Opening_Post - Closing_Pre) / Closing_Pre) * 100
  ) %>%
  select(Doc_ID, Company, Quarter, Year, 
         Delta_Day,
         Delta_HL, 
         Delta_OP_CP,
         Date, Weekday, Volume)
print(EC_pdelta, n = 72)

# Descriptive plots

# Stock Trend from Q1 2015 to Q4 2023 in relative adjusted scales
EC_prices_graph <- EC_prices
EC_prices_graph <- EC_prices_graph %>%
  group_by(Company) %>%
  mutate(Normalized_Closing = Closing / first(Closing))
ggplot(EC_prices_graph, aes(x = Date, y = Normalized_Closing, color = Company)) +
  geom_line() +
  labs(title = "Normalized Stock Price Trend Over Time by Company",
       x = "Date",
       y = "Normalized Stock Price") +
  theme_minimal()

# Closing Price and Volume
ggplot(EC_prices_graph, aes(x = Volume, y = Closing, color = Company)) +
  geom_point() +
  labs(title = "Correlation Between Closing Price and Trading Volume",
       x = "Trading Volume",
       y = "Closing Price") +
  theme_minimal()

# Stock Price and Volume Relationships
unique_companies <- unique(EC_prices_graph$Company)
for(company in unique_companies) {
  company_data <- filter(EC_prices_graph, Company == company)
  gg <- ggplot(data = company_data, aes(x = Date)) +
    geom_line(aes(y = Closing, color = "Stock Price"), size = 1) +
    geom_line(aes(y = Volume / max(Volume) * max(Closing), color = "Volume"), size = 1) +
    scale_y_continuous(name = "Stock Price",
                       sec.axis = sec_axis(~ . / max(company_data$Closing) * max(company_data$Volume), name = "Volume")) +
    scale_color_manual(values = c("Stock Price" = "blue", "Volume" = "red")) +
    labs(title = paste("Stock Price and Volume for", company),
         x = "Date", y = "") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
  print(gg)
}

# Annual Average Percentage Deltas by Company
EC_pdelta_mean <- EC_pdelta %>%
  group_by(Company, Year) %>%
  summarise(
    Mean_Delta_Day = mean(Delta_Day, na.rm = TRUE),
    Mean_Delta_HL = mean(Delta_HL, na.rm = TRUE),
    Mean_Delta_OP_CP = mean(Delta_OP_CP, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(data = EC_pdelta_mean, aes(x = Year)) +
  geom_line(aes(y = Mean_Delta_Day, group = Company, color = "Mean Delta Day"), linewidth = 1) +
  geom_line(aes(y = Mean_Delta_HL, group = Company, color = "Mean Delta HL"), linewidth = 1) +
  geom_line(aes(y = Mean_Delta_OP_CP, group = Company, color = "Mean Delta OP-CP"), linewidth = 1) +
  facet_wrap(~ Company, scales = "free_y") +
  labs(title = "Annual Average of Percentage Deltas by Company",
       x = "Year",
       y = "Percentage Delta Average") +
  scale_color_manual(values = c("Mean Delta Day" = "red", "Mean Delta HL" = "green", "Mean Delta OP-CP" = "blue")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Percentage Deltas by Year and Quarter for Company
ggplot(data = EC_pdelta, aes(x = YQ)) +
  geom_line(aes(y = Delta_Day, group = Company, color = "Delta Day"), linewidth = 1) +
  geom_line(aes(y = Delta_HL, group = Company, color = "Delta HL"), linewidth = 1) +
  geom_line(aes(y = Delta_OP_CP, group = Company, color = "Delta OP-CP"), linewidth = 1) +
  facet_wrap(~ Company, scales = "free_y") +
  labs(title = "Percentage Deltas by Year and Quarter for Company",
       x = "",
       y = "Percentage Delta") +
  scale_color_manual(values = c("Delta Day" = "red", "Delta HL" = "green", "Delta OP-CP" = "blue")) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        axis.text.x = element_blank()) 
        
# Visualization of weekdays distribution
EC_pdelta$Weekday <- factor(EC_pdelta$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
EC_pdelta_long <- EC_pdelta %>%
  select(Delta_Day, Delta_HL, Delta_OP_CP, Weekday) %>%
  pivot_longer(cols = c(Delta_Day, Delta_HL, Delta_OP_CP), names_to = "Delta_Type", values_to = "Value")
EC_pdelta_mean <- EC_pdelta_long %>%
  group_by(Weekday, Delta_Type) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE)) %>%
  ungroup()
ggplot(EC_pdelta_mean, aes(x = Weekday, y = Mean_Value, group = Delta_Type, color = Delta_Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Deltas by Day of the Week",
       x = "Day of the Week",
       y = "Delta Average",
       color = "Type of Delta") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# Dependent Variable Distributions

# Distribution of Delta_HL by Company
ggplot(dataset_OLS, aes(x = Delta_HL, fill = Company)) + 
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") + 
  facet_wrap(~ Company) + 
  ggtitle("Distribution of Delta_HL by Company") +
  theme_minimal()
# Distribution of Delta_OP_CP
ggplot(dataset_OLS, aes(x = Delta_OP_CP, fill = Company)) + 
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") + 
  facet_wrap(~ Company) + 
  ggtitle("Distribution of Delta_OP_CP by Company") +
  theme_minimal()
# Distribution of Delta_Day
ggplot(dataset_OLS, aes(x = Delta_Day, fill = Company)) + 
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") + 
  facet_wrap(~ Company) + 
  ggtitle("Distribution of Delta_Day by Company") +
  theme_minimal()
