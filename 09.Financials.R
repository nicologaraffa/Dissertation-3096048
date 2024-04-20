# This code imports and analyzes the regression independent variables: financial metrics.
# It also offers a variety of descriptive plots highlighting their trends.

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Import from Excel
data_xlsx <- "EC_Prices.xlsx"
EC_financials <- read_excel(data_xlsx, sheet = 2)
EC_financials <- EC_financials %>%
  select(-`Company Ticker`, -`Weekday`, -`Preceding Workday`, -`Following Workday`)
EC_financials$Date <- as.Date(EC_financials$Date)
print(EC_financials, n = 72)

# Descriptive Plots
# Price/Earning Comparison & Trend
ggplot(EC_financials, aes(x = Company, y = `P/E`, fill = Company)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Average P/E, GS & UBS", x = "", y = "P/E Ratio")

ggplot(EC_financials, aes(x = Date, y = `P/E`, color = Company)) +
  geom_line() +
  theme_minimal() +
  labs(title = "P/E Trend Over Time", x = "Time", y = "P/E") +
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Earnings Per Share Comparison & Trend
ggplot(EC_financials, aes(x = Company, y = `EPS (diluted) TTM`, fill = Company)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Diluted EPS TTM for GS and UBS", x = "", y = "EPS (diluted) TTM")

ggplot(EC_financials, aes(x = Date, y = `EPS (diluted) TTM`, color = Company)) +
  geom_line() +
  theme_minimal() +
  labs(title = "EPS Trend Over Time", x = "Time", y = "P/E") +
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Price to Book value Comparison & Trend
ggplot(EC_financials, aes(x = Company, y = `P/B`, fill = Company)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "P/B Comparison Between GS and UBS", x = "", y = "P/B Ratio")

ggplot(EC_financials, aes(x = Date, y = `P/B`, color = Company)) +
  geom_line() +
  theme_minimal() +
  labs(title = "P/B Trend Over Time", x = "Time", y = "P/E") +
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# No. of Outstanding Shares (non adjusted)
EC_financials_long <- EC_financials %>%
  gather(key = "Variable", value = "Value", `P/E`, `EPS (diluted) TTM`, `Outstanding Shares (M)`)
ggplot(data = EC_financials, aes(x = Date, y = `Outstanding Shares (M)`, group = Company, color = Company)) +
  geom_line() + 
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  labs(title = "Outstanding Shares for GS vs UBS",
       x = "Time",
       y = "Outstanding Shares (M)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Comprehensive 3-fold plot (EPS + no. Shares + P/E)
ggplot(EC_financials_long, aes(x = Date, y = Value, group = interaction(Company, Variable), color = Company, linetype = Variable)) +
  geom_line() +
  facet_wrap(~Variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Trend of Outstanding Shares, EPS, and P/E Over Time", x = NULL, y = "Value") +
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  scale_linetype_manual(values = c("P/E" = "solid", "EPS (diluted) TTM" = "solid", "Outstanding Shares (M)" = "solid")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom", 
        legend.box = "horizontal", 
        legend.title = element_blank()) 

# Dividend announcement Dummy
ggplot(EC_financials, aes(x = Date, y = `Dividend Ann.`, color = Company)) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) + 
  theme_minimal() +
  labs(title = "Dividend Announcements Over Time", x = "Time", y = "Dividend Announcement?") + 
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

# Dividend per share dollar amount ($)
EC_financials_sorted <- EC_financials %>%
  arrange(Year, Quarter) %>%
  mutate(YearQuarter = paste(Year, Quarter))
ggplot(EC_financials_sorted, aes(x = YearQuarter, y = `Dividend $`, group = Company, color = Company)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = `Dividend $`), vjust = -1, size = 2.5) + 
  theme_minimal() +
  labs(title = "Dividend $ Over Time by Company", x = "Year and Quarter", y = "Dividend $") +
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

# Payout ratio Comparison & Trend
ggplot(EC_financials_sorted, aes(x = YearQuarter, y = `Payout Ratio`, group = Company, color = Company)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = `Payout Ratio`), vjust = -1, size = 2.2) + 
  theme_minimal() +
  labs(title = "Payout Ratio Over Time by Company", x = "Year and Quarter", y = "Payout Ratio") +
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

# Comprehensive 2-fold plot (Revenues + TTM Cash Flow)
ggplot(data = EC_financials_sorted) +
  geom_line(aes(x = YearQuarter, y = `FCF TTM`, color = Company, group = Company), linetype = "dashed", alpha = 0.7) +
  geom_line(aes(x = YearQuarter, y = `Revenues TTM`, color = Company, group = Company), linetype = "solid", alpha = 1) +
  scale_color_manual(values = c("UBS" = "blue", "GS" = "red")) +
  labs(title = "Revenues TTM and FCF TTM Over Time by Company",
       x = "Year and Quarter", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

# TTM Revenues
ggplot(EC_financials, aes(x = Date, y = `Revenues TTM`, color = Company)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Revenues Trend Over Time", x = "Time", y = "Revenues TTM") +
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Debt/Equity ratio
ggplot(EC_financials, aes(x = Date, y = `D/E`, color = Company)) +
  geom_line() +
  theme_minimal() +
  labs(title = "D/E Trend Over Time", x = "Time", y = "D/E") +
  scale_color_manual(values = c("GS" = "red", "UBS" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Comprehensive 2-fold plot (Revenues + ROIC)
ggplot(data = EC_financials_sorted) +
  geom_line(aes(x = YearQuarter, y = `ROIC`, color = Company, group = Company), linetype = "dashed", alpha = 0.7) +
  geom_line(aes(x = YearQuarter, y = `Revenues TTM`, color = Company, group = Company), linetype = "solid", alpha = 1) +
  scale_color_manual(values = c("UBS" = "blue", "GS" = "red")) +
  labs(title = "Revenues TTM and ROIC Over Time by Company",
       x = "Year and Quarter", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
