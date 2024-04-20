# The following script will perform OLS regression on Goldman Sachs and UBS sentiments
# outputted from Loughran-McDonald sentiment analysis.

library(lmtest)
library(car)
library(corrplot)
library(dplyr)
library(data.table)
library(leaps)

# Dataset building (do not run) #########

# Sentiment relative score
sentiment_columns <- c("positive", "negative", "uncertainty", "litigious", "strong_modal", "weak_modal", "constraining")
# Compute total sentiment scores per doc
sentiment_scores_all[, total_sentiment := rowSums(.SD), .SDcols = sentiment_columns]
# Compute Sentiment Proportions
for (col in sentiment_columns) {
  sentiment_scores_all[, paste0(col, "_prop") := get(col) / total_sentiment]
}
sentiment_scores_all$polarity <- with(sentiment_score, (positive - negative) / (positive + negative))
# Configure Sentiment datatable
sentiment_scores_all[, c("positive", "negative", "uncertainty", "litigious", "strong_modal", "weak_modal", "constraining", "total_sentiment", "doc_id") := NULL] 
setnames(sentiment_scores_all, "company", "Company")
setnames(sentiment_scores_all, "quarter", "Quarter")
setnames(sentiment_scores_all, "year", "Year")

# Comprehensive Dataset creation
dataset_OLS_sentiments <- merge(sentiment_scores_all, EC_pdelta, by = c("Company", "Quarter", "Year"))
dataset_OLS_sentiments <- subset(dataset_OLS_sentiments, select = -c(YQ, Date, Doc_ID, Volume))



# OLS Regression Modeling ######
# Regressions for GS
model_Day_gs_s <- lm(Delta_Day ~ positive_prop + negative_prop + uncertainty_prop + litigious_prop + strong_modal_prop + weak_modal_prop + constraining_prop + polarity, data = filter(dataset_OLS_sentiments, Company == "GS"))
summary(model_Day_gs_s) # choice
model_HL_gs_s <- lm(Delta_HL ~ positive_prop + negative_prop + uncertainty_prop + litigious_prop + strong_modal_prop + weak_modal_prop + constraining_prop + polarity, data = filter(dataset_OLS_sentiments, Company == "GS"))
summary(model_HL_gs_s)
model_OP_CP_gs_s <- lm(Delta_OP_CP ~ positive_prop + negative_prop + uncertainty_prop + litigious_prop + strong_modal_prop + weak_modal_prop + constraining_prop + polarity, data = filter(dataset_OLS_sentiments, Company == "GS"))
summary(model_OP_CP_gs_s)

# Regressions for UBS
model_Day_ubs_s <- lm(Delta_Day ~ positive_prop + negative_prop + uncertainty_prop + litigious_prop + strong_modal_prop + weak_modal_prop + constraining_prop + polarity, data = filter(dataset_OLS_sentiments, Company == "UBS"))
summary(model_Day_ubs_s)
model_HL_ubs_s <- lm(Delta_HL ~ positive_prop + negative_prop + uncertainty_prop + litigious_prop + strong_modal_prop + weak_modal_prop + constraining_prop + polarity, data = filter(dataset_OLS_sentiments, Company == "UBS"))
summary(model_HL_ubs_s)
model_OP_CP_ubs_s <- lm(Delta_OP_CP ~ positive_prop + negative_prop + uncertainty_prop + litigious_prop + strong_modal_prop + weak_modal_prop + constraining_prop + polarity, data = filter(dataset_OLS_sentiments, Company == "UBS"))
summary(model_OP_CP_ubs_s) # choice

# Examine Linear Relationships
ggpairs(dataset_OLS_sentiments_gs[, c("Delta_Day", "positive_prop", "negative_prop", "uncertainty_prop", "litigious_prop", "strong_modal_prop", "weak_modal_prop", "constraining_prop", "polarity")])
ggpairs(dataset_OLS_sentiments_ubs[, c("Delta_OP_CP", "positive_prop", "negative_prop", "uncertainty_prop", "litigious_prop", "strong_modal_prop", "weak_modal_prop", "constraining_prop", "polarity")])

ggpairs(dataset_OLS_sentiments_gs[, c("Delta_Day", "positive_prop", "negative_prop", "uncertainty_prop", "litigious_prop", "strong_modal_prop", "weak_modal_prop", "constraining_prop", "polarity")],
        lower = list(continuous = wrap("points", size = 1)),
        upper = list(continuous = wrap("cor", size = 2.5)),
        theme = ggplot2::theme(text = ggplot2::element_text(size = 8))) 

ggpairs(dataset_OLS_sentiments_ubs[, c("Delta_OP_CP", "positive_prop", "negative_prop", "uncertainty_prop", "litigious_prop", "strong_modal_prop", "weak_modal_prop", "constraining_prop", "polarity")],
        lower = list(continuous = wrap("points", size = 1)),
        upper = list(continuous = wrap("cor", size = 2.5)),
        theme = ggplot2::theme(text = ggplot2::element_text(size = 8))) 

######### ADJUSTING VARIABLES DISTRIBUTIONS: QUADRATIC TERMS

# GS: RegSubset on Quadratic Terms
dataset_OLS_sentiments_gs <- filter(dataset_OLS_sentiments, Company == "GS")
results_gs_s <- regsubsets(Delta_Day ~ positive_prop + negative_prop + uncertainty_prop + litigious_prop + strong_modal_prop + weak_modal_prop + constraining_prop + polarity +
                             I(positive_prop^2) + I(negative_prop^2) + I(uncertainty_prop^2) + I(litigious_prop^2) + I(strong_modal_prop^2) + I(weak_modal_prop^2) + I(constraining_prop^2) + I(polarity^2),
                           data = dataset_OLS_sentiments_gs, nbest = 1, really.big = TRUE, method = "exhaustive")
summary(results_gs_s)

# GS: Quadratic Regression Models
model_Day_gs_s_exp <- lm(Delta_Day ~ negative_prop + uncertainty_prop + constraining_prop + polarity + 
                       I(litigious_prop^2) + I(strong_modal_prop^2) + I(weak_modal_prop^2) + I(constraining_prop^2) + I(polarity^2), data = filter(dataset_OLS_sentiments, Company == "GS"))
model_HL_gs_s_exp <- lm(Delta_HL ~ positive_prop + negative_prop + strong_modal_prop + constraining_prop + polarity + I(positive_prop^2) + 
                      I(negative_prop^2) + I(strong_modal_prop^2) + I(constraining_prop^2), data = filter(dataset_OLS_sentiments, Company == "GS"))
model_OP_CP_gs_s_exp <- lm(Delta_OP_CP ~ positive_prop + litigious_prop + constraining_prop + 
                         I(positive_prop^2) + I(negative_prop^2) + I(uncertainty_prop^2) + I(strong_modal_prop^2) +
                         I(weak_modal_prop^2) + I(constraining_prop^2), data = filter(dataset_OLS_sentiments, Company == "GS"))
summary(model_Day_gs_s_exp) # choice
summary(model_HL_gs_s_exp)
summary(model_OP_CP_gs_s_exp)

# UBS: RegSubset on Quadratic Terms
dataset_OLS_sentiments_ubs <- filter(dataset_OLS_sentiments, Company == "UBS")
results_ubs_s <- regsubsets(Delta_OP_CP ~ positive_prop + negative_prop + uncertainty_prop + litigious_prop + strong_modal_prop + weak_modal_prop + constraining_prop + polarity +
                              I(positive_prop^2) + I(negative_prop^2) + I(uncertainty_prop^2) + I(litigious_prop^2) + I(strong_modal_prop^2) + I(weak_modal_prop^2) + I(constraining_prop^2) + I(polarity^2),
                            data = dataset_OLS_sentiments_ubs, nbest = 1, really.big = TRUE, method = "exhaustive")
summary(results_ubs_s)

# UBS: Quadratic Regression Models
model_Day_ubs_s_exp <- lm(Delta_Day ~ positive_prop + negative_prop + uncertainty_prop + litigious_prop + polarity + I(positive_prop^2) + 
                        I(negative_prop^2) + I(litigious_prop^2) + I(polarity^2), data = filter(dataset_OLS_sentiments, Company == "UBS"))
model_HL_ubs_s_exp <- lm(Delta_HL ~ positive_prop + negative_prop + strong_modal_prop + constraining_prop + polarity + I(positive_prop^2) + 
                       I(negative_prop^2) + I(strong_modal_prop^2) + I(constraining_prop^2), data = filter(dataset_OLS_sentiments, Company == "UBS"))
model_OP_CP_ubs_s_exp <- lm(Delta_OP_CP ~ negative_prop + uncertainty_prop + weak_modal_prop + constraining_prop + polarity + 
                          I(litigious_prop^2) + I(strong_modal_prop^2) + I(constraining_prop^2) + I(polarity^2), data = filter(dataset_OLS_sentiments, Company == "UBS"))
summary(model_Day_ubs_s_exp)
summary(model_HL_ubs_s_exp)
summary(model_OP_CP_ubs_s_exp) # choice

# Diagnostic Checks
vif(model_Day_gs_s_exp)
vif(model_OP_CP_ubs_s_exp)

# QQ Plots
par(mfrow = c(1, 2))
residuals_Day_gs_s <- residuals(model_Day_gs_s_exp)
qqnorm(residuals_Day_gs_s, main = "Q-Q Plot for model_Day_gs_s_exp")
qqline(residuals_Day_gs_s)

residuals_OP_CP_ubs_s <- residuals(model_OP_CP_ubs_s_exp)
qqnorm(residuals_OP_CP_ubs_s, main = "Q-Q Plot for model_OP_CP_ubs_s_exp")
qqline(residuals_OP_CP_ubs_s)
par(mfrow = c(1, 1))

# Shapiro-Wilk Test for Residuals
shapiro.test(residuals(model_Day_gs_s_exp))
shapiro.test(residuals(model_OP_CP_ubs_s_exp))

# Residuals Analysis
par(mfrow = c(1, 2))
plot(residuals(model_Day_gs_s_exp) ~ fitted(model_Day_gs_s_exp))
abline(h = 0, col = "red")
plot(residuals(model_OP_CP_ubs_s_exp) ~ fitted(model_OP_CP_ubs_s_exp))
abline(h = 0, col = "red")
par(mfrow = c(1, 1))

# Ramsey RESET Test
resettest(model_Day_gs_s_exp, power = 2:3)
resettest(model_OP_CP_ubs_s_exp, power = 2:3)


# MACROECONOMIC & FINANCIAL VARIABLES ANALYSIS (correlations and distributions)
# Adding financial and macroeconomic metrics to the dataset
dataset_OLS_sentiments <- merge(dataset_OLS_sentiments, 
                                EC_financials[, c("Company", "Quarter", "Year", "P/E", 
                                                  "EPS (diluted) TTM", "P/B", "Outstanding Shares (M)", 
                                                  "Revenues TTM", "ROIC", "FCF TTM", "Payout Ratio", 
                                                  "Dividend $", "D/E")], 
                                by = c("Company", "Quarter", "Year"), 
                                all.x = TRUE)
EC_macro$Year <- as.character(EC_macro$Year)
dataset_OLS_sentiments <- merge(dataset_OLS_sentiments, 
                                EC_macro[, c("Company", "Quarter", "Year", "10Y (%)", "ForEx", "Inflation (%)", "MSCI ACWI Banks")], 
                                by = c("Company", "Quarter", "Year"), 
                                all.x = TRUE)

# Test a FULL LINEAR model with all new independent variables, GS
dataset_OLS_sentiments_gs <- filter(dataset_OLS_sentiments, Company == "GS")
dataset_OLS_sentiments_ubs <- filter(dataset_OLS_sentiments, Company == "UBS")
subsets_fit <- regsubsets(Delta_Day ~ `P/E` + I(`P/E`^2) + `EPS (diluted) TTM` + I(`EPS (diluted) TTM`^2) + 
                            `P/B` + I(`P/B`^2) + `Outstanding Shares (M)` + I(`Outstanding Shares (M)`^2) + 
                            `Revenues TTM` + I(`Revenues TTM`^2) + ROIC + I(ROIC^2) + `FCF TTM` + I(`FCF TTM`^2) + 
                            `Payout Ratio` + I(`Payout Ratio`^2) + `Dividend $` + `Dividend Ann.` + `D/E` + I(`D/E`^2) + 
                            `Dividend $`:`Dividend Ann.` + `10Y (%)` + ForEx + `Inflation (%)` + `MSCI ACWI Banks` +
                            I(`10Y (%)`^2) + I(ForEx^2) + I(`Inflation (%)`^2) + I(`MSCI ACWI Banks`^2) + 
                            `Dividend $`:`Dividend Ann.` + positive_prop + negative_prop + uncertainty_prop + litigious_prop + strong_modal_prop + weak_modal_prop + constraining_prop + polarity +
                            I(positive_prop^2) + I(negative_prop^2) + I(uncertainty_prop^2) + I(litigious_prop^2) + I(strong_modal_prop^2) + I(weak_modal_prop^2) + I(constraining_prop^2) + I(polarity^2), data = dataset_OLS_sentiments_gs, nbest = 1, really.big = TRUE)
summary(subsets_fit)

# OPTIMIZED FINAL MODELS
model_Day_gs_s_optimized <- lm(Delta_Day ~ negative_prop + constraining_prop + I(negative_prop^2) + (litigious_prop^2) +
                               + I(`Inflation (%)`^2) + I(`P/B`^2) + I(`Revenues TTM`^2)
                               + I(ROIC^2) + `10Y (%)` + I(`10Y (%)`^2) + I(`MSCI ACWI Banks`^2), data = dataset_OLS_sentiments_gs) 
summary(model_Day_gs_s_optimized)
vif(model_Day_gs_s_optimized)

model_HL_ubs_s_optimized <- lm(Delta_OP_CP ~ negative_prop + litigious_prop + constraining_prop + I(negative_prop^2)
                               + I(litigious_prop^2) + I(constraining_prop^2) + `P/B`
                               + I(`P/B`^2) + `Dividend $`, data = dataset_OLS_sentiments_ubs) 
summary(model_HL_ubs_s_optimized)
vif(model_HL_ubs_s_optimized)

# Durbin-Watson test for autocorrelation
dwtest(model_Day_gs_s_optimized)
dwtest(model_HL_ubs_s_optimized)

# QQ plot GS
par(mfrow = c(1, 2))
residuals_Day_gs_s_optimized <- residuals(model_Day_gs_s_optimized)
qqnorm(residuals_Day_gs_s_optimized, main = "Q-Q Plot for Sentiment comprehensive model, GS")
qqline(residuals_Day_gs_s_optimized)

# QQ plot UBS
residuals_HL_ubs_s_optimized <- residuals(model_HL_ubs_s_optimized)
qqnorm(residuals_HL_ubs_s_optimized, main = "Q-Q Plot for Sentiment comprehensive model, UBS")
qqline(residuals_HL_ubs_s_optimized)
par(mfrow = c(1, 1))

# Residuals vs Fitted values
par(mfrow = c(1, 2))
plot(residuals(model_Day_gs_s_optimized) ~ fitted(model_Day_gs_s_optimized), main = "Residuals vs Fitted for GS")
abline(h = 0, col = "red")
plot(residuals(model_HL_ubs_s_optimized) ~ fitted(model_HL_ubs_s_optimized), main = "Residuals vs Fitted for UBS")
abline(h = 0, col = "red")
par(mfrow = c(1, 1))