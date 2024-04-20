# The following script will perform OLS regression on Goldman Sachs and UBS topics
# outputted from LDA.

library(dplyr)
library(stringr)
library(car)
library(leaps)
library(lmtest)
library(GGally)
library(glmnet)
library(corrplot)
library(caret)


# GS Data Preprocessing
dataset_OLS_gs <- merge(gamma_gs, EC_pdelta, by = c("Company", "Quarter", "Year"))
dataset_OLS_gs <- subset(dataset_OLS_gs, select = -c(YQ, Date, Doc_ID))
# Rename GS topic columns with their assigned titles
names(dataset_OLS_gs) <- gsub("^([0-9]+)$", "V\\1", names(dataset_OLS_gs))
dataset_OLS_gs <- as.data.frame(dataset_OLS_gs)

# UBS Data Preprocessing
dataset_OLS_ubs <- merge(gamma_ubs, EC_pdelta, by = c("Company", "Quarter", "Year"))
dataset_OLS_ubs <- subset(dataset_OLS_ubs, select = -c(YQ, Date, Doc_ID))
# Rename UBS topic columns with their assigned titles
names(dataset_OLS_ubs) <- gsub("^([0-9]+)$", "V\\1", names(dataset_OLS_ubs))
dataset_OLS_ubs <- as.data.frame(dataset_OLS_ubs)

# GS full linear regressions (act to choose the dependent variable)
model_Day_gs <- lm(Delta_Day ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + Volume, data = dataset_OLS_gs)
summary(model_Day_gs)
model_HL_gs <- lm(Delta_HL ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + Volume, data = dataset_OLS_gs)
summary(model_HL_gs)
model_OP_CP_gs <- lm(Delta_OP_CP ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + Volume, data = dataset_OLS_gs)
summary(model_OP_CP_gs) # choice

# UBS full linear regressions (act to choose the dependent variable)
model_Day_ubs <- lm(Delta_Day ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + Volume, data = dataset_OLS_ubs)
summary(model_Day_ubs)
model_HL_ubs <- lm(Delta_HL ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + Volume, data = dataset_OLS_ubs)
summary(model_HL_ubs) # choice
model_OP_CP_ubs <- lm(Delta_OP_CP ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + Volume, data = dataset_OLS_ubs)
summary(model_OP_CP_ubs)

# Examine Linear Relationships
# GS:
ggpairs(dataset_OLS_gs[, c("Delta_OP_CP", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")],
        lower = list(continuous = wrap("points", size = 1)),
        upper = list(continuous = wrap("cor", size = 2.5)),
        theme = ggplot2::theme(text = ggplot2::element_text(size = 8))) 

# UBS:
ggpairs(dataset_OLS_ubs[, c("Delta_HL", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")],
        lower = list(continuous = wrap("points", size = 1)),
        upper = list(continuous = wrap("cor", size = 2.5)),
        theme = ggplot2::theme(text = ggplot2::element_text(size = 8)))


######### ADJUSTING VARIABLES DISTRIBUTIONS: QUADRATIC TERMS

# GS: RegSubset on Quadratic Terms
results_gs <- regsubsets(Delta_OP_CP ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 +
                           I(V1^2) + I(V2^2) + I(V3^2) + I(V4^2) + I(V5^2) + 
                           I(V6^2) + I(V7^2) + I(V8^2) + I(V9^2) + I(V10^2) + 
                           Volume + I(Volume^2),
                         data = dataset_OLS_gs, nbest = 1, really.big = TRUE, method = "exhaustive")
summary(results_gs)

# GS: Quadratic Regression Models
model_Day_gs_exp <- lm(Delta_Day ~ V2 + V3 + V7 + V9 + I(V2^2) + I(V3^2) + I(V7^2) + I(V9^2) + Volume, data = dataset_OLS_gs)
model_HL_gs_exp <- lm(Delta_HL ~ V3 + V5 + V10 + I(V3^2) + I(V5^2) + I(V8^2) + I(V10^2) + Volume + I(Volume^2), data = dataset_OLS_gs)
model_OP_CP_gs_exp <- lm(Delta_OP_CP ~ V1 + V9 + V10 + I(V1^2) + I(V2^2) + I(V3^2) + I(V4^2) + I(V8^2) + I(V9^2) + I(Volume^2), data = dataset_OLS_gs)

summary(model_Day_gs_exp)
summary(model_HL_gs_exp)
summary(model_OP_CP_gs_exp) # choice

# UBS: RegSubset on Quadratic Terms
results_ubs <- regsubsets(Delta_HL ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 +
                            I(V1^2) + I(V2^2) + I(V3^2) + I(V4^2) + I(V5^2) + 
                            I(V6^2) + I(V7^2) + I(V8^2) + I(V9^2) + I(V10^2) + 
                            Volume + I(Volume^2),
                          data = dataset_OLS_ubs, nbest = 1, really.big = TRUE, method = "exhaustive")
summary(results_ubs)

# UBS: Quadratic Regression Models
model_Day_ubs_exp <- lm(Delta_Day ~ V1 + V3 + V6 + V9 + I(V3^2) + I(V6^2) + Volume + I(Volume^2), data = dataset_OLS_ubs)
model_HL_ubs_exp <- lm(Delta_HL ~ V3 + V5 + V6 + I(V3^2) + I(V5^2) + I(V6^2) + I(V7^2) + I(V9^2), data = dataset_OLS_ubs)
model_OP_CP_ubs_exp <- lm(Delta_OP_CP ~ V3 + V6 + I(V3^2) + I(V5^2) + I(V6^2) + I(V8^2) + Volume + I(Volume^2), data = dataset_OLS_ubs)

summary(model_Day_ubs_exp)
summary(model_HL_ubs_exp) # choice
summary(model_OP_CP_ubs_exp)

# Diagnostic Checks
vif(model_OP_CP_gs_exp)
vif(model_HL_ubs_exp)

# QQ Plots
par(mfrow = c(1, 2))
qqnorm(residuals(model_OP_CP_gs_exp), main = "Q-Q Plot for model_OP_CP_gs_exp")
qqline(residuals(model_OP_CP_gs_exp))
qqnorm(residuals(model_HL_ubs_exp), main = "Q-Q Plot for model_HL_ubs_exp")
qqline(residuals(model_HL_ubs_exp))
par(mfrow = c(1, 1))

# Shapiro-Wilk Test for Residuals
shapiro.test(residuals(model_HL_ubs_exp))
shapiro.test(residuals(model_OP_CP_gs_exp))

# Residuals Analysis
par(mfrow = c(1, 2))
plot(residuals(model_OP_CP_gs_exp) ~ fitted(model_OP_CP_gs_exp), main = "Residuals vs Fitted for GS")
abline(h = 0, col = "red")
plot(residuals(model_HL_ubs_exp) ~ fitted(model_HL_ubs_exp), main = "Residuals vs Fitted for UBS")
abline(h = 0, col = "red")
par(mfrow = c(1, 1))

# Ramsey RESET Test
resettest(model_OP_CP_gs_exp, power = 2:3)
resettest(model_HL_ubs_exp, power = 2:3)


# MACROECONOMIC & FINANCIAL VARIABLES ANALYSIS (correlations and distributions)
# Test a FULL LINEAR model with new independent variables, GS
model_OP_CP_gs_linear <- lm(Delta_OP_CP ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + Volume + `P/E` + `D/E`
                            + `EPS (diluted) TTM` + `P/B` + `Outstanding Shares (M)` + ROIC + `MSCI ACWI Banks` +
                            + `FCF TTM` + `Payout Ratio` + `10Y (%)`+ ForEx + `Inflation (%)`, data = dataset_OLS_gs)
summary(model_OP_CP_gs_linear)
vif(model_OP_CP_gs_linear)

# Searching for Collinearity
independent_vars_gs <- dataset_OLS_gs[, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "Volume", "P/E", "EPS (diluted) TTM", "P/B", "ROIC",
                                          "FCF TTM", "Payout Ratio", "10Y (%)", "ForEx", "MSCI ACWI Banks", "Inflation (%)", "D/E", "Outstanding Shares (M)")]
cor_matrix_gs <- cor(independent_vars_gs, use = "complete.obs") 
print(cor_matrix_gs)
heatmap(cor_matrix_gs, symm = TRUE, margin = c(10, 10))

# Plot the correlation matrix using corrplot
output_path <- file.path(getwd(), "Plots", "12.", "correlation_plot_gs.png")
png(output_path, width = 1200, height = 1200)
# Correlation plot
corrplot(cor_matrix_gs, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black",
         tl.cex = 1.2,
         cl.cex = 1.5, 
         number.cex = 1.2)
dev.off()

anova(model_OP_CP_gs,model_OP_CP_gs_linear) # has the model improved?

# Test a FULL LINEAR model with new independent variables, UBS
model_HL_ubs_linear <- lm(Delta_HL ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + Volume + `P/E` + `D/E`
                          + `EPS (diluted) TTM` + `P/B` + `Outstanding Shares (M)` + ROIC + `MSCI ACWI Banks` +
                            + `FCF TTM` + `Payout Ratio` + `10Y (%)`+ ForEx + `Inflation (%)`, data = dataset_OLS_ubs)
summary(model_HL_ubs_linear)
vif(model_HL_ubs_linear)

# Searching for Collinearity
independent_vars_ubs <- dataset_OLS_ubs[, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "Volume", "P/E", "EPS (diluted) TTM", "P/B", "ROIC",
                                            "FCF TTM", "Payout Ratio", "10Y (%)", "ForEx", "MSCI ACWI Banks", "Inflation (%)", "D/E", "Outstanding Shares (M)")]
cor_matrix_ubs <- cor(independent_vars_ubs, use = "complete.obs") 
print(cor_matrix_ubs)
heatmap(cor_matrix_ubs, symm = TRUE, margin = c(10, 10))

# Plot the correlation matrix using corrplot
output_path <- file.path(getwd(), "Plots", "12.", "correlation_plot_ubs.png")
png(output_path, width = 1200, height = 1200)
# Correlation plot
corrplot(cor_matrix_ubs, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black",
         tl.cex = 1.2,
         cl.cex = 1.5, 
         number.cex = 1.2)
dev.off()

anova(model_HL_ubs,model_HL_ubs_linear) # has the model improved?

######### FINANCIAL METRICS ADDITION

# Adding Financial Variables
dataset_OLS_gs <- merge(dataset_OLS_gs, EC_financials[, c("Company", "Quarter", "Year", 
                                                          "P/E", "EPS (diluted) TTM", "P/B", 
                                                          "Outstanding Shares (M)", "Revenues TTM", 
                                                          "ROIC", "FCF TTM", "Payout Ratio", 
                                                          "Dividend $", "D/E")],
                        by = c("Company", "Quarter", "Year"), all.x = TRUE)
dataset_OLS_ubs <- merge(dataset_OLS_ubs, EC_financials[, c("Company", "Quarter", "Year", 
                                                          "P/E", "EPS (diluted) TTM", "P/B", 
                                                          "Outstanding Shares (M)", "Revenues TTM", 
                                                          "ROIC", "FCF TTM", "Payout Ratio", 
                                                          "Dividend $", "D/E")],
                        by = c("Company", "Quarter", "Year"), all.x = TRUE)

# Examine Linear Relationships
ggpairs(dataset_OLS_gs[, c("Delta_OP_CP", "P/E", "EPS (diluted) TTM", "P/B", "Outstanding Shares (M)", "Revenues TTM", 
                           "ROIC", "FCF TTM", "Payout Ratio", "Dividend $", "D/E")])
ggpairs(dataset_OLS_ubs[, c("Delta_HL", "P/E", "EPS (diluted) TTM", "P/B", "Outstanding Shares (M)", "Revenues TTM", 
                            "ROIC", "FCF TTM", "Payout Ratio", "Dividend $", "D/E")])

ggpairs(dataset_OLS_gs[, c("Delta_OP_CP", "P/E", "EPS (diluted) TTM", "P/B", "Outstanding Shares (M)", "Revenues TTM", 
                           "ROIC", "FCF TTM", "Payout Ratio", "Dividend $", "D/E")],
        lower = list(continuous = wrap("points", size = 1)),
        upper = list(continuous = wrap("cor", size = 2.5)),
        theme = ggplot2::theme(text = ggplot2::element_text(size = 8))) 

ggpairs(dataset_OLS_ubs[, c("Delta_HL", "P/E", "EPS (diluted) TTM", "P/B", "Outstanding Shares (M)", "Revenues TTM", 
                            "ROIC", "FCF TTM", "Payout Ratio", "Dividend $", "D/E")],
        lower = list(continuous = wrap("points", size = 1)),
        upper = list(continuous = wrap("cor", size = 2.5)),
        theme = ggplot2::theme(text = ggplot2::element_text(size = 8))) 

# Regsubsets for both models adding financials metrics
subsets_fit <- regsubsets(Delta_HL ~ `P/E` + I(`P/E`^2) + `EPS (diluted) TTM` + I(`EPS (diluted) TTM`^2) + 
                            `P/B` + I(`P/B`^2) + `Outstanding Shares (M)` + I(`Outstanding Shares (M)`^2) + 
                            `Revenues TTM` + I(`Revenues TTM`^2) + ROIC + I(ROIC^2) + `FCF TTM` + I(`FCF TTM`^2) + 
                            `Payout Ratio` + I(`Payout Ratio`^2) + `Dividend $` + `Dividend Ann.` + `D/E` + I(`D/E`^2) + 
                            `Dividend $`:`Dividend Ann.`, data = dataset_OLS_ubs, nbest = 1, really.big = TRUE)
summary(subsets_fit)

# Models with financial variables
model_OP_CP_gs_expfin <- lm(Delta_OP_CP ~ V1 + V9 + V10 + I(V1^2) + I(V2^2) + I(V3^2) + I(V4^2)
                              + I(V8^2) + I(V9^2) + I(Volume^2) + `P/E` + `EPS (diluted) TTM` + `P/B`
                              + I(`P/E`^2) + ROIC + `FCF TTM` + `Payout Ratio` + I(`Payout Ratio`^2), data = dataset_OLS_gs) 
summary(model_OP_CP_gs_expfin)

model_HL_ubs_expfin <- lm(Delta_HL ~ V3 + V5 + V6 + I(V3^2) + I(V5^2) + I(V6^2) + I(V7^2) + I(V9^2) + `P/E`
                           + I(`EPS (diluted) TTM`^2) + `P/B` + I(`P/B`^2) + I(`Outstanding Shares (M)`^2) + I(ROIC^2)
                           + I(`FCF TTM`^2) + I(`Payout Ratio`^2), data = dataset_OLS_ubs)
summary(model_HL_ubs_expfin)

anova(model_OP_CP_gs_exp, model_OP_CP_gs_expfin) # has the model improved?
anova(model_HL_ubs_exp, model_HL_ubs_expfin) # has the model improved?


######### MACROECONOMICS METRICS ADDITION

# Adding Macro Variables
dataset_OLS_gs <- merge(dataset_OLS_gs, EC_macro[, c("Company", "Quarter", "Year", 
                                                     "10Y (%)", "ForEx", "Inflation (%)", "MSCI ACWI Banks")],
                        by = c("Company", "Quarter", "Year"), all.x = TRUE)
dataset_OLS_ubs <- merge(dataset_OLS_ubs, EC_macro[, c("Company", "Quarter", "Year", 
                                                     "10Y (%)", "ForEx", "Inflation (%)", "MSCI ACWI Banks")],
                         by = c("Company", "Quarter", "Year"), all.x = TRUE)

# Examine Linear Relationships
ggpairs(dataset_OLS_gs[, c("Delta_OP_CP", "10Y (%)", "ForEx", "Inflation (%)", "MSCI ACWI Banks")])
ggpairs(dataset_OLS_ubs[, c("Delta_HL", "10Y (%)", "ForEx", "Inflation (%)", "MSCI ACWI Banks")])

# Regsubsets for model adding macroeconomics metrics too
subsets_fit <- regsubsets(Delta_HL ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 +
                            I(V1^2) + I(V2^2) + I(V3^2) + I(V4^2) + I(V5^2) + 
                            I(V6^2) + I(V7^2) + I(V8^2) + I(V9^2) + I(V10^2) + 
                            Volume + I(Volume^2) + `P/E` + I(`P/E`^2) + `EPS (diluted) TTM` + I(`EPS (diluted) TTM`^2) + 
                            `P/B` + I(`P/B`^2) + `Outstanding Shares (M)` + I(`Outstanding Shares (M)`^2) + 
                            `Revenues TTM` + I(`Revenues TTM`^2) + ROIC + I(ROIC^2) + `FCF TTM` + I(`FCF TTM`^2) + 
                            `Payout Ratio` + I(`Payout Ratio`^2) + `Dividend $` + `Dividend Ann.` + `D/E` + I(`D/E`^2) + 
                            `Dividend $`:`Dividend Ann.` + `10Y (%)` + ForEx + `Inflation (%)` + `MSCI ACWI Banks` +
                            I(`10Y (%)`^2) + I(ForEx^2) + I(`Inflation (%)`^2) + I(`MSCI ACWI Banks`^2), data = dataset_OLS_ubs, nbest = 1, really.big = TRUE)
summary(subsets_fit)

# OPTIMIZED FINAL MODELS
model_OP_CP_gs_optimized <- lm(Delta_OP_CP ~ V8 + V9 + I(V2^2) + I(V8^2) +
                                 I(Volume^2) + I(`Revenues TTM`^2) + I(ROIC^2) + `10Y (%)` + I(`10Y (%)`^2) , data = dataset_OLS_gs) 
summary(model_OP_CP_gs_optimized)
vif(model_OP_CP_gs_optimized)

model_HL_ubs_optimized <- lm(Delta_HL ~ V5 + I(V3^2) + I(V5^2) + `P/E` + I(`Payout Ratio`^2)
                               + I(`D/E`^2) + ForEx + I(`10Y (%)`^2) + I(ForEx^2) + `Outstanding Shares (M)`, data = dataset_OLS_ubs) 
summary(model_HL_ubs_optimized)
vif(model_HL_ubs_optimized)

# Durbin-Watson test for autocorrelation
dwtest(model_OP_CP_gs_optimized)
dwtest(model_HL_ubs_optimized)

# QQ plot GS
par(mfrow = c(1, 2))
residuals_OP_CP_gs_optimized <- residuals(model_OP_CP_gs_optimized)
qqnorm(residuals_OP_CP_gs_optimized, main = "Q-Q Plot for comprehensive model, GS")
qqline(residuals_OP_CP_gs_optimized)

# QQ plot UBS
residuals_HL_ubs_optimized <- residuals(model_HL_ubs_optimized)
qqnorm(residuals_HL_ubs_optimized, main = "Q-Q Plot for comprehensive model, UBS")
qqline(residuals_HL_ubs_optimized)
par(mfrow = c(1, 1))

# Residuals vs Fitted values
par(mfrow = c(1, 2))
plot(residuals(model_OP_CP_gs_optimized) ~ fitted(model_OP_CP_gs_optimized), main = "Residuals vs Fitted for GS")
abline(h = 0, col = "red")
plot(residuals(model_HL_ubs_optimized) ~ fitted(model_HL_ubs_optimized), main = "Residuals vs Fitted for UBS")
abline(h = 0, col = "red")
par(mfrow = c(1, 1))