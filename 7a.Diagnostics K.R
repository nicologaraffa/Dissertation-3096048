# This script performs topic modeling diagnostics on K topics through -Log-Likelihood.

library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(patchwork)
library(ldatuning)

# Diagnostics k
k_values <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 75, 100)
lda_list <- vector("list", length(k_values))
metrics_gs <- data.frame(k = integer(), logLik = numeric(), perplexity = numeric())

# Cicle for K: GS
for (k in k_values) {
  cat("Computing LDA with", k, "topics\n")
  lda_model_gs <- LDA(dfm_gs, k = k, control = list(seed = 321))
  lda_list[[as.character(k)]] <- lda_model_gs
  
  # Log likelihood and Perplexity computations
  logLik_model <- logLik(lda_model_gs)
  perplexity_model <- perplexity(lda_model_gs)
  # Save output
  metrics_gs <- rbind(metrics_gs, data.frame(k = k, logLik = logLik_model, perplexity = perplexity_model))
}

# Cicle for K: UBS
metrics_ubs <- data.frame(k = integer(), logLik = numeric(), perplexity = numeric())

for (k in k_values) {
  cat("Computing LDA with", k, "topics\n")
  lda_model_ubs <- LDA(dfm_ubs, k = k, control = list(seed = 321))
  lda_list[[as.character(k)]] <- lda_model_ubs
  
  # Log likelihood and Perplexity computations
  logLik_model <- logLik(lda_model_ubs)
  perplexity_model <- perplexity(lda_model_ubs)
  # Save output
  metrics_ubs <- rbind(metrics_ubs, data.frame(k = k, logLik = logLik_model, perplexity = perplexity_model))
}

# Log-Likelihood plots, GS & UBS
log_gs <- ggplot(metrics_gs, aes(x = factor(k), y = -logLik, group = 1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  scale_x_discrete(name = "Number of Topics [k]", labels = k_values) +
  ylab("-Log Likelihood") +
  ggtitle("Model Metrics by k Number of Topics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

log_ubs <- ggplot(metrics_ubs, aes(x = factor(k), y = -logLik, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_discrete(name = "Number of Topics [k]", labels = k_values) +
  ylab("-Log Likelihood") +
  ggtitle("Model Metrics by k Number of Topics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Comprehensive Plot
log_gs + log_ubs + plot_layout(ncol = 2)

# Compute Differences in consecutive models, GS
metrics_gs$delta_perplexity <- c(NA, diff(metrics_gs$perplexity))
# Plot Delta Perplexity for GS
delta_gs <- ggplot(metrics_gs %>% filter(!is.na(delta_perplexity)), aes(x = factor(k), y = delta_perplexity)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  scale_x_discrete(name = "Number of Topics [k]", breaks = as.character(k_values)) +
  ylab("∆Perplexity for GS") +
  ggtitle("∆Perplexity between consecutive-k models, GS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Compute Differences in consecutive models, UBS
metrics_ubs$delta_perplexity <- c(NA, diff(metrics_ubs$perplexity))
# Plot Delta Perplexity for UBS
delta_ubs <- ggplot(metrics_ubs %>% filter(!is.na(delta_perplexity)), aes(x = factor(k), y = delta_perplexity)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_discrete(name = "Number of Topics [k]", breaks = as.character(k_values)) +
  ylab("∆Perplexity for UBS") +
  ggtitle("∆Perplexity between consecutive-k models, UBS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Comprehensive Plot
delta_gs + delta_ubs + plot_layout(ncol = 2)