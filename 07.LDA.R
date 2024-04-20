# This code performs topic modeling on bigrams' corpus, by means of LDA.

library(tidyr)
library(ggplot2)
library(dplyr) 
library(quanteda) 
library(broom)
library(tidytext)

# Extract bigrams corpus from DFM
dfm_gs = dfm_subset(ect_dfm_bi, company == "GS")
dfm_ubs = dfm_subset(ect_dfm_bi, company == "UBS")

# Apply LDA to individual sub-corpora (company)
LDAmodel_gs <- LDA(dfm_gs, k = 10, control = list(seed = 321))
LDAmodel_ubs <- LDA(dfm_ubs, k = 10, control = list(seed = 321))

# Convert GS and UBS results into dataframes for visualization
gamma_gs <- as.data.frame(posterior(LDAmodel_gs)$topics)
topic_titles_gs <- c("Capital_Markets", "Fixed_Income", "Product_Innovation", 
                     "Operating_Leverage", "Market_Strategy", "Client_Focus", 
                     "Asset_Management", "Global_Markets", "Income_Creation", "Growth_Strategy")
colnames(gamma_gs) <- topic_titles_gs
gamma_gs$Year <- docvars(dfm_gs, "year")
gamma_gs$Company <- docvars(dfm_gs, "company")
gamma_gs$Quarter <- docvars(dfm_gs, "quarter")

gamma_ubs <- as.data.frame(posterior(LDAmodel_ubs)$topics)
topic_titles_ubs <- c("Cost_Income", "Corporate_Strategy", "Op_Sustainablity",
                      "Global_Investment", "Fee_Generation", "Asset_Growth", "Cross_Border",
                      "Investment_Banking", "Credit_Suisse", "Investment_Growth")
colnames(gamma_ubs) <- topic_titles_ubs
gamma_ubs$Year <- docvars(dfm_ubs, "year")
gamma_ubs$Company <- docvars(dfm_ubs, "company")
gamma_ubs$Quarter <- docvars(dfm_ubs, "quarter")

# Visualize top 10 terms in each topic
n_terms <- 10
terms_gs <- terms(LDAmodel_gs, n_terms)
print(terms_gs)
terms_ubs <- terms(LDAmodel_ubs, n_terms)
print(terms_ubs)

# GS
topic_distr_gs <- gamma_gs
numeric_columns_gs <- sapply(topic_distr_gs, is.numeric) & !names(topic_distr_gs) %in% c("Year", "Company", "Quarter")
cols_to_keep_gs <- c("Year", "Quarter", names(numeric_columns_gs)[numeric_columns_gs])
avg_topic_distr_gs <- aggregate(. ~ Year + Quarter, data = topic_distr_gs[, cols_to_keep_gs], FUN = mean, na.rm = TRUE)
long_avg_topic_distr_gs <- pivot_longer(avg_topic_distr_gs,
                                        cols = all_of(topic_titles_gs),
                                        names_to = "topic",
                                        values_to = "gamma_prop")

# UBS
topic_distr_ubs <- gamma_ubs
numeric_columns_ubs <- sapply(topic_distr_ubs, is.numeric) & !names(topic_distr_ubs) %in% c("Year", "Company", "Quarter")
cols_to_keep_ubs <- c("Year", "Quarter", names(numeric_columns_ubs)[numeric_columns_ubs])
avg_topic_distr_ubs <- aggregate(. ~ Year + Quarter, data = topic_distr_ubs[, cols_to_keep_ubs], FUN = mean, na.rm = TRUE)
long_avg_topic_distr_ubs <- pivot_longer(avg_topic_distr_ubs,
                                         cols = all_of(topic_titles_ubs),
                                         names_to = "topic",
                                         values_to = "gamma_prop")


# Visualization top terms for GS
top_terms_gs <- tidy(LDAmodel_gs, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_gs$topic <- factor(paste0("V", top_terms_gs$topic))
top_terms_gs <- top_terms_gs %>%
  mutate(topic = factor(topic, levels = paste0("V", 1:10), labels = topic_titles_gs))
ggplot(top_terms_gs, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y", ncol = 5) +
  coord_flip() +
  labs(title = "Most relevant terms in GS Topics",
       x = "Terms",
       y = "Term Weight (Beta)") +
  theme_minimal()

# Visualization top terms for UBS
top_terms_ubs <- tidy(LDAmodel_ubs, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_ubs$topic <- factor(paste0("V", top_terms_ubs$topic))
top_terms_ubs <- top_terms_ubs %>%
  mutate(topic = factor(topic, levels = paste0("V", 1:10), labels = topic_titles_ubs))
ggplot(top_terms_ubs, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y", ncol = 5) +
  coord_flip() +
  labs(title = "Most relevant terms in UBS Topics",
       x = "Terms",
       y = "Term Weight (Beta)") +
  theme_minimal()

# List of top 50 words for each topic, GS
top_words_per_topic_gs <- tidy(LDAmodel_gs, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(50, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
words_summary_gs <- top_words_per_topic_gs %>%
  group_by(topic) %>%
  summarise(words = paste(term, collapse = ", ")) %>%
  arrange(topic)
cat("Top 50 words in GS topics")
for (i in 1:nrow(words_summary)) {
  cat("Topic", words_summary$topic[i], ": ", words_summary$words[i], "\n\n")
}

# List of top 50 words for each topic, UBS
top_words_per_topic_ubs <- tidy(LDAmodel_ubs, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(50, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
words_summary_ubs <- top_words_per_topic_ubs %>%
  group_by(topic) %>%
  summarise(words = paste(term, collapse = ", ")) %>%
  arrange(topic)
cat("Top 50 words in UBS topics")
for (i in 1:nrow(words_summary_ubs)) {
  cat("Topic", words_summary_ubs$topic[i], ": ", words_summary_ubs$words[i], "\n\n")
}

# Visualizations

# Topic Distribution over years, GS
ggplot(long_avg_topic_distr_gs, aes(x = as.factor(Year), y = gamma_prop, fill = topic)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Average Topic Distribution per Year - GS") +
  xlab("Year") +
  ylab("Average Topic Proportion") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "bottom")

# Topic Distribution over years, UBS
ggplot(long_avg_topic_distr_ubs, aes(x = as.factor(Year), y = gamma_prop, fill = topic)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Average Topic Distribution per Year - UBS") +
  xlab("Year") +
  ylab("Average Topic Proportion") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "bottom")

# Topic Distribution over quarters, GS
ggplot(long_avg_topic_distr_gs, aes(x = as.factor(Quarter), y = gamma_prop, fill = topic)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Average Topic Distribution per Quarter - GS") +
  xlab("Quarter") +
  ylab("Average Topic Proportion") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "bottom")

# Topic Distribution over quarters, UBS
ggplot(long_avg_topic_distr_ubs, aes(x = as.factor(Quarter), y = gamma_prop, fill = topic)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Average Topic Distribution per Quarter - UBS") +
  xlab("Quarter") +
  ylab("Average Topic Proportion") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "bottom")

# Comprehensive Pie Chart GS
topic_weights_total <- colSums(avg_topic_distr_gs[,-c(1,2)], na.rm = TRUE)
topic_weights_df <- data.frame(topic = names(topic_weights_total), weight = topic_weights_total, row.names = NULL)
ggplot(topic_weights_df, aes(x = "", y = weight, fill = topic)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") + 
  labs(fill = "Topic", y = "Total Weight", x = NULL, title = "Comprehensive Topic Weights, GS") +
  theme_void() + 
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 10)) 

# Comprehensive Pie Chart UBS
topic_weights_total <- colSums(avg_topic_distr_ubs[,-c(1,2)], na.rm = TRUE)
topic_weights_df <- data.frame(topic = names(topic_weights_total), weight = topic_weights_total, row.names = NULL)
ggplot(topic_weights_df, aes(x = "", y = weight, fill = topic)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  labs(fill = "Topic", y = "Total Weight", x = NULL, title = "Comprehensive Topic Weights, UBS") +
  theme_void() +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 10))
