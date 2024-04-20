# This script performs dictionary-based sentiment analysis on the transcripts.
# The imported dictionary is the Loughran-McDonald.

library(quanteda)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Load Loughran-McDonald
setwd("/Users/nicologaraffa/Documents/R Dissertation WD/Working Directory")
loughran_dic <- read_csv("Dictionaries/LM-SA-2020.csv")

# A look at words for each sentiment
positive_words <- loughran_dic$word[loughran_dic$sentiment == "Positive"]
negative_words <- loughran_dic$word[loughran_dic$sentiment == "Negative"]
uncertainty_words <- loughran_dic$word[loughran_dic$sentiment == "Uncertainty"]
litigious_words <- loughran_dic$word[loughran_dic$sentiment == "Litigious"]
strong_modal_words <- loughran_dic$word[loughran_dic$sentiment == "StrongModal"]
weak_modal_words <- loughran_dic$word[loughran_dic$sentiment == "WeakModal"]
constraining_words <- loughran_dic$word[loughran_dic$sentiment == "Constraining"]

# Dictionary creation with just negative and positive sentiments (first part)
sentiment_dic <- dictionary(list(positive = positive_words, negative = negative_words))
dfm_sentiment <- dfm_lookup(ect_dfm_nl, sentiment_dic)

# Absolute Sentiment score
sentiment_score <- dfm_weight(dfm_sentiment, scheme = "count") %>%
  convert(to = "data.frame") %>%
  rowwise() %>%
  mutate(sentiment = positive - negative) 

# Import metadata from docvars for further analysis
sentiment_score$company <- docvars(ect_dfm_nl, "company")
sentiment_score$year <- docvars(ect_dfm_nl, "year")
sentiment_score$quarter <- docvars(ect_dfm_nl, "quarter")

# Group by company
sentiment_scores_long <- sentiment_score %>%
  select(sentiment, company) %>%
  group_by(company) %>%
  summarise(average_sentiment = mean(sentiment, na.rm = TRUE))

# Boxplot for visualization
ggplot(sentiment_score, aes(x = company, y = sentiment, fill = company)) +
  geom_boxplot() +
  xlab("Company") + ylab("Sentiment Score") +
  ggtitle("Sentiment Scores distribution per company")

# UBS worst and best 
ubs_sentiment <- sentiment_score %>%
  filter(company == "UBS") %>%
  arrange(sentiment)
lowest_sentiment_discourse_ubs <- head(ubs_sentiment, 3)
highest_sentiment_discourse_ubs <- tail(ubs_sentiment, 3)

# GS worst and best
gs_sentiment <- sentiment_score %>%
  filter(company == "GS") %>%
  arrange(sentiment)
lowest_sentiment_discourse_gs <- head(gs_sentiment, 3)
highest_sentiment_discourse_gs <- tail(gs_sentiment, 3)

# Brief of the three most negative and most positive calls
print(lowest_sentiment_discourse_ubs)
print(lowest_sentiment_discourse_gs)
print(highest_sentiment_discourse_ubs)
print(highest_sentiment_discourse_gs)

# Descriptive Plots
# Sentiment scores over years
sentiment_score$year <- as.factor(sentiment_score$year)
ggplot(sentiment_score, aes(x = year, y = sentiment, fill = year)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Net Sentiment Scores distribution (positive - negative) per years", x = "Year", y = "Sentiment Score") +
  scale_fill_brewer(palette = "Pastel1")  

# Sentiment scores over years, per company
sentiment_score$year <- as.factor(sentiment_score$year)
sentiment_score$company <- as.factor(sentiment_score$company)
ggplot(sentiment_score, aes(x = year, y = sentiment, fill = year)) +
  geom_boxplot() + 
  facet_wrap(~ company, scales = "free", dir = "v") + 
  theme_minimal() +
  labs(title = "Net Sentiment Scores distribution (positive - negative) per Company and Year", 
       x = "Year", 
       y = "Sentiment Score") +
  scale_fill_brewer(palette = "Pastel1")

# Sentiment scores over quarters
ggplot(sentiment_score, aes(x = quarter, y = sentiment, fill = quarter)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Sentiment Scores distribution per quarters", x = "Quarter", y = "Sentiment Score") +
  scale_fill_brewer(palette = "Pastel2") 

# Positive and Negative sentiment scores over time
sentiment_trend <- sentiment_scores_all %>%
  select(company, year, positive_prop, negative_prop) %>%
  pivot_longer(cols = c(positive_prop, negative_prop),
               names_to = "sentiment_type",
               values_to = "score") %>%
  group_by(year, sentiment_type) %>%
  summarise(average_score = mean(score, na.rm = TRUE), .groups = 'drop')

ggplot(sentiment_trend, aes(x = year, y = average_score, color = sentiment_type)) +
  geom_line() +
  facet_wrap(~sentiment_type) +
  labs(title = "Positive and Negative sentiment, per year", x = "Year", y = "Average Abs. Sentiment Score") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

# Polarity
# Metric from -1 (all negative) to +1 (all positive)
sentiment_score$polarity <- with(sentiment_score, (positive - negative) / (positive + negative))

# Average polarity per company, over years and quarters
average_polarity <- sentiment_score %>%
  group_by(company, year) %>%
  summarise(average_polarity = mean(polarity, na.rm = TRUE), .groups = 'drop')
ggplot(average_polarity, aes(x = year, y = average_polarity, color = company, group = company)) +
  geom_line() +  
  geom_point() +  
  labs(title = "Average Polarity over years",
       x = "Year",
       y = "Average Polarity") +
  scale_color_brewer(palette = "Set1") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Most negative document, according to polarity
min_polarity_doc <- sentiment_score[which.min(sentiment_score$polarity), ]
# Most positive document, according to polarity
max_polarity_doc <- sentiment_score[which.max(sentiment_score$polarity), ]


# Importing all other sentiments
# Dictionary creation for Quanteda with all sentiments included
sentiment_dic <- dictionary(list(
  positive = positive_words,
  negative = negative_words,
  uncertainty = uncertainty_words,
  litigious = litigious_words,
  strong_modal = strong_modal_words,
  weak_modal = weak_modal_words,
  constraining = constraining_words
))

dfm_sentiments_all <- dfm_lookup(ect_dfm_nl, dictionary = sentiment_dic)
sentiment_scores_all <- convert(dfm_sentiments_all, to = "data.frame") %>%
  cbind(docvars(ect_dfm_nl), .)

# Convert in long format for plotting
sentiment_scores_long <- pivot_longer(sentiment_scores_all,
                                      cols = c(positive, negative, uncertainty, litigious, strong_modal, weak_modal, constraining),
                                      names_to = "sentiment_type",
                                      values_to = "score")

# All Sentiments distribution for Company, over years 
ggplot(sentiment_scores_long, aes(x = year, y = score, fill = sentiment_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~company) +
  labs(title = "All Sentiments distribution for Company, over years", x = "Year", y = "Sentiment Score") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# All Sentiments distribution for Company
sentiment_summary <- sentiment_scores_long %>%
  group_by(company, sentiment_type) %>%
  summarise(total_score = sum(score), .groups = 'drop') 
ggplot(sentiment_summary, aes(x = sentiment_type, y = total_score, fill = company)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  labs(title = "All Sentiments Scores per company, in absolute terms",
       x = "Sentiment",
       y = "Absolute Total Score") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
        axis.title = element_text(size = 14),  
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 10)) +  
  guides(fill = guide_legend(title = "Company")) 


# Focus on: "litigious" & "uncertainty" nuances
relevant_sentiments <- sentiment_scores_long %>%
  filter(sentiment_type %in% c("uncertainty", "litigious"))
# Average Score computation
yearly_sentiment <- relevant_sentiments %>%
  group_by(year, sentiment_type, company) %>%
  summarise(average_score = mean(score, na.rm = TRUE), .groups = 'drop')
ggplot(yearly_sentiment, aes(x = year, y = average_score, color = sentiment_type, group = interaction(sentiment_type, company))) +
  geom_line() +
  geom_point() +  
  facet_wrap(~company) +  
  labs(title = "Uncertainty & Litigious Sentiment Scores per Company and Year",
       x = "Year",
       y = "Average Year Score") +
  scale_color_brewer(palette = "Set1") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 10))


# Focus on: Covid19
# Filter for 2020 and corrisponding Q1, Q2, Q3, Q4
sentiment_scores_2020_quarters <- sentiment_score %>%
  filter(year == "20" & quarter %in% c("Q1", "Q2", "Q3", "Q4")) %>%
  select(sentiment, company, year, quarter)
print(sentiment_scores_2020_quarters)
sentiment_scores_2020_quarters$quarter <- factor(sentiment_scores_2020_quarters$quarter, 
                                                 levels = c("Q1", "Q2", "Q3", "Q4"))
# Plot on 2020
ggplot(sentiment_scores_2020_quarters, aes(x = quarter, y = sentiment, group = company, color = company)) +
  geom_line() +  
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Sentiment Scores for Company during 2020",
       x = "Quarter",
       y = "Sentiment Score") +
  scale_color_brewer(palette = "Set1") + 
  theme(legend.title = element_blank(),  
        legend.position = "right")  