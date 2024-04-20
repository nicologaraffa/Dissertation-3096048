# This script creates the bigrams' DFM. The DFM contains common bigrams (non lemmatized)
# The script then plots four comprehensive graphical visualizations, plus a wordcloud.

library(quanteda)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(stringr)
library(quanteda.textstats)
library(RColorBrewer)

# Generate N-grams
bigrams <- tokens_ngrams(tokens, n = 2)
ect_dfm_bi <- dfm(bigrams)
ect_dfm_bi <- dfm_trim(ect_dfm_bi, min_docfreq = 0.05, max_docfreq = 0.85, docfreq_type = "prop")

# Wordcloud top 100 bigrams for unlemmatized version
top_features_bi <- topfeatures(ect_dfm_bi, n = 100)
set.seed(123) 
wordcloud(names(top_features_bi), top_features_bi, max.words = 100, min.freq = 2,
          scale = c(1.5, 0.5), colors = brewer.pal(8, "Dark2"), 
          random.order = FALSE, rot.per = 0.15) 

# Frequency visualization of bigrams by year
ECT_freq_y <- textstat_frequency(ect_dfm_bi, n = 5, groups = year)
ggplot(ECT_freq_y, aes(x = nrow(ECT_freq_y):1, y = frequency)) +
  geom_point() +
  geom_line(color = "blue") + 
  facet_wrap(~group, scales = "free_y") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(ECT_freq_y):1, labels = ECT_freq_y$feature) +
  scale_y_continuous(breaks = seq(0, max(ECT_freq_y$frequency), 10)) + 
  labs(x = NULL, y = "Absolute Freq.", title = "Bigram Frequency per Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 12))

# Frequency visualization of bigrams by company
ECT_freq_c <- textstat_frequency(ect_dfm_bi, n = 20, groups = company)
ggplot(data = ECT_freq_c, 
       aes(x = nrow(ECT_freq_c):1, y = frequency)) +
  geom_point() +
  geom_line(color = "blue") + 
  facet_wrap(~group, scales = "free_y") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(ECT_freq_c):1, labels = ECT_freq_c$feature) +
  scale_y_continuous(breaks = seq(0, max(ECT_freq_c$frequency), 10)) + 
  labs(x = NULL, y = "Absolute Freq.",title = "Bigram Frequency per Company") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 12))

# Frequency visualization of bigrams by year and company
ECT_freq_yc = textstat_frequency(ect_dfm_bi, n = 5, groups = interaction(year, company))
ggplot(data = ECT_freq_yc, 
       aes(x = nrow(ECT_freq_yc):1, y = frequency)) +
  geom_point() +
  geom_line(color = "blue") + 
  facet_wrap(~group, scales = "free_y", ncol = 2) + 
  coord_flip() +
  scale_x_continuous(breaks = nrow(ECT_freq_yc):1,
                     labels = ECT_freq_yc$feature) +
  scale_y_continuous(breaks = seq(0, max(ECT_freq_yc$frequency), by = 10)) + 
  labs(x = NULL, y = "Absolute Freq.", title = "Bigram Frequency by Year and Company") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 12))