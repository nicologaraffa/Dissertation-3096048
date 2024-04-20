# This script creates the unigrams' DFM. A DFM contains lemmatized tokens
# and another non lemmatized ones The script then shows four
# comprehensive graphical visualizations.

library(quanteda)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(quanteda.textstats)

# DFM creation
ect_dfm_uni <- dfm(tokens_l) 
ect_dfm_nl <- dfm(tokens)

# Trim DFM: removal of tokens in less than 5% or more than 85% of documents.
ect_dfm_uni <- dfm_trim(ect_dfm_uni, min_docfreq = 0.05, max_docfreq = 0.85, docfreq_type = "prop")
ect_dfm_nl <- dfm_trim(ect_dfm_nl, min_docfreq = 0.05, max_docfreq = 0.85, docfreq_type = "prop")

# Wordcloud top 100 tokens for lemmatized version
top_features_uni <- topfeatures(ect_dfm_uni, n = 100)
set.seed(123) 
wordcloud(names(top_features_uni), top_features_uni, max.words = 100, 
          min.freq = 2, 
          scale = c(1.5, 0.5),
          colors = brewer.pal(8, "Dark2"), 
          random.order = FALSE,  
          rot.per = 0.20) 

# Frequency visualization of unigrams by year
ECT_freq_yt <- textstat_frequency(ect_dfm_uni, n = 5, groups = year)
ggplot(ECT_freq_yt, aes(x = nrow(ECT_freq_yt):1, y = frequency)) +
  geom_point() +
  geom_line(color = "darkgreen") + 
  facet_wrap(~group, scales = "free_y") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(ECT_freq_yt):1, labels = ECT_freq_yt$feature) +
  scale_y_continuous(breaks = seq(0, max(ECT_freq_yt$frequency), 10)) + 
  labs(x = NULL, y = "Absolute Frequency", title = "Unigram Frequency per Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 5),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 10))

# Frequency visualization of unigrams by company
ECT_freq_ct <- textstat_frequency(ect_dfm_uni, n = 20, groups = company)
ggplot(data = ECT_freq_ct, 
       aes(x = nrow(ECT_freq_ct):1, y = frequency)) +
  geom_point() +
  geom_line(color = "darkgreen") + 
  facet_wrap(~group, scales = "free_y") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(ECT_freq_ct):1, labels = ECT_freq_ct$feature) +
  scale_y_continuous(breaks = seq(0, max(ECT_freq_ct$frequency), 10)) + 
  labs(x = NULL, y = "Absolute Frequency", title = "Unigram Frequency per Company") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 5),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 10))

# Frequency visualization of unigrams by year & company
ECT_freq_yct = textstat_frequency(ect_dfm_uni, n = 5, groups = interaction(year, company))
ggplot(data = ECT_freq_yct, 
       aes(x = nrow(ECT_freq_yct):1, y = frequency)) +
  geom_point() +
  geom_line(color = "darkgreen") + 
  facet_wrap(~group, scales = "free_y", ncol = 2) + 
  coord_flip() +
  scale_x_continuous(breaks = nrow(ECT_freq_yct):1,
                     labels = ECT_freq_yct$feature) +
  scale_y_continuous(breaks = seq(0, max(ECT_freq_yct$frequency), by = 10)) + 
  labs(x = NULL, y = "Absolute Frequency", title = "Unigram Frequency by Year and Company") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 5),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 10))
