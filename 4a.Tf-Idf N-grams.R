# This script aims to identify the most relevant bigrams and differentiate them by term frequency.
# It then creates the inverse document matrix to weight them by the number of documents in which they
# appear. The script shows a PCA on bigram frequencies and a comprehensive Tf-Idf plot.

library(quanteda)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(scales)
library(RColorBrewer)

# Descriptive metrics + ABSOLUTE TERM FREQUENCY

# Total number of bigrams in the whole corpus:
total_bigrams <- sum(colSums(ect_dfm_bi))
print(total_bigrams)

# Total number of bigrams in each document:
total_bigrams_per_doc <- rowSums(ect_dfm_bi)
print(total_bigrams_per_doc)

# Average number of bigrams in each document:
avg_bigrams_per_doc <- mean(total_bigrams_per_doc)
print(avg_bigrams_per_doc)

# Average number of bigrams for each company:
bigrams_GS <- total_bigrams_per_doc[grep("GS", names(total_bigrams_per_doc))]
bigrams_UBS <- total_bigrams_per_doc[grep("UBS", names(total_bigrams_per_doc))]
avg_bigrams_GS <- mean(bigrams_GS)
avg_bigrams_UBS <- mean(bigrams_UBS)
print(avg_bigrams_GS)
print(avg_bigrams_UBS)

# Unique bigrams in corpus
num_unique_bigrams <- ncol(ect_dfm_bi)
print(num_unique_bigrams)

# Average number of unique bigrams in each document:
unique_bigrams_per_doc <- apply(ect_dfm_bi, 1, function(x) sum(x > 0))
avg_unique_bigrams_per_doc <- mean(unique_bigrams_per_doc)
print(avg_unique_bigrams_per_doc)

# Average number of unique bigrams for each company:
unique_bigrams_GS <- unique_bigrams_per_doc[grep("GS", names(unique_bigrams_per_doc))]
unique_bigrams_UBS <- unique_bigrams_per_doc[grep("UBS", names(unique_bigrams_per_doc))]
unique_avg_bigrams_GS <- mean(unique_bigrams_GS)
unique_avg_bigrams_UBS <- mean(unique_bigrams_UBS)
print(unique_avg_bigrams_GS)
print(unique_avg_bigrams_UBS)

# Calculate the absolute term frequency
tf_abs_bi <- colSums(ect_dfm_bi)

# Obtain the top 10 bigrams for absolute frequency
tf_abs_bi_sorted <- sort(tf_abs_bi, decreasing = TRUE)
top_10_bigrams <- head(tf_abs_bi_sorted, 10)

# Obtain the bottom 10 bigrams for absolute frequency
tf_abs_bi_sorted <- sort(tf_abs_bi, decreasing = FALSE)
bottom_10_bigrams <- head(tf_abs_bi_sorted, 10)

# Scatter plot for absolute bigram frequencies
ggplot(tf_df_bi, aes(x = reorder(term, Absolute_Frequency), y = Absolute_Frequency)) +
  geom_point() + 
  theme_minimal() +
  labs(x = "Bigram", y = "Absolute Frequency", title = "Absolute Bigram Frequency in the Corpus") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))  



# Relative Frequency

# Computing TF -> Occurrences per Doc / Total bigrams in the Doc
tf_relative_bi <- ect_dfm_bi / matrix(total_bigrams_per_doc, nrow = nrow(ect_dfm_bi), ncol = ncol(ect_dfm_bi), byrow = TRUE)

# Arrange TF for visualization
tf_matrix_bi <- as.matrix(tf_relative_bi)
tf_long_bi <- as.data.frame(tf_matrix_bi) %>% 
  tibble::rownames_to_column('document') %>%
  pivot_longer(-document, names_to = 'term', values_to = 'frequency')
# Import metadata from doc_id
tf_long_bi <- tf_long_bi %>%
  mutate(
    company = str_extract(document, "^[^_]*"),  
    year = str_extract(document, "\\d{2}(?=\\.)") )

# Average year TF for company
max_freq_by_company_bi <- tf_long_bi %>%
  group_by(company, term) %>%
  summarise(max_frequency = max(frequency, na.rm = TRUE)) %>%
  ungroup()
# 10 Most frequent bigrams, per company
top_bigram_by_company <- max_freq_by_company_bi %>%
  arrange(company, -max_frequency) %>%
  group_by(company) %>%
  slice_max(max_frequency, n = 10) %>%
  ungroup()
print(top_bigram_by_company)

# Lineplot with average year TF (avg of Q1, Q2, Q3, Q4) for all bigrams
ggplot(tf_yearly_general_avg_bi, aes(x = as.numeric(year), y = average_frequency, color = company)) +
  geom_point(size = 4) +  
  geom_line(aes(group = company), alpha = 0.5) +  
  theme_minimal() +
  labs(x = "Year", y = "Average Relative Frequency", title = "General Average Bigram Frequency by Year and Company") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")  

# TF Heatmap for all bigrams, by company, over years
tf_yearly_general_avg_bi <- tf_long_bi %>%
  group_by(year, company) %>%
  summarise(average_frequency = mean(frequency, na.rm = TRUE), .groups = "drop")
ggplot(tf_yearly_general_avg_bi, aes(x = company, y = as.factor(year), fill = average_frequency)) +
  geom_tile() +  
  scale_fill_gradientn(colors = brewer.pal(9, "Blues")) +  
  theme_minimal() +
  labs(x = "Company", y = "Year", fill = "Avg Relative\nFrequency", title = "General Average Bigram Frequency by Year and Company") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 




# PCA
pca_result_bi <- prcomp(tf_matrix_bi, scale. = TRUE)

# Dataframe for visualization
pca_df_bi <- as.data.frame(pca_result_bi$x) %>%
  mutate(
    document = rownames(.),
    company = str_extract(document, "^[^_]*")
  )
# PCA scatterplot
ggplot(pca_df_bi, aes(x = PC1, y = PC2, color = company)) +
  geom_point(alpha = 0.6) + 
  theme_minimal() +
  labs(title = "PCA of Bigram Frequencies by Company", x = "First Principal Component (PC1)", y = "Second Principal Component (PC2)") +
  scale_color_manual(values = c("GS" = "blue", "UBS" = "red"))


# IDF
# Inverse Document Frequency computation for bigrams
document_count_bi <- nrow(ect_dfm_bi) 
term_document_occurrences_bi <- colSums(ect_dfm_bi > 0) 
idf_values_bi <- log(document_count_bi / term_document_occurrences_bi)
idf_df_bi <- data.frame(term = names(idf_values_bi), IDF = idf_values_bi)

# Top & Bottom bigrams visualization
top_idf_bi <- head(idf_df_bi[order(-idf_df_bi$IDF), ], 20)
bottom_idf_bi <- head(idf_df_bi[order(idf_df_bi$IDF), ], 20)
cat("Top 20 Bigrams with Highest IDF:\n")
print(top_idf_bi)
cat("Top 20 Bigrams with Lowest IDF:\n")
print(bottom_idf_bi)

# TF & IDF combined
# Merge
tf_idf_df_bi <- merge(tf_df_bi, idf_df_bi, by = "term")

highlight_data_bi <- subset(tf_idf_df_bi, Relative_Frequency > quantile(tf_idf_df_bi$Relative_Frequency, 0.95) | IDF > quantile(tf_idf_df_bi$IDF, 0.95))
ggplot(tf_idf_df_bi, aes(x = Relative_Frequency, y = IDF)) +
  geom_point(alpha = 0.5) +
  geom_text(data = highlight_data_bi, aes(label = term), check_overlap = TRUE, size = 3, vjust = 1.5, hjust = 1.5) +
  theme_minimal() +
  labs(title = "TF vs. IDF for All Bigrams", x = "Bigram Frequency (TF)", y = "Inverse Document Frequency (IDF)") +
  theme(plot.title = element_text(hjust = 0.5))

# Tf-Idf computation
tf_idf_df_bi$TF_IDF <- tf_idf_df_bi$Relative_Frequency * tf_idf_df_bi$IDF
# Order for Tf-Idf
tf_idf_df_bi <- tf_idf_df_bi[order(-tf_idf_df_bi$TF_IDF), ]