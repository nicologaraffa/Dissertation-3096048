# This script aims to identify the most relevant tokens and differentiate them by term frequency.
# It then creates the inverse document matrix to weight them by the number of documents in which they
# appear. The script shows a PCA on term frequencies and a comprehensive Tf-Idf plot.

library(quanteda)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(RColorBrewer)

# Descriptive metrics + ABSOLUTE TERM FREQUENCY
# Total number of words in the whole corpus:
total_tokens <- sum(colSums(ect_dfm_nl))
print(total_tokens)

# Total number of words in each document:
total_tokens_per_doc <- rowSums(ect_dfm_nl)
print(total_tokens_per_doc)

# Average number of words in each document:
avg_tokens_per_doc <- mean(total_tokens_per_doc)
print(avg_tokens_per_doc)

# Average number of words for each company:
tokens_GS <- total_tokens_per_doc[grep("GS", names(total_tokens_per_doc))]
tokens_UBS <- total_tokens_per_doc[grep("UBS", names(total_tokens_per_doc))]
avg_GS <- mean(tokens_GS)
avg_UBS <- mean(tokens_UBS)
print(avg_GS)
print(avg_UBS)

# Unique tokens in corpus
num_unique_tokens <- ncol(ect_dfm_nl)
print(num_unique_tokens)

# Average number of unique words in each document:
unique_tokens_per_doc <- apply(ect_dfm_nl, 1, function(x) sum(x > 0))
avg_unique_tokens_per_doc <- mean(unique_tokens_per_doc)
print(avg_unique_tokens_per_doc)

# Average number of unique words for each company:
unique_tokens_GS <- unique_tokens_per_doc[grep("GS", names(unique_tokens_per_doc))]
unique_tokens_UBS <- unique_tokens_per_doc[grep("UBS", names(unique_tokens_per_doc))]
unique_avg_GS <- mean(unique_tokens_GS)
unique_avg_UBS <- mean(unique_tokens_UBS)
print(unique_avg_GS)
print(unique_avg_UBS)

# Calculate the absolute term frequency
tf_abs <- colSums(ect_dfm_nl)

# Top 10 terms for absolute frequency
tf_abs_sorted <- sort(tf_abs, decreasing = TRUE)
top_10_terms <- head(tf_abs_sorted, 10)

# Bottom 10 terms for absolute frequency
tf_abs_sorted <- sort(tf_abs, decreasing = FALSE)
bottom_10_terms <- head(tf_abs_sorted, 10)

# Scatter plot for the absolute term frequencies
ggplot(tf_df, aes(x = reorder(term, Absolute_Frequency), y = Absolute_Frequency)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Term", y = "Absolute Frequency", title = "Absolute Term Frequency in the Corpus") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))


# Relative Frequency

# Computing TF -> Occurrences per Doc / Total tokens in the Doc
tf_relative <- ect_dfm_nl / matrix(total_tokens_per_doc, nrow = nrow(ect_dfm_nl), ncol = ncol(ect_dfm_nl), byrow = TRUE)

# Arrange TF for visualization
tf_matrix <- as.matrix(tf_relative)
tf_long <- as.data.frame(tf_matrix) %>% 
  tibble::rownames_to_column('document') %>%
  pivot_longer(-document, names_to = 'term', values_to = 'frequency')
# Import metadata from doc_id
tf_long <- tf_long %>%
  mutate(
    company = str_extract(document, "^[^_]*"),  
    year = str_extract(document, "\\d{2}(?=\\.)") )

# Average year TF for company
avg_freq_by_company <- tf_long %>%
  group_by(company, term) %>%
  summarise(average_frequency = mean(frequency, na.rm = TRUE)) %>%
  ungroup()

# 10 Most Relative Frequency tokens, per company
top_tokens_by_company <- avg_freq_by_company %>%
  arrange(company, -average_frequency) %>%
  group_by(company) %>%
  slice_max(average_frequency, n = 10) %>%
  ungroup()
print(top_tokens_by_company)

# Lineplot with average year TF (avg of Q1, Q2, Q3, Q4) for all tokens
ggplot(tf_yearly_general_avg, aes(x = as.numeric(year), y = average_frequency, color = company)) +
  geom_point(size = 4) +  
  geom_line(aes(group = company), alpha = 0.5) +  
  theme_minimal() +
  labs(x = "Year", y = "Average Relative Frequency", title = "General Average Term Frequency by Year and Company") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")  

# TF Heatmap for all tokens, by company, over years
tf_yearly_general_avg <- tf_long %>%
  group_by(year, company) %>%
  summarise(average_frequency = mean(frequency, na.rm = TRUE), .groups = "drop")
ggplot(tf_yearly_general_avg, aes(x = company, y = as.factor(year), fill = average_frequency)) +
  geom_tile() +  
  scale_fill_gradientn(colors = brewer.pal(9, "Blues")) + 
  theme_minimal() +
  labs(x = "Company", y = "Year", fill = "Avg Relative\nFrequency", title = "General Average Term Frequency by Year and Company") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# PCA
pca_result <- prcomp(tf_matrix, scale. = TRUE)

# Dataframe for visualization
pca_df <- as.data.frame(pca_result$x) %>%
  mutate(
    document = rownames(.),
    company = str_extract(document, "^[^_]*") 
  )
# PCA scatterplot
ggplot(pca_df, aes(x = PC1, y = PC2, color = company)) +
  geom_point(alpha = 0.6) +  
  theme_minimal() +
  labs(title = "PCA of Term Frequencies by Company", x = "First Principal Component (PC1)", y = "Second Principal Component (PC2)") +
  scale_color_manual(values = c("GS" = "blue", "UBS" = "red"))



# IDF
# Inverse Document Frequency computation for tokens
document_count <- nrow(ect_dfm_uni)
term_document_occurrences <- colSums(ect_dfm_nl > 0) 
idf_values <- log(document_count / term_document_occurrences)
idf_df <- data.frame(term = names(idf_values), IDF = idf_values)

# Top & Bottom terms visualization
top_idf <- head(idf_df[order(-idf_df$IDF), ], 20)  
bottom_idf <- head(idf_df[order(idf_df$IDF), ], 20) 
cat("Top 20 Terms with Highest IDF:\n")
print(top_idf)
cat("Top 20 Terms with Lowest IDF:\n")
print(bottom_idf)

# TF & IDF combined
# Merge
tf_idf_df <- merge(tf_df, idf_df, by = "term")

highlight_data <- subset(tf_idf_df, Relative_Frequency > quantile(tf_idf_df$Relative_Frequency, 0.95) | IDF > quantile(tf_idf_df$IDF, 0.95))
ggplot(tf_idf_df, aes(x = Relative_Frequency, y = IDF)) +
  geom_point(alpha = 0.5) +
  geom_text(data = highlight_data, aes(label = term), check_overlap = TRUE, size = 3, vjust = 1.5, hjust = 1.5) +
  theme_minimal() +
  labs(title = "TF vs. IDF for All Terms", x = "Term Frequency (TF)", y = "Inverse Document Frequency (IDF)") +
  theme(plot.title = element_text(hjust = 0.5))

# Tf-Idf computation
tf_idf_df$TF_IDF <- tf_idf_df$Relative_Frequency * tf_idf_df$IDF
# Order for Tf-Idf
tf_idf_df <- tf_idf_df[order(-tf_idf_df$TF_IDF), ]