# The following script aims to analyze the readibility aspect of transcripts.
# The textstat:readibility function, built in quanteda, works on unprocessed corpora,
# thus making no differences between uni- or bigrams. This way, it is able to catch
# minor nuances in complex texts.

library(quanteda)
library(quanteda.textstats)

meanSentenceLength <- textstat_readability(ECTcorpus,
                            measure = "meanSentenceLength",
                            remove_hyphens = TRUE,
                            min_sentence_length = 1,
                            max_sentence_length = 10000,
                            intermediate = FALSE
)
Flesch <- textstat_readability(ECTcorpus_clean,
                            measure = "Flesch",
                            remove_hyphens = TRUE,
                            min_sentence_length = 1,
                            max_sentence_length = 10000,
                            intermediate = FALSE
)
ARI <- textstat_readability(ECTcorpus_clean,
                            measure = "ARI",
                            remove_hyphens = TRUE,
                            min_sentence_length = 1,
                            max_sentence_length = 10000,
                            intermediate = FALSE
)


# Plot visualizations
ggplot(data = Readibility_scores, aes(x = document, y = MSL)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Mean Sentence Length (MSL) Trend", x = "Doc", y = "MSL")

ggplot(data = Readibility_scores, aes(x = document, y = Flesch)) +
  geom_bar(stat = "identity", fill = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Flesch Reading Ease Trend", x = "Doc", y = "Flesch Score")

ggplot(data = Readibility_scores, aes(x = document, y = ARI)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Automated Readability Index Trend", x = "Doc", y = "ARI")