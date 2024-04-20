# This script performs a first lexicon-based analysis on the corpus by loading
# the Lexicoder Policy Agenda dictionary. It will capture topic variations across
# years, company and quarters.

library(tidytext)
library(textdata)
library(readtext)
library(readr)
library(dplyr)
library(quanteda)
library(ggplot2)
library(data.table)
library(corrplot)
library(stringr)

setwd("/Users/nicologaraffa/Documents/R Dissertation WD/Working Directory")

# Load English policy agendas dictionary
LTD_topics = dictionary(file = "Dictionaries/policy_agendas_english.lcd")
names(LTD_topics)
sapply(LTD_topics, length)

# Apply dictionary to the DFM of non-lemmatized unigrams.
ECT_dict = dfm_lookup(ect_dfm_nl, dictionary = LTD_topics)

# Focus only on topic-of-interest.
topics = c("year", "macroeconomics", "finance", "civil_rights", "healthcare",
           "labour", "environment", "energy", "defence", "intl_affairs",
           "immigration")

# Group by year to discover topic trends across years.
ECT_dict_y = dfm_group(ECT_dict, groups = year)
ECT_dict_dt_y = convert(ECT_dict_y, to = "data.frame")
setDT(ECT_dict_dt_y)
setnames(ECT_dict_dt_y, "doc_id", "year")

prop_topics_y = melt(ECT_dict_dt_y[, mget(topics)], 
                     id.vars = "year", 
                     variable.name = "topic", 
                     value.name = "weight")
setDT(prop_topics_y)

# Calculate the proportion of the weight of each topic per year
prop_topics_y[, weight_prop := weight / sum(weight), by = .(year)]
prop_topics_y[, topic := as.factor(topic)]

order_topics <- c("macroeconomics", "civil_rights", "healthcare", 
                  "labour", "immigration", "environment", 
                  "energy", "finance", "defence", "intl_affairs")
prop_topics_y$topic <- factor(prop_topics_y$topic, levels = order_topics)

# Plot visualizations
ggplot(prop_topics_y, 
       aes(year, weight_prop, color = topic, fill = topic)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Topic Distribution per Year") +
  xlab("Year") +
  ylab("Topic Proportion (%)") +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90, vjust = .2, size = 6),
        axis.ticks.x = element_blank())
  
# Distribution by company

# Group by company
ECT_dict_c = dfm_group(ECT_dict, groups = company)
ECT_dict_dt_c = convert(ECT_dict_c, to = "data.frame")
setDT(ECT_dict_dt_c)
setnames(ECT_dict_dt_c, "doc_id", "company")

prop_topics_c <- melt(ECT_dict_dt_c, id.vars = "company", variable.name = "topic", value.name = "weight")

# Filter for relevant topics (only once)
prop_topics_c <- prop_topics_c[prop_topics_c$topic %in% topics, ]

# Convert to data.table for further operations
setDT(prop_topics_c)

# Calculate proportions
prop_topics_c[, weight_prop_c := weight / sum(weight), by = company]
prop_topics_c[, topic := as.factor(topic)]

# Plot distribution of topics for each company
ggplot(prop_topics_c, 
       aes(company, weight_prop_c, color = topic, fill = topic)) +
  geom_bar(stat = "identity") +
  ggtitle("Topic Distribution per Company") +
  xlab("Company") +
  ylab("Topic Proportion (%)") +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90, vjust = .2, size = 6),
        axis.ticks.x = element_blank()) 


# Distribution by quarter

# Group by quarter
ECT_dict_q = dfm_group(ECT_dict, groups = quarter)
ECT_dict_dt_q = convert(ECT_dict_q, to = "data.frame")
setDT(ECT_dict_dt_q)
setnames(ECT_dict_dt_q, "doc_id", "quarter")

prop_topics_q = melt(ECT_dict_dt_q, id.vars = "quarter", variable.name = "topic", value.name = "weight")
prop_topics_q <- prop_topics_q[prop_topics_q$topic %in% topics, ]
# Convert ECT_dict_dt_q extracted data to a data.table
setDT(prop_topics_q)
# Calculate the proportion of the weight of each topic per quarter
prop_topics_q[, weight_prop := weight / sum(weight), by = .(quarter)]
prop_topics_q[, topic := as.factor(topic)]

# Plot distribution of topics for each quarter
ggplot(prop_topics_q, 
       aes(quarter, weight_prop, color = topic, fill = topic)) +
  geom_bar(stat = "identity") +
  ggtitle("Topic Distribution per Quarter") +
  xlab("Quarter") +
  ylab("Topic Proportion (%)") +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90, vjust = .2, size = 6),
        axis.ticks.x = element_blank())


# Distribution by company and years

ECT_dict_dt <- convert(ECT_dict, to = "data.frame")
# Company ands year extraction from doc_id
ECT_dict_dt <- ECT_dict_dt %>%
  mutate(
    company = str_extract(doc_id, "^[^_]*"),  
    year = str_extract(doc_id, "\\d{2}(?=\\.)") )

topics_cy <- c("macroeconomics", "finance", "civil_rights", "healthcare",
            "labour", "environment", "energy", "defence", "intl_affairs",
            "immigration")

ECT_dict_dt <- ECT_dict_dt %>%
  select(doc_id, company, year, all_of(topics_cy))

ECT_dict_long <- pivot_longer(ECT_dict_dt, cols = all_of(topics_cy), names_to = "topic", values_to = "weight")
# Conversion of 'year' in numeric
ECT_dict_long$year <- as.numeric(ECT_dict_long$year)

# Topic weight for year and company
total_weight_by_company_year <- ECT_dict_long %>%
  group_by(company, year) %>%
  summarise(total_weight = sum(weight, na.rm = TRUE))  # weighted sum for each topic

total_weight_by_company_year$company <- as.character(total_weight_by_company_year$company)
total_weight_by_company_year$year <- as.numeric(total_weight_by_company_year$year)

# Join for final visualization
ECT_dict_prop <- ECT_dict_long %>%
  left_join(total_weight_by_company_year, by = c("company", "year")) %>%
  mutate(proportion = weight / total_weight)
ECT_dict_prop$topic <- factor(ECT_dict_prop$topic, levels = order_topics)

# Plot visualization
ggplot(ECT_dict_prop, aes(x = year, y = proportion, fill = topic)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~company, scales = 'free_y', ncol = 1) +
  theme_minimal() +
  labs(x = "Year", y = "Topic Proportion (%)", title = "Topic Distribution per Company Over Years") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
        axis.ticks.x = element_blank())