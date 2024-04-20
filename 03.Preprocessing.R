# This script is designed for preprocessing earnings call transcripts.
# It imports metadata from file names, define irrelevant terms to remove,
# applies cleaning and eventually build up a corpus.

library(quanteda)
library(readtext)
library(stringr)

setwd("/Users/nicologaraffa/Documents/R Dissertation WD/Working Directory")

ECT <- readtext("ECT",
                docvarsfrom = "filenames",
                docvarnames = c("company", "quarter", "year"),
                dvsep = "_",
                encoding = "UTF-8")

# Remove apostrophes and keep "&"
text <- str_replace_all(ECT$text, c("['’]" = "", "S&P" = "sp", "M&A" = "ma","P&C" = "pc", "P&L" = "pl"))
ECT$text <- text
# Create a corpus
ECTcorpus <- corpus(ECT, text_field = "text")

# Tokenization and cleaning
tokens <- tokens(ECTcorpus, remove_punct = TRUE, remove_numbers = TRUE,
                 remove_symbols = TRUE, remove_url = TRUE, split_hyphens = TRUE,
                 include_docvars = TRUE)
tokens <- tokens_tolower(tokens)

# Reinsert relevant terms containing "&"
tokens <- tokens_replace(tokens, "sp", "S&P", valuetype = "fixed")
tokens <- tokens_replace(tokens, "ma", "M&A", valuetype = "fixed")
tokens <- tokens_replace(tokens, "pc", "P&C", valuetype = "fixed")
tokens <- tokens_replace(tokens, "pl", "P&L", valuetype = "fixed")

# Manual definition of non significant words, names and surnames to trim
extra_words <- c(
  # Prepositions and verbs
  "#", "across", "ago", "ahead", "also", "am", "are", "basically", "be", "been", "being", "bit", "can", "cetera", "could", "do", "et","fully",
  "get", "going", "go", "have", "hello", "hey", "hi", "it", "is", "just", "l", "like", "look", "mainly", "make", "much", "next", "net", "ok",
  "okay", "our", "partially", "per", "please", "pm", "quite", "see", "seen", "since", "take", "thank", "thanks", "that", "think", "third",
  "though", "too", "turn", "us", "versus", "walk", "wanted", "we", "welcome", "well", "will", "yeah", "yes",
  
  # Nouns
  "1q", "2q", "3q", "4q", "q1", "q1", "q1", "q4", "banking", "billion", "business", "ceo", "cfo", "chf", "chief", "couple", "day", "dollar",
  "evening", "everyone", "executive", "first", "fx", "goldman", "good", "group", "guys", "half", "ib", "last", "line", "million", "money",
  "morning", "now", "officer", "one",  "operator", "page", "phone", "president", "quarter", "question", "questions", "revenue",
  "revenues", "sachs", "slide", "speaker", "stage", "thing", "time", "trillion", "two", "ubs", "usd", "vice", "year", "years",
  
  # Personal names
  "adam", "al", "alastair", "alevizakos", "amit", "adam", "andrew", "andy", "andy", "anke", "benjamin", "bernstein", "betsy",
  "brennan", "brian", "caroline", "cassidy", "chris", "chris", "christian", "christians", "christian", "christopher", "daniel",
  "dane", "dan", "david", "davids", "denis", "eric", "glenn", "guy", "harvey", "heather", "iqbal", "james", "jeff", "jeremy",
  "jernej", "jim", "john", "jon", "kian", "kirt", "magda", "magdalena", "marty", "marcus", "martin", "matt", "michael", "mike", "nicholas",
  "nicolas", "patrick", "piers", "ralph", "ralph", "ryan", "sarah", "sarah", "sergio", "stefan", "stephen", "steven", "todd", "tom",
  
  # Surnames
  "abouhossein", "alastair", "alevizakos", "anke", "bolu", "burnell", "carrier", "cheuvreux", "chavez", "chubak", "coleman",
  "coombs", "darn", "devin", "ermotti", "gardner", "goel", "graseck", "grasseck", "hallett", "hamers", "hawken", "hallam", "kleinhanzi",
  "kleinhanzl", "kotowski", "lakhani", "magdalena", "mayo", "mitchell", "mosby", "moszkowski", "moszkowskki", "naratil", "oconnor", "oppenheimer",
  "payen", "poonawala", "scherr", "schorr", "schwartz", "sigee", "solomon", "stalmann", "wasser", "youngwood",
  
  # Compounds
  "aint", "arent", "cant", "couldnt", "dont", "doesnt", "hadnt", "hes", "havent", "hasnt", "id", "ill", "im", "isnt", "ive", "shouldnt",
  "suisses", "theres", "thats", "thats", "theyve", "wont", "were", "weve", "werent", "whove", "youre", "youve"
)

# Remove stopwords and extra words
tokens <- tokens_remove(tokens, stopwords("en"), valuetype = "fixed")
tokens <- tokens_remove(tokens, pattern = extra_words, valuetype = "fixed")

# Lemmatization
tokens_l <- tokens_replace(tokens, 
                           pattern = lexicon::hash_lemmas$token, 
                           replacement = lexicon::hash_lemmas$lemma)