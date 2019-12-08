
#'
#' Train NLP model
#'


# Import dependencies ----

suppressPackageStartupMessages({
  library(h2o)
  
  library(dplyr)
  library(data.table)
  library(tidyr)
  library(purrr)
  library(magrittr)
  
  library(stringr)
  library(tidytext)
  library(wordcloud)
})


# Load dataset ----
tweets <- readRDS("cache/processed.2019-09-15.rds")



# Prepare dataset ----

tweets %<>% 
  mutate(direction = as.factor(direction)) %>% 
  rename(text = text_total)

# TODO: find code w/ stemming lemmatization etc

# Remove stopwords ----
tweet_words <- tweets %>%
  select(created_date, text) %>% 
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tweet_words



# Stats and word clouds ---- 
tweet_word_counts <- tweet_words[, c("word", "created_date")] %>% count(created_date, word)
tweet_word_counts %>% filter(word != "rt") %>% arrange(-n)
tweet_word_counts %>% filter(word != "rt") %>% count(word) %>% arrange(-n)


total_words <- tweet_word_counts %>% 
  group_by(created_date) %>% 
  summarize(total = sum(n))

total_words %>% arrange(-total)


tweet_words %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, color = "gray"))



# Compute TD-IDF ---- 
tweet_word_counts <- tweet_word_counts %>% 
  left_join(total_words, by = "created_date") %>%
  bind_tf_idf(word, created_date, n)

tweet_word_counts %>% 
  group_by(created_date) %>% 
  filter(tf_idf == max(tf_idf)) %>% 
  arrange(desc(created_date))




# Calc Document-Term Matrix ---- 
dtm <- cast_sparse_(tweet_word_counts, "created_date", "word", "tf_idf")

dim(dtm)
dtm[1:10, 1:10]




# Init H2O cluster ----

h2o.init(nthreads = -1, # use all cores
         max_mem_size = "64G") # max mem size



# Prepare data ----
data <- h2o.cbind(tweets %>% select(-text) %>% as.h2o,
                  dtm %>% as.h2o)

stopifnot(
  nrow(data) > 0,
  is.factor(data$direction)
)

data %>% as_tibble


# Set label and features
data$direction
features <- setdiff(names(data), "direction")
features %>% head(50)



# Split ----
data_split <- h2o.splitFrame(data, ratios = .8, seed = 314)

train_dt <- data_split[[1]]
test_dt <- data_split[[2]]



# Train model ----
auto <- h2o.automl(x = features, y = "direction",
                   
                   training_frame = train_dt,
                   leaderboard_frame = test_dt,
                  
                   include_algos = c("GBM", "DeepLearning", "StackedEnsemble"),
                   
                   nfolds = 6,
                   max_runtime_secs = 60*10,
                   
                   seed = 314) # duration of searching


# view leaderboard
auto@leaderboard


# view best model
best <- auto@leader
best

# plot var importance
h2o.varimp_plot(best, num_of_features = 10)




# Word2vec ----

# Pretrained model http://s3.amazonaws.com/tomk/h2o-world/megan/w2v.hex
w2v_model <- h2o.loadModel("cache/w2v.hex")

# see continue https://github.com/h2oai/h2o-tutorials/blob/master/h2o-world-2017/nlp/AmazonReviews.ipynb

