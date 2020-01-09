

# Import dependencies and read config ----
options(max.print = 1e3, scipen = 999, width = 1e2)
options(stringsAsFactors = F)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(magrittr)
  
  library(lubridate)
  
  library(quanteda)
})

secrets <- config::get(file = "secrets.yml")



# Load dataset ----
donald_tweets_url <- sprintf("https://datainstinct.blob.core.windows.net/twitter/realdonaldtrump.csv?%s", secrets$azure_storage_key)

donald_tweets <- fread(donald_tweets_url, quote = "")

donald_tweets %<>% 
  filter(!is_retweet) %>% 
  mutate(
    id = as.character(id_str),
    
    # dates processing
    created_at = mdy_hms(created_at),
    created_date = as.Date(created_at),
    
    # text processing
    text = str_replace_all(text, '\n', " || "),
    text = str_replace_all(text, "&amp;", "and"),
    text = str_replace_all(text, '"', ""),
    text = str_replace_all(text, "http(s)://t.co/[A-Za-z\\d]+", "<url>"),
  ) %>% 
  select(-id_str)


donald_tweets %>% 
  select(created_at, text, retweet_count, favorite_count) %>% 
  arrange(desc(created_at)) %>% 
  as_tibble()



# Tokenize words ----
tweets_t <- tokens(donald_tweets$text,
                   remove_separators = T,
                   remove_symbols = T,
                   remove_punct = T,
                   remove_url = T,
                   remove_hyphens = T,
                   remove_numbers = T) %>% 
  tokens_ngrams(n = 1:3, concatenator = " ")


print(tweets_t %>% tail)



# Topics network ----
tweets_dfm <- dfm(tweets_t, 
                  tolower = T, stem = T,
                  remove = stopwords("en"))

print(tweets_dfm)

tweets_dfm %>% 
  textplot_wordcloud(max_words = 200) 


## @user network
users_dfm <- dfm_select(tweets_dfm, pattern = "@*")
top_users <- names(topfeatures(users_dfm, 20))
top_users %>% head

users_fcm <- dfm_select(tweets_dfm, pattern = top_users) %>% fcm()
users_fcm %>% head

textplot_network(users_fcm, 
                 min_freq = .1,
                 edge_alpha = .25,
                 edge_size = 5)


## #hashtags network
hashtags_dfm <- dfm_select(tweets_dfm, pattern = "#*")


top_hashtags <- names(topfeatures(hashtags_dfm, 20))
top_hashtags %>% head


hashtags_fcm <- dfm_select(tweets_dfm, pattern = top_hashtags) %>% fcm()
hashtags_fcm %>% head


textplot_network(hashtags_fcm, 
                 min_freq = .1,
                 edge_alpha = .25,
                 edge_size = 5)


# Calc Td-Idf ----
tweets_dfm_w <- dfm_tfidf(tweets_dfm)
tweets_dfm_w %>% head


# Sentiment analysis ----
tweets_dict <- tokens_lookup(tweets_t, dictionary = data_dictionary_LSD2015)
print(tweets_dict %>% tail)

tweets_sa <- dfm(tweets_dict, 
                 tolower = T, stem = T,
                 remove = stopwords("en"))
print(tweets_sa)


