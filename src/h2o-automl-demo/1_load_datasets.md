@realDonaldTrump Tweets for Dow Jones Index Direction Prediction
================
Dmitry Petukhov

**Prepare dataset contains:**

  - **@realDonaldTrump tweets,**

  - **its sentiment scores from Microsoft Cognitive Service**

  - **Dow Jones Index.**

Import dependencies and read config:

``` r
options(max.print = 1e3, scipen = 999, width = 1e2)
options(stringsAsFactors = F)
```

``` r
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(magrittr)
  
  library(stringr)
  library(lubridate)
  
  library(Quandl)
})
```

``` r
secrets <- config::get(file = "secrets.yml")
```

## @realDonaldTrump Tweets

Download
tweets:

``` r
donald_tweets_url <- sprintf("https://datainstinct.blob.core.windows.net/twitter/realdonaldtrump.csv?%s", secrets$azure_storage_key)

donald_tweets <- fread(donald_tweets_url, quote = "")

donald_tweets %<>% 
  mutate(
    id_str = as.character(id_str),
    created_at = mdy_hms(created_at),
    created_date = as.Date(created_at)
  ) %>% 
  mutate(
    text = str_replace_all(text, '[^([:graph:][:space:])]', " | "),
    text = str_replace_all(text, '\"|\n', " || ")
  )

donald_tweets %>% as_tibble()
```

    ## # A tibble: 10,096 x 8
    ##    source  text      created_at          retweet_count favorite_count is_retweet id_str created_date
    ##    <chr>   <chr>     <dttm>                      <int>          <int> <lgl>      <chr>  <date>      
    ##  1 Twitte… TO ALL A… 2017-01-01 05:00:10         32665         126230 FALSE      81542… 2017-01-01  
    ##  2 Twitte… RT @DanS… 2017-01-01 05:39:13          5548              0 TRUE       81543… 2017-01-01  
    ##  3 Twitte… RT @Rein… 2017-01-01 05:43:23          7144              0 TRUE       81543… 2017-01-01  
    ##  4 Twitte… RT @Eric… 2017-01-01 05:44:17          6941              0 TRUE       81543… 2017-01-01  
    ##  5 Twitte… RT @Dona… 2017-01-01 06:49:33          6847              0 TRUE       81544… 2017-01-01  
    ##  6 Twitte… RT @Ivan… 2017-01-01 06:49:49         13659              0 TRUE       81544… 2017-01-01  
    ##  7 Twitte… Well the… 2017-01-02 14:40:10         29248         124024 FALSE      81593… 2017-01-02  
    ##  8 Twitte… Chicago … 2017-01-02 17:31:17         17411          63340 FALSE      81597… 2017-01-02  
    ##  9 Twitte… @CNN jus… 2017-01-02 18:32:29          3948          13862 FALSE      81598… 2017-01-02  
    ## 10 Twitte… Various … 2017-01-02 18:37:10          9057          47285 FALSE      81599… 2017-01-02  
    ## # … with 10,086 more rows

## Tweets sentiment

Get tweets sentiment score:

``` r
source("text_analytics_api.R")

donald_tweets_sentiments <- donald_tweets %>%
  group_by(lubridate::year(created_date), lubridate::month(created_date)) %>% 
  group_map(~ get_sentiment(.x, secrets$cognitive_services_api_key)) %>%
  ungroup() %>% 
  transmute(
    id_str = id,
    sentiment_score = score
  )

donald_tweets_sentiments %>% as_tibble
```

    ## # A tibble: 10,096 x 2
    ##    id_str             sentiment_score
    ##    <chr>                        <dbl>
    ##  1 815422340540547073          0.993 
    ##  2 815432169464197121          0.5   
    ##  3 815433217595547648          0.993 
    ##  4 815433444591304704          0.962 
    ##  5 815449868739211265          0.915 
    ##  6 815449933453127681          0.907 
    ##  7 815930688889352192          0.931 
    ##  8 815973752785793024          0.5   
    ##  9 815989154555297792          0.934 
    ## 10 815990335318982656          0.0372
    ## # … with 10,086 more rows

## Dow Jones index

Get stock data:

``` r
Quandl.api_key(secrets$quandl_api_key)
dji <- Quandl("BCB/7809")

dji %<>% 
  arrange(Date) %>% 
  mutate(diff = Value - lag(Value)) %>% 
  na.omit %>% 
  mutate(
    direction = if_else(diff < 0, 0L, 1L)
  ) %>% 
  transmute(
    date = Date, direction
  )

dji %>% as_tibble
```

    ## # A tibble: 4,937 x 2
    ##    date       direction
    ##    <date>         <int>
    ##  1 2000-01-04         0
    ##  2 2000-01-05         1
    ##  3 2000-01-06         1
    ##  4 2000-01-07         1
    ##  5 2000-01-10         1
    ##  6 2000-01-11         0
    ##  7 2000-01-12         1
    ##  8 2000-01-13         1
    ##  9 2000-01-14         1
    ## 10 2000-01-18         0
    ## # … with 4,927 more rows

## Join datasets

Aggregate tweets (daily):

``` r
donald_tweets_aggr <- donald_tweets %>% 
  filter(!is_retweet) %>% 
  inner_join(donald_tweets_sentiments, by = "id_str") %>% 
  group_by(created_date) %>% 
  summarise(
    n = n(),
    
    sentiment_score_min = min(sentiment_score),
    sentiment_score_mean = mean(sentiment_score),
    sentiment_score_max = max(sentiment_score),
    
    retweet_count_total = sum(retweet_count),
    retweet_count_median = median(retweet_count),
    
    favorite_count_total = sum(favorite_count),
    favorite_count_median = median(retweet_count),
    
    text_total = paste(text, collapse = " ||| ")
  ) %>% 
  ungroup() %>% 
  inner_join(dji, by = c("created_date" = "date"))

donald_tweets_aggr %>% as_tibble
```

    ## # A tibble: 651 x 11
    ##    created_date     n sentiment_score… sentiment_score… sentiment_score… retweet_count_t…
    ##    <date>       <int>            <dbl>            <dbl>            <dbl>            <int>
    ##  1 2017-01-03      10           0.0128            0.407            0.902           144006
    ##  2 2017-01-04      10           0.0217            0.343            0.955           149930
    ##  3 2017-01-05       6           0.0509            0.373            0.908           105763
    ##  4 2017-01-06      12           0.0970            0.510            0.981           185275
    ##  5 2017-01-09       8           0.0578            0.460            0.996           172996
    ##  6 2017-01-10       4           0.5               0.600            0.901            56680
    ##  7 2017-01-11       6           0.0838            0.406            0.758           145811
    ##  8 2017-01-12       6           0.0587            0.572            0.980           130610
    ##  9 2017-01-13       8           0.0116            0.377            0.950           136100
    ## 10 2017-01-17      11           0.0355            0.589            0.905           166291
    ## # … with 641 more rows, and 5 more variables: retweet_count_median <dbl>,
    ## #   favorite_count_total <int>, favorite_count_median <dbl>, text_total <chr>, direction <int>
