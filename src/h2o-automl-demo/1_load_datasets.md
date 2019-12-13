@realDonaldTrump Tweets for Dow Jones Index Direction Prediction
================
Dmitry Petukhov

**Prepare dataset contains:**

  - **@realDonaldTrump tweets,**

  - **its sentiment scores from Microsoft Cognitive Service**

  - **Dow Jones Index.**

Import dependencies and read config:

# Part I. Naive approach

## Get @realDonaldTrump Tweets

Download
tweets:

``` r
donald_tweets_url <- sprintf("https://datainstinct.blob.core.windows.net/twitter/realdonaldtrump.csv?%s", secrets$azure_storage_key)


donald_tweets <- fread(donald_tweets_url, quote = "")

donald_tweets %<>% 
  mutate(
    id = as.character(id_str),
    
    # dates processing
    created_at = mdy_hms(created_at),
    created_date = as.Date(created_at),
    
    # text processing
    text = str_replace_all(text, "[^([:graph:][:space:])]", " | "),
    text = str_replace_all(text, '\"|\n', " || ")
  ) %>% 
  select(-id_str)


donald_tweets %>% 
  select(created_at, text, retweet_count, favorite_count) %>% 
  arrange(desc(created_at)) %>% 
  as_tibble()
```

    ## # A tibble: 10,096 x 4
    ##    created_at          text                                             retweet_count favorite_count
    ##    <dttm>              <chr>                                                    <int>          <int>
    ##  1 2019-08-29 13:19:47 The Farmers are going to be so happy when they …          8205          33277
    ##  2 2019-08-29 13:05:22 The Economy is doing GREAT with tremendous upsi…          7504          29745
    ##  3 2019-08-29 11:47:57 The totally inaccurate reporting by Lawrence O’…         13434          51166
    ##  4 2019-08-29 11:31:42 Crazy Lawrence O’Donnell who has been calling m…         10475          46181
    ##  5 2019-08-29 11:31:42 ....for the most ridiculous claim of all that R…          9998          45638
    ##  6 2019-08-29 11:03:38 Hurricane Dorian looks like it will be hitting …          8718          35362
    ##  7 2019-08-29 10:58:11 Puerto Rico is in great shape with Hurricane Do…          7307          19092
    ##  8 2019-08-29 02:33:04 RT @GOP: “Look what [@realDonaldTrump] did with…          8664              0
    ##  9 2019-08-29 02:32:01 RT @mike_pence: During the last Administration …         11435              0
    ## 10 2019-08-29 00:35:10 There has never been a time in the history of o…         29325         115301
    ## # … with 10,086 more rows

## Get Dow Jones NYSE index

Get (Dow Jones
NYSE)\[<https://www.quandl.com/data/BCB/7809-Dow-Jones-NYSE-index>\]
data:

``` r
Quandl.api_key(secrets$quandl_api_key)


dji <- Quandl("BCB/7809")

dji %<>% 
  arrange(Date) %>% 
  mutate(diff = Value - lag(Value)) %>% 
  na.omit %>% 
  mutate(direction = if_else(diff < 0, 0L, 1L)) %>% 
  rename(date = Date)


dji %>% 
  arrange(desc(date)) %>% 
  as_tibble
```

    ## # A tibble: 4,955 x 4
    ##    date       Value  diff direction
    ##    <date>     <dbl> <dbl>     <int>
    ##  1 2019-09-30 26917    97         1
    ##  2 2019-09-27 26820   -71         0
    ##  3 2019-09-26 26891   -80         0
    ##  4 2019-09-25 26971   163         1
    ##  5 2019-09-24 26808  -142         0
    ##  6 2019-09-23 26950    15         1
    ##  7 2019-09-20 26935  -160         0
    ##  8 2019-09-19 27095   -52         0
    ##  9 2019-09-18 27147    36         1
    ## 10 2019-09-17 27111    34         1
    ## # … with 4,945 more rows

## Join datasets

Aggregate tweets (daily):

``` r
donald_tweets_daily <- donald_tweets %>% 
  # remove retweets
  filter(!is_retweet) %>% 
  # calc daily stats
  group_by(created_date) %>% 
  summarise(
    n = n(),
    text = paste(text, collapse = " ||| ")
  ) %>% 
  ungroup() %>% 
  # join w/ Dow Jones Index
  inner_join(
    dji %>% select(date, direction), 
    by = c("created_date" = "date")
  ) %>% 
  # final touch
  mutate(direction = as.factor(direction))


donald_tweets_daily %>%
  arrange(desc(created_date)) %>% 
  as_tibble
```

    ## # A tibble: 651 x 4
    ##    created_date     n text                                                                 direction
    ##    <date>       <int> <chr>                                                                <fct>    
    ##  1 2019-08-29       9 The  Amazon Washington Post and @CNN just did a Fake Interview on P… 1        
    ##  2 2019-08-28      15 A made up Radical Left Story about Doral bedbugs but Bret Stephens … 1        
    ##  3 2019-08-27      18 Just returned to Washington from France and the very successful G-7… 0        
    ##  4 2019-08-26      13 .@SteveHiltonx “The Democrats statements about Donald Trump are lie… 1        
    ##  5 2019-08-23      21 .@SenCoryGardner has done a fantastic job representing the people o… 0        
    ##  6 2019-08-22      10 It was my honor to sign a Presidential Memorandum facilitating the … 1        
    ##  7 2019-08-21      22 “Thank you to Wayne Allyn Root for the very nice words. “President … 1        
    ##  8 2019-08-20      18 Thank you to my great supporters at the 2019 @IowaStateFair! https:… 0        
    ##  9 2019-08-19      13 Anthony Scaramucci is a highly unstable “nut job” who was with othe… 1        
    ## 10 2019-08-16      16 https://t.co/TCg6dLoIya ||| Thank you New Hampshire. KEEP AMERICA G… 1        
    ## # … with 641 more rows

## Save artifacts

# Part II. NLP approach

## Tweets sentiment

Get tweets sentiment score from Azure Cognitive Services (Text Analytics
API)\[<https://azure.microsoft.com/en-us/services/cognitive-services/text-analytics/>\]:

``` r
source("cognitive_services_api.R")

donald_tweets_sentiments <- donald_tweets %>%
  #filter(year(created_date) == 2019) %>% # TODO: only for debug
  group_by(lubridate::year(created_date), lubridate::month(created_date)) %>% 
  group_map(~ get_sentiment(.x, secrets$cognitive_services_api_key, verbose = T))
```

    ## [1] "Processing tweets number: 214, size: 61.3 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 154, size: 44 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 147, size: 41.9 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 152, size: 43.6 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 155, size: 44 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 212, size: 60 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 245, size: 70.5 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 267, size: 77 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 306, size: 87.4 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 290, size: 82.3 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 261, size: 82.5 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 199, size: 68.9 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 205, size: 73 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 175, size: 63.7 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 171, size: 60.4 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 257, size: 89.6 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 259, size: 90.9 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 386, size: 133.4 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 336, size: 115.5 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 380, size: 133.8 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 383, size: 122.6 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 384, size: 123.8 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 338, size: 116.3 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 294, size: 109.1 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 369, size: 126.1 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 249, size: 82.5 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 405, size: 127.5 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 431, size: 139.9 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 692, size: 222.2 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 487, size: 161 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 692, size: 223.4 Kb"

    ## Completed.

    ## [1] "Processing tweets number: 601, size: 191.8 Kb"

    ## Completed.

``` r
donald_tweets_sentiments %<>% 
  bind_rows %>% 
  transmute(id, sentiment_score = score)


donald_tweets %>% 
  inner_join(donald_tweets_sentiments, by = "id") %>%
  select(text, sentiment_score) %>% 
  as_tibble
```

    ## # A tibble: 10,096 x 2
    ##    text                                                                              sentiment_score
    ##    <chr>                                                                                       <dbl>
    ##  1 TO ALL AMERICANS-#HappyNewYear &amp; many blessings to you all! Looking forward …          0.993 
    ##  2 RT @DanScavino: On behalf of our next #POTUS &amp; @TeamTrump-#HappyNewYear AMER…          0.5   
    ##  3 RT @Reince: Happy New Year + God's blessings to you all.  Looking forward to inc…          0.993 
    ##  4 RT @EricTrump: 2016 was such an incredible year for our entire family! My beauti…          0.962 
    ##  5 RT @DonaldJTrumpJr: Happy new year everyone. #newyear #family #vacation #familyt…          0.915 
    ##  6 RT @IvankaTrump: 2016 has been one of the most eventful and exciting years of my…          0.907 
    ##  7 Well the New Year begins. We will together MAKE AMERICA GREAT AGAIN!                       0.931 
    ##  8 Chicago murder rate is record setting - 4331 shooting victims with 762 murders i…          0.5   
    ##  9 @CNN just released a book called  || Unprecedented ||  which explores the 2016 r…          0.934 
    ## 10 Various media outlets and pundits say that I thought I was going to lose the ele…          0.0372
    ## # … with 10,086 more rows

## Join datasets

Aggregate tweets (daily):

``` r
donald_tweets_daily <- donald_tweets %>% 
  # remove retweets
  filter(!is_retweet) %>%
  # join w/ tweets sentiments
  inner_join(donald_tweets_sentiments, by = "id") %>% 
  # calс daily aggregates
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
  # join w/ DJI
  inner_join(
    dji %>% select(date, direction), 
    by = c("created_date" = "date")
  ) %>% 
  # final touch 
  mutate(direction = as.factor(direction)) %>% 
  rename(text = text_total)


donald_tweets_daily %>% as_tibble
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
    ## #   favorite_count_total <int>, favorite_count_median <dbl>, text <chr>, direction <fct>

## Remove stopwords

``` r
tweet_words <- donald_tweets_daily %>%
  select(created_date, text) %>% 
  filter(!str_detect(text, '^"')) %>% # remove quotations
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% # remove links
  unnest_tokens(word, text, token = "regex", pattern = "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))") %>% # remove @ and #
  filter(
    !word %in% stop_words$word &
    str_detect(word, "[a-z]")
  )


tweet_words %>% 
  count(word) %>% 
  arrange(desc(n))
```

    ## # A tibble: 9,363 x 2
    ##    word          n
    ##    <chr>     <int>
    ##  1 people      694
    ##  2 country     533
    ##  3 president   490
    ##  4 news        488
    ##  5 democrats   466
    ##  6 fake        436
    ##  7 trump       423
    ##  8 border      421
    ##  9 america     379
    ## 10 time        343
    ## # … with 9,353 more rows

## Stats and word cloud

``` r
tweet_word_counts <- tweet_words %>% count(created_date, word)


# go to 4-th page
tweet_word_counts %>% 
  filter(word != "rt") %>% 
  count(word) %>% 
  arrange(-n)
```

    ## # A tibble: 9,363 x 2
    ##    word          n
    ##    <chr>     <int>
    ##  1 people      351
    ##  2 country     294
    ##  3 news        274
    ##  4 president   267
    ##  5 democrats   259
    ##  6 trump       255
    ##  7 fake        250
    ##  8 america     245
    ##  9 time        241
    ## 10 american    200
    ## # … with 9,353 more rows

``` r
# go to 6-th page
tweet_word_counts %>% 
  filter(word != "rt" & year(created_date) == 2019) %>% 
  count(word) %>% 
  arrange(-n)
```

    ## # A tibble: 5,384 x 2
    ##    word          n
    ##    <chr>     <int>
    ##  1 people      112
    ##  2 country     102
    ##  3 democrats    94
    ##  4 president    91
    ##  5 news         89
    ##  6 border       86
    ##  7 trump        85
    ##  8 fake         82
    ##  9 time         79
    ## 10 united       77
    ## # … with 5,374 more rows

``` r
total_words <- tweet_word_counts %>% 
  group_by(created_date) %>% 
  summarize(total = sum(n))

total_words %>% arrange(-total)
```

    ## # A tibble: 651 x 2
    ##    created_date total
    ##    <date>       <int>
    ##  1 2019-05-10     435
    ##  2 2019-05-13     409
    ##  3 2019-07-11     405
    ##  4 2019-06-13     403
    ##  5 2019-06-26     368
    ##  6 2018-08-29     356
    ##  7 2018-12-07     351
    ##  8 2018-10-30     304
    ##  9 2019-08-07     304
    ## 10 2019-08-21     300
    ## # … with 641 more rows

``` r
tweet_words %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50, color = "gray"))
```

    ## Warning in wordcloud(word, n, max.words = 50, color = "gray"): president could not be fit on page.
    ## It will not be plotted.

![](1_load_datasets_files/figure-gfm/word_clouds-1.png)<!-- -->

## Compute TD-IDF

``` r
tweet_word_counts <- tweet_word_counts %>% 
  left_join(total_words, by = "created_date") %>%
  bind_tf_idf(word, created_date, n)

tweet_word_counts %>% 
  group_by(created_date) %>% 
  filter(tf_idf == max(tf_idf)) %>% 
  arrange(desc(created_date))
```

    ## # A tibble: 930 x 7
    ## # Groups:   created_date [651]
    ##    created_date word            n total      tf   idf tf_idf
    ##    <date>       <chr>       <int> <int>   <dbl> <dbl>  <dbl>
    ##  1 2019-08-29   apologize       4   139 0.0288   3.91 0.113 
    ##  2 2019-08-28   bret            2   209 0.00957  6.48 0.0620
    ##  3 2019-08-28   stephens        2   209 0.00957  6.48 0.0620
    ##  4 2019-08-27   appalachian     2   236 0.00847  6.48 0.0549
    ##  5 2019-08-27   italian         2   236 0.00847  6.48 0.0549
    ##  6 2019-08-27   powerfully      2   236 0.00847  6.48 0.0549
    ##  7 2019-08-27   stooges         2   236 0.00847  6.48 0.0549
    ##  8 2019-08-26   biarritz        3    88 0.0341   6.48 0.221 
    ##  9 2019-08-23   china          13   264 0.0492   1.78 0.0876
    ## 10 2019-08-22   germany         3   144 0.0208   3.18 0.0663
    ## # … with 920 more rows

## Calc Document-Term Matrix

``` r
tweets_dtm <- cast_sparse_(tweet_word_counts, "created_date", "word", "tf_idf")

print(dim(tweets_dtm))
```

    ## [1]  651 9363

``` r
print(tweets_dtm[1:7, 1:7])
```

    ## 7 x 7 sparse Matrix of class "dgCMatrix"
    ##                   act affordable    allowed     america    arizona battlefield     bill
    ## 2017-01-03 0.05911224   0.184056 0.03331250 0.023548227 0.04137334  0.06970316 0.024671
    ## 2017-01-04 .            .        .          .           0.03858413  .          .       
    ## 2017-01-05 .            .        .          .           .           .          .       
    ## 2017-01-06 .            .        .          0.009396648 .           .          .       
    ## 2017-01-09 .            .        .          .           .           .          .       
    ## 2017-01-10 .            .        .          .           .           .          .       
    ## 2017-01-11 .            .        0.05317188 .           .           .          .

``` r
donald_tweets_daily %<>% 
  select(-text) %>% 
  cbind(tweets_dtm %>% as.matrix %>% as_tibble)


donald_tweets_daily
```

    ##      created_date n sentiment_score_min sentiment_score_mean sentiment_score_max
    ##      retweet_count_total retweet_count_median favorite_count_total favorite_count_median direction
    ##      act affordable allowed america arizona battlefield bill border called car care chevy clinton
    ##      conference congress crazy creation cruze dangerous @danscavino dealers delivering democrat
    ##      driving #dts due eleventh ethics extremely focus ford free gitmo governor healthcare
    ##      importance increases independent innovation invest january job jobs lousy magnet mexican
    ##      mexico michigan minnesota model motors news obamacare pay people plant policies priority
    ##      promised reform releases remember scrap sending tax trump unfair watchdog weakening wealth
    ##      world's album announcing assange beginning blame briefing build careful careless clowns
    ##      coverage creating debate deductibles defense delayed dems disaster dishonest dnc double
    ##      evancho's failed fall follow @foxnews friday giving hacked hacking #hannity hike inauguration
    ##      info intelligence jackie julian massive media mess movement pass performance podesta poor
    ##      practically premium questions republicans responded rnc russian russians sales schumer
    ##      scrapping skyrocketed standard strange terrible time total understand useless usual web weight
    ##      agreement bad baja cars chuck clown corolla democrats doctor expensive fan fix head lead lie
    ##      lies likes minds motor plan political simply toyota truth typical wrong 20th anna arnold beat
    ##      building cancelled cares chairs committee committees compare compared comparison computer
    ##      conde democratic destroyed djt editors examination exclusive fbi fireworks florida friend
    ##      happening hillary house investigate kasich machine meet minute money monitoring movie nast
    ##      national nbc newhouse obama office paid passion politics prayers prior ratings report
    ##      requested russia safe sake schwarzenegger scott season secret senate servers shared situation
    ##      speed spent spoke star stay steven study supported supporters supposedly swamped top tower
    ##      trouble voters wall weeks wintour wow 100th 1billion actresses adding announced attacked
    ##      billion built candidate changed chrysler developer disabled dollar expand fake fiat finally
    ##      flunky globes golden groveling guy hollywood lost meryl mocked murdoch nancy night ohio paying
    ##      picture plans plants quickly rated reagan reporter ronald rupert story streep successful
    ##      totally week written business communities confidence election helps index
    ##      #lawenforcementappreciationday lift optimism protect serve 'small soars surges 'trump 'u women
    ##      yr agencies belittle 'buzzfeed claims complete crooked deals easily fabrication #fakenews
    ##      germany hunt leak leverage living loans nazi nonsense opponents public runs shot unverifiable
    ##      unverified utter verified victory win witch amy3 bean buy circulated clapper @cnn congrats
    ##      couple courage credibility denounce donald elect false fictitious illegally james
    ##      @lbperfectmaine linda meltdown morning organizations phony president proud @realdonaldtrump
    ##      #repealobamacare step support taking tanking yesterday @zhu afraid allegations based beautiful
    ##      cabinet campaigned clinton's community complaining days detective enforcement enthusiasm
    ##      exists express funeral guilty hell hero history information knowing law mcdonald mine nice
    ##      nominee nyc operatives proof real released respect run sleazebag spy sued unaffordable
    ##      approval attend auto bikers boycotted bringing bush bush's character class cnn coming cost
    ##      country daughter @drgoodspine elected familiar hypocritical imagine informed ivanka @ivanka
    ##      john @levisteveholt lewis maintain military missed negotiated polls pouring purchases push
    ##      rating record reductions rigged saturday sound special starting stuff swearing thursday
    ##      transparency true twitter walmart washington woman won wp add address ag ago @ainsleyearhardt
    ##      alabama ambassadors announcement badly barbara bayer biased ceo's companies countries
    ##      diplomats enjoy forward @foxandfriends george glorious hospitalized inaugural interviewed
    ##      investments lago letter lockheed mar meeting @nbc @nbcnews past playing pledged recovery
    ##      speedy string @thealabamaband white winter wonderful world writing @wsj 4pm amazing american
    ##      begins concert dinner divided doubt evenings family fighting franklin #godblesstheusa graham
    ##      hard honor #inauguration2017 join journey kimberly leave lincoln memorial ready reverend
    ##      service stated @theleegreenwood tonight vp warriors wounded administration #americafirst
    ##      americans borders bring city continues controlled controls day dreams evening forgotten
    ##      government hire #inauguration #inaugurationday joining #maga matters moment mountain nation
    ##      party power remembered rulers rules simple transferring busy executives heavy manufacturing
    ##      planned security talk abbas access automobile construction dakota delivered displayed hall
    ##      industry keystone leaders lower move oval photo pipelines press signing sold upper @whitehouse
    ##      xl @abc barra carnage ceo chicago congratulations court @davidmuir dead depending feds fields
    ##      fraud horrible illegal including interview investigation killings major mark mary o'clock pick
    ##      procedures registered results send shootings smart strengthen supreme times tomorrow vote
    ##      voter voting 00pm @abc2020 @abcworldnews afternoon calling cancel chelsea congressional
    ##      control dade david deal decision deficit @dhsgov drops duty @gop leader lives manning mayor
    ##      miami minutes muir nafta pa philadelphia policy @potus prison retreat @romoabcnews sanctuary
    ##      @seanhannity @senatemajldr @senmajleader sided @speakerryan speech strong trade traitor
    ##      ungrateful united unwilling upcoming @vp weak advantage change crew deficits final gregg
    ##      holocaust international #marchforlife marching mattis mike pence phillips promise remembrance
    ##      secretary speaking statement today's votes votestand airports ban campaign caused create delta
    ##      detained dream dudes enter environment fleeing haven held kelly live lot ma nominate notice
    ##      opposition outage outrage protesters questioning rush searching senator tears terrorists
    ##      tuesday ashamed attorney delaying dem mic obstruct pelosi picks purely rally reasons rest
    ##      steps arguing brilliant call deliver gorsuch hope intentions judge keeping neil nomination
    ##      respected agreed attending australia ballistic berkeley brave chief collapse dignified dollars
    ##      dumb federal firing form formally funds immigrants innocent iran iraq legs life line love
    ##      missile obvious owens practices rapidly rex ryan @samsung squandered sworn thankful thousands
    ##      tillerson transfer trillion view violence anarchists apprentice biggest bills california civil
    ##      conversation crafted edge evil fire france health islamic lied locked louvre millions minister
    ##      museum paris prime professional proving radical telling terrorist thugs tourists voted worse
    ##      abc accumulation airs bob bowl brady coach comeback data extended extreme failing fiction
    ##      kraft marginalize negative @nytimes @oreillyfactor patriots shots sources stories super tom
    ##      vetting winners writes #1in apologize assoc courts east europe fast forced haters incredible
    ##      middle putin reporting serv sheriffs subscribers terror terrorism threat brian chiefs delay
    ##      discussing disgrace easy entitled eu 'immigration increase @intel investment krzanich
    ##      leadership leading 'majority nations @nordstrom obstruction person police poll popular pushing
    ##      reporters safety style traffic travel treated trump's truthful unfairly vulnerable wait
    ##      anymore blumenthal bogged chris conflict cuomo died emboldens enemy failure fought losing
    ##      mccain misrepresents mission richard sen @senatorsessions stake start success talking term
    ##      told vietnam winning andrews base bother china cite disgraceful earlier entire heading joint
    ##      lawfare length #marineone nov opinion panel remarkably shinz spoken statute xi canada canadian
    ##      discuss @justintrudeau pm trudeau workforce @betsydevos dr fail humana #icymi korea leaks
    ##      listening mnuchin parent pull 'remarks repeal replace resolution save session shulkin teacher
    ##      treasury @usedgov #vasecretary veterans watch 5pm aetna airport association attempt benjamin
    ##      blind bloomberg candy classified connection conspiracy cover crimea 'death eli hatred husband
    ##      interfere @israelipm lake leopoldo @liliantintori lopez @marcorubio melbourne met mistakes
    ##      @msnbc nsa orlando prisoner #repealandreplace retail sara scandal sense soft spiral theories
    ##      tickets unwatchable usa venezuela @washingtonpost caught coal decades discredited effective
    ##      fading hits leakers leaking level low makes market mining rollout rule signs spotlight stock
    ##      streak undoing 00am agree approving boeing calls carolina @cbs charleston conferences delays
    ##      happy keith kellogg limbaugh moving play price replacement sick south spend statements advisor
    ##      beautifully break generals immigration mcmaster named presidents scale sweden activists
    ##      'americans angry cities crowds districts home liberal numerous oppose overwhelmingly sad video
    ##      african culture ellison enjoyed fight predicted rep smithsonian's tour edges pledge renews 'sp
    ##      council cpac devastating effect 'epidemic henry human killed kravis permeated stop trafficking
    ##      unable vows ceos era favorably gop @hotlinejosh insurance peo provide viewed wsj #jointaddress
    ##      #jointsession 8th consumer gains november posted accurately adams addressjoin andrew approved
    ##      book card close demand education green grip hand hear hearby honest hypocrite intentional jeff
    ##      kids lying merit narrative nick overplaying pathetic read reality response saint saving
    ##      @senschumer sessions supposed system ties warrior weekly agenda coast congratulates core exxon
    ##      @exxonmobil gulf mobil 'president principals program region spirit sweeping added admin
    ##      competition drug imploding infighting lines missiles negotiation phase picked pricing
    ##      principles prisoners promote ran returned review rid stronger vicious worry august consecutive
    ##      critical discussion economy fabric february feel hiring honoring linkedin months @randpaul
    ##      @repcummings role roles september society strongest tremendous vital women's comfort decade
    ##      delegates honored reached @senateyouth empty gm incoming @mayorbowser metro paul preparations
    ##      #readthebill representatives rude seats @secpricemd storm wiedefeld worst aimed begin career
    ##      continue crowd cut detroit expected fired found fun gun heard jackson jail mailbox nashville
    ##      optimistic outcry #potus7 production regulations returns slash @snoopdogg tennessee
    ##      @tuckercarlson unnecessary #usa budget chamber commerce @endakennytd future growth hispanic
    ##      improvements ireland mtgs progress prosperity taoiseach wh behaving businesses fheile friends
    ##      irish north phadraig @republicanstudy college colluded contact electoral eric evidence excuse
    ##      fox kentucky lara leaker louisville potus pushed running astronauts capitol dedication
    ##      engineers farmers @housegop ingrained joined legislation @nasa's #nationalagday #passthebill
    ##      ranchers scientists sign action #ahca attack cochran condolences dishonesty kurt london
    ##      #obamacare offer reports theresa truckers trucking watched 20k announce caucus chance
    ##      commitment freedom grea irony #keystonepipeline official parenthood permit pleased premiums
    ##      presidential pro skyrocketing thrilled company covered defeat energy fo folds @glfop hoax jaws
    ##      #madeintheusa praise republican reset sale shape snatch uranium addiction apologized combating
    ##      commission crisis empowerment eo established @flotus inaccurately melania opioid viciously
    ##      alternatives board cuts difficult disgraced ed helping heroes hurt @jim jordan labrador laws
    ##      libel losses op prepared raul @raul @repkenbuck @repmarkmeadows solid team #thankaveteran war
    ##      celebrates condoleezza #confirmgorsuch defend denmark enlisting exciting field flynn historic
    ##      immunity kke lars @larsloekke loss #makeamericagreatagain potential proportion rasmussen rice
    ##      #scotus survey #weeklyaddress worker al answers behalf betwe brother egypt electronic @fbi
    ##      hosting lifted light #liub multiple partnership project receiving relationship renew ride
    ##      sanctions scheme sisi soldier spied surveillance unmasking unprecedented
    ##      #worldautismawarenessday @wwp #ceotownhall governing invite mcgarvey #nabtu2017 sean speak
    ##      america's committed deeply preserving standing strengthening host #soldierridedc americ
    ##      celebrating #chagsameach elevation israel justice passover @usnavy decides estes explained
    ##      forum kansas korean ron secretaries solve strategic economic fair @foxbusiness gen heavily
    ##      @mariabartiromo menace nato private promises race sec sector share soaring stand unleash
    ##      workers allies atlanta's fine heroic lasting peace plummeting properly responders restored
    ##      returning senses congressioal criminals enjoyment event exposed foreign game georgia hold
    ##      knowles michael milwaukee presidency raise reading recent slanted spurs taxes wisconsin 2nd
    ##      #buyamericanhireamerican candidates crime district eleven force gangs jon learned ms ossoff
    ##      @ossoff removing runoff choice extension glad handel improvement june karen #s544 signed
    ##      champion england gentiloni italy labor paolo @patriots steel @superbowl visit @astropeggy
    ##      #buildthewall #congratspeggy congratulating death drugs fixed hashtag poisoning positive
    ##      proclamation stopping tool tumbling youth agriculture america'executive dairy executive
    ##      hearing issues @ivankatrump museum's position 'presidential promoting remarks rural w20
    ##      antiquities bail birthday brought choices circuit dept designations disastrous enforcing fro
    ##      gdp @interior messy ninth overturned prohibitions puerto recognize recorded 'review rico
    ##      ridiculous rulings shopping slowest statutory accountability aluminum argentina awada bailing
    ##      billions blocking @commercegov @deptvetaffairs donors families illegals imports improving
    ##      jeopardizing juliana lady @mauriciomacri memorandum miners ocare parks pour prepare protection
    ##      reach rebuild rebuilding received relationships renegotiate @secretaryross secure shut subject
    ##      summer terminate threaten threats troops vacations whistleblower 7pm disrespected highly
    ##      launched @marthamaccallum started unsuccessfully vets wishes air chief's commander falcons
    ##      happen reason senators shutdown trophy @betsydevosed comey deeds director happened hosted
    ##      justification mahmoud #schoolchoice terrific 'aetna attention citing exit garden immediately
    ##      indv markets #nationaldayofprayer officially officials refusing religious rose subcommittee
    ##      susan testify transition unbelievable va victorious watching 75th anniversary australians
    ##      bedminster causing disruption inclusion marking rarely saves @turnbullmalcolm weekend wowthe
    ##      charade clearance collusion counsel funded giorgi johndaly kvirikashvili newspapers oath @pga
    ##      reiterated sally seldom taxpayer unhappy winner yates hires levels 'manufacturing month
    ##      openings rise teachers acts aggrieved alike baby begged bravery calm connecticut conquests
    ##      cried cryin devised dir #draintheswamp forgiveness frauds hypocrites indignant investigated
    ##      joke jury pol prestige pretend recently replaced richie roger stone thanking advisory
    ##      cybersecurity establishment fm infrastructure integrity laughing lavrov meetings networks
    ##      pavlo rosie sergei sleeves ukraine accuracy active beef briefings center challenge christian
    ##      conversations fabricated hel knowledge lots luck mother's overtime perfect podium products
    ##      responses rocketry school sell starts surrogates tapes #tarc virtually welcomed 36th annual
    ##      #nationalpoliceweekwatch officers absolute airline and#policeweek april erdo flight greatly
    ##      humanitarian industrial isis openly #peaceofficersmemorialday pertaining recep scheduled
    ##      surged tayyip turkey #cgacommencement17 @uscgacademy appointed colombia conf councel juan
    ##      manuel politician santos single #policeweek proclaims protecting strongly trip invitation
    ##      @netanyahu @presidentruvi arabia citizens civilized #g7 kingdom #nato pope sacred saudi
    ##      solidarity unforgettable vatican belgium brussels determined francis holiness lifetime peac
    ##      pursue warm arrived ceremony #g7summit #natomeeting #potusabroad resolve taormina civilization
    ##      confront engage engaged g7 mattarella saved sergio lame switch accord alliances barron blows
    ##      brennan carter children constant covfefe excoriated figure griffin kathy meaning misleading
    ##      nguy page partnerships ph reported 's son testimony traveled xu blames facebook refuses
    ##      alarmed approvals approve correct deserve expedited forever initiative khan lawyers msm
    ##      obstructionists original politically sadiq @secshulkin's seek slow stayed submitted tougher
    ##      version watered wins beaches cbs #dday extremism folks funding hate horror ideology king
    ##      message normandy nytimes qatar reference relied social stormed unfiltered washpost asap
    ##      christopher cincinnati credentials desk details @gopleader impeccable @johncornyn nominating
    ##      passed @senategop substandard tolerate victims wray coalition dodd @faithandfreedom financial
    ##      frank god hensarling jeb restore summit ty vindication worship br @cabinet 'first forget
    ##      horrific kicking killing passage pennsylvania #pulsenightclub regulation shooting 9th answer
    ##      apology @clemsonfb decisions dirty dropped incorrect lo lynch million #nationalchampions
    ##      obstructionist purposely purposes resist ruled tigers army injured louisiana patriot recover
    ##      sacrifices scalise soldiers steve cuba hates hearings investigations powerful 5th cutting
    ##      deepest fantastic #ga06 handel's handle's heartfelt jay @jc kill norman opponent otto panama
    ##      p's ralph represent sailors sc's sekulow #ussfitzgerald varela #votekarenhandel
    ##      #voteralphnorman warmbier brutality condemns efforts ideas mourn regime victim 6th 8pme
    ##      carolina's cellular cutssecurity elections healthcaretax huge iowa kirkwood landed petro
    ##      poroshenko busts deadliest dhs gang grand hacks #healthcarebill homeland @icegov idea
    ##      intercepts jeh johnson operation ranked recordings refuse scam server @stevescalise supportive
    ##      syria #teamscalise thinking york amounts helped legislative @marineband picnic tradition
    ##      #vaaccountability boat burn choke cia colluding complain crash glass grateful magnifying
    ##      meddling notified obstructed rock bombshell check cold courtesy dismissed employees falsely
    ##      finished legacy management resign retract rig unlike ahead amazon #amazonwashingtonpost
    ##      crushing @cubs guardian india's internet medicaid misstated monday @narendramodi referred
    ##      restrictions roundtable series swift ve verify welcoming bleeding chemical crucial cusp ene
    ##      @epascottpruitt eve insisted joe kate #kateslaw lawmakers mika @morning nights 'no
    ##      #nosanctuaryforcriminalsact passing poorly psycho row #saveamericanlives @secretaryperry
    ##      @secretaryzinke speaks stopped sugar trillions #unleashingamericanenergy unlock warning
    ##      weapons year's article assurances carry closely date discussed encouraged enquirer epidemic
    ##      inspire moon's patience proportions subjects begun #charliegard delighted dow dumping hit
    ##      intraday kick oppression overcoming shawn uniform words 152nd celebrate dedicated grew
    ##      #happyindependenceday poland professionals quarter usss blessing broken eradicate evils
    ##      #potusinpoland prevail shoulder thrive triumph values west abe chancellor defining editorial
    ##      experience #g20summit hamburg japan merkel moon refused reviews vladimir agrees angela
    ##      ceasefire cyber dare leaked left mother motheras pres seat short unit confirmed heartbreaking
    ##      marine mississippi nominees olympics plane tuned 100yrs bastille chinese crossings
    ##      disinformation entry exist focused functioning influence loves macron moscow opportunity
    ##      outlook perfectly rdy removed standards transparent @washtimes willfully wiped wwi bilateral
    ##      @emmanuelmacron @emmanuelmacronthank friendship invalides les lys palace unbreakable
    ##      @usembassyfrance #14juillet 7yrs accomplished #bastilleday celebration eiffel impt inviting
    ##      jersey magnificent mcconnell mourns o'care parade pen suffering the#uswomensopen tournament
    ##      attended don hyun jr park politicians sung @usga #uswomensopen clean current loyal majority
    ##      return slate victories arranged dies invited lunch lunchtime scream sinister spouses cindy
    ##      departing pentagon teams 1st carried corning gysxxqazbl #honorthem icymi #madeinamerica
    ##      manufacturers merck pfizer privilege qtr richer @shopfloornam stuart survivors #ussarizona
    ##      @varneyco adam arriving beleaguered campaigning congressman crimes cuttingstock drain hillarys
    ##      investigators jobsborder @loudobbs #ocarenightmare ourselvesnot reg relations repercussions
    ##      schiff sewer sleazy spends swamp television #2017jambo acting assad boom boost challenged
    ##      charge cherish collude fooled grt hariri intel jared kushner lebanon lobbyist mails mccabe
    ##      monopoly payments plate post protectionist quietly rebels saad sabotage @senjohnmccain shortly
    ##      syrian torturing traveling ukrainian waited wasteful weapon wife youngstown accept advised
    ##      alaska amvets boys broke burdened capacity consultation costs decisive entail experts girls
    ##      individuals @lisamurkowski medical overwhelming records salutes transgender wife's 13k 14th
    ##      actions alexandria award catherine collected courageous foxconn gathered herridge investing
    ##      involving madeintheusa medal nation's officer ridding shine spread tribute valor budgets
    ##      enforce eradicated #flashbackfriday implode inform island #lesm #lesmharrisburg #ms13 priebus
    ##      reince senseless siege spectacular staff waiting chaos hurting lowest pays raising
    ##      unemployment wages corporations enemies mainstream mentions roarwatch varney belt comrades
    ##      dreamers hero's innovators owners powering protects #raiseact rust taxpayers vet 00pme
    ##      breakthrough buildings connect continuing costly dump grow hcare homes huntington tirelessly
    ##      tune untrue #vavideoconnect virginia 4k 6b busting #cg227 excellent @fema goal hurricane local
    ##      matter mazda preparedness proudly spending stifling @uscg artist battle battles bigger child
    ##      closer completed con council's defrauded deregulation driven existent #fake favor impressed
    ##      inept judges liability penn polling prediction #purpleheartday rallies remain renovation
    ##      sacrificed securityisis strength surging unfunded vacation wapo achieved amazonwashingtonpost
    ##      dangers failingnewyorktimes failurecountries holding posed reluctant suppression tough arsenal
    ##      endorsement excessive expectations luther mitch modernize nuclear renovate representing
    ##      delegation entrepreneurship #ges2017 globally hispanics india rebounds screamed supporting
    ##      union development ensure gain grows jong kim loadedshould path placelocked programs reaches
    ##      skills solutions succeed trail unwisely additional changing charlottesville councilhe frazier
    ##      ken leaving @merck pharma president's prices realize renovated resigned ripoff satisfied anger
    ##      feels grandstanders interference alternative assistance businesspeople catastrophic colmery
    ##      congratulation damage educational harry heather heyer moore pressure putting reasoned
    ##      retailers roy @secshulkin strategy towns unacceptable wise 22nd barcelona beauty bigotry
    ##      comparably convention disgusting equivalency factor flake foolish jefferson kelli kkk learn
    ##      learning lee lindsey @mayorgimenez misrepresent monuments moral nazis neo pershing phoenix
    ##      publicity removal ripped robert seeking shame spain statues stonewall supremacists toxic
    ##      trouncing ward alert camp combatant command cont directed elevated means nj protective rights
    ##      status unified aboard falwell jerry liberty listen rescue search underway university
    ##      #ussjohnsmccain abroad arlington bless everlasting fallen fort hearts myer nationfull onward
    ##      pray pride serving souls transcript #alconv2017 @americanlegion appeals conquer customs eye
    ##      facility fairly faith filibuster modernization packed #phoenixrally platform streets turnout
    ##      wasting yuma afghanistan authority az ballard ceiling debt dynamic enthusiastic famously
    ##      #hurricaneharvey intensifies legion patrol #planahead respectful somber speeches staes station
    ##      tie tones types @usmc visited administrations advice assist brock category constantly
    ##      convinced corker cos developments doings duke edwards elaine encourage @govabbott harvey heed
    ##      @louisianagov mph projected respond retaking sc site strengthens talent texas @tombossert45
    ##      winds fill @ingrahamangle positions reduce size devastation dying extortion ferocious harveymy
    ##      heart magazines missouri witnessing countless devotion #texasstrong clip exonerated happier
    ##      healing heeling prayer #prayfortexas allowing amount daca d's equipment hardworking increased
    ##      r's sophisticated substantially andeavor approves atlantic declarations emergency governors
    ##      islands issue kenneth largest legalize mapp refinery revisit ricardo rick rossell taxed virgin
    ##      weary epic fellow guard #hurricaneirma hurry irma #irmahurricane2017 #oneamericaappeal repub
    ##      water wayif #neverforget #neverforget911 abdul articles birth books distinguished fascinating
    ##      locationsthan luke malaysia najib razak fema package process age bernie cell consent curse
    ##      debates deplorables direction earthquake educated exchange existing expressed fault feelings
    ##      fences fl haunt loud match parents payer phone rate reception sanders throw veto walls 70th
    ##      chain dealt demented espn eternally fx giaccio larger loser main manner migration missing
    ##      nasty @natlparkservice proactive programming recruitment scotland sights specific stupidly
    ##      suffered tx untruth @usairforce yard @antonioguterres bureaucracy @cia commend sg #unga
    ##      #usaatunga democracy destroy freedoms #noko restoration tricky #ungafull abdullah blessings
    ##      cassidy challenges conditions criticizes direct discussions emmys facing fixing fulfill
    ##      helpful ii include involved jacob jewish lucky luncheon monster noticing nukes opportunities
    ##      palestinian pre question rand research saddened smartest sooo @un ashraf denuclearization
    ##      endorsed endorses ghani nra pleasure #prstrong @ricardorossello ads airlines campaigns doug
    ##      flights gained madman mightily mind parker screaming starving tested anthem backlash booed
    ##      disrespect disrespecting fans flag kneeled kneeling looked nascar nfl opposed percentage
    ##      players stance #standforouranthem affected ambassador banks belief booing brutal carmen clips
    ##      cruz dallas deep devastated electrical extend #fema finish food football grid isolate kneel
    ##      knees loudest @marianorajoy @nikkihaley owed priorities proven #puertorico repealing replacing
    ##      sadly san set sorts stood street tortured @usun #usvi vile yulin advising anti athletes bolt
    ##      cowboys deadline dec hencefake hospital indiana indianapolis jamaica jones nsc primary
    ##      reconciliation roaring runners sounds #taxreform usain @wapo docks electric generators locals
    ##      treat aligning classrooms department hardagainst holiest hurricanes @impdnews kippur
    ##      metropolitan observing oddsin reaching revised roads rossello taught upward yom las
    ##      @presidentscup sympathies #teamusa vegas warmest respects clock landing miracle refuted
    ##      remains shooter warmth write doctors sunday threatened verification gillespie killer
    ##      northamwho violent virginiais #columbusday dealing october agreeing art author bedford bench
    ##      bordersthey breaks fool hill jemele lessons liddle owner penguins pittsburgh recording salute
    ##      sit #stanleycup tanked 1m 45pme 9pme awarding bounds demanding demean encourages finest
    ##      goodell harrisburg importantly leaps license ot producing pure tenfold tragedy #242navybday
    ##      acquire beware deserves fakenews hostile h's iranian pet piece recognizable subsidy unmatched
    ##      #valuesvoterssummit viewers greenville hated int'l laffer spartanburg 30pme alexis @cbsnews
    ##      consideration czar #dow23k earnings fueled greece marino passes product prototypes tsipras
    ##      withdrawing writers amb assertion @bradthorthank brazenly burden code compliance compliment
    ##      confirms congresswoman co's creators @danaperino decided drafted exonerating finance fortune
    ##      founders generation gift govt harmful @heritage hides ins lamar limits milestones @nfl
    ##      protected revitalize revive technical thousand understood violates wildfires dossier firm
    ##      foundation inventing amid annually colonel content dilloncjtf @geraldorivera getter heaven
    ##      jeffress late oir personal relief rich rises secretly stage @unsecretary #vegasstrong wacky
    ##      wilson beirut dozen hesitation hizballah la sgt stays widow catcher clue dog endorse
    ##      incompetent lightweight ovations tennesse 7pme aired anticipated colleagues congratulate
    ##      @danpatrickthank #dobbs extraordinary fest gov #jfkfiles led liyuan lt madame noko peng
    ##      @reince release talented todays declared heritage #opioidcrisis #'s underestimate unity abuse
    ##      affliction anxiously awaiting black carefully chairwoman commonly depths despair diane
    ##      difference everyday fed files @flgovscott hc inspired jfk @joy @leegreenwood83 #opioidepidemic
    ##      setting song steyer unhinged villa wallace fusion gps manafort michaelcaputo ok'd addition
    ##      alleged apprehension attacks benghazi corruption december defeating deranged dither earth
    ##      #endingwaroncoal events fulfilled halloween #halloween2017 highs imam lawyer liar
    ##      @luisriveramarin manaforts mentioned mustafa productionup shattering volunteer wishing
    ##      acceptable appointing blasio col craziness de diversity europes grieving handled horrifying
    ##      import individual lottery loved mandate moves @nycmayor #nycstrong @nygovcuomo nypd praying
    ##      precious professionally quality savings shaffer smarter supports systems tony unpopular visa
    ##      account animal apoplectic bergdahl bought brazile b's capital cheating cherished claimed
    ##      comey's degenerate deleted donna employee guess harbor harder hawaii impact land laundering
    ##      legendary lobbyists network pearl philippines pocahontas primaries proper rogue sergeant
    ##      skorea steal stole storming theft tv violation word arms benefits crown gentleman hands
    ##      harshly #japan lock milking prince sadness salman treating yield 99th amendment @billygraham
    ##      disposed @edwgillespie father humphreys nat'l northam participated rampant rok rx assembly
    ##      assurance beijing commitments commmitment completely defiance delegations determination doubts
    ##      embrace fatal forbidden headed hundreds interpreted landslide miscalculation #nationalassembly
    ##      peninsula #potusinasia pursued rejoining restraint reunited weakness 100's admins display
    ##      impressive incompetence jinping memorable productive representative would've 242nd achieve
    ##      #apec2017 beneficial bonds cultures da destinies diverse greet indo marines mutually nang
    ##      pacific prosper related reminded renewed #semperfi sharing sovereign staying travels unlocked
    ##      #usmc242 vision zoo alex #aseansummit azar hhs income indiv reducing 50's accurate #asean50
    ##      asia embassy excited fashion floor reciprocal basketball commit envoy fulfilling investigative
    ##      mass mechanism murder pitfalls road #taxcutsandjobsact ucla winding arrested cartel champions
    ##      collegiate crackdown criminal frankenstien harassment 'hundreds lecturing lesley
    ##      #ncaachampions photos pictures sexual sleeps stahl stocks tape analysts attendance boos
    ##      disappoint drop marshawn oakland predict raiders remainder responsible sits southern stands
    ##      suspend blue continued drumstick gobbler mountains pardoning participate ridge thanksgiving
    ##      wishbone aaa aircraft commissioner conducting contract ground hair lavar league locker nba
    ##      phones sentence shoplifting smoothly submarine twelve wasn basis boss club cowardly
    ##      defenseless discredit dustin erdogan existence extremist forms golf hemorrhaging inherited
    ##      jupiter magazine militarily mistake penalty shoot talks tiger tragic woods worshipers actual
    ##      contest corrupt distorted favorite mathematical producers provision receive result simpler
    ##      filling flooding #givingtuesday nasdaq notching roosevelt sentiment sp stadiums unchecked
    ##      amnesty andy aren authorities backbone behavior boycott bureau cea cfpb charles christmas
    ##      comcast comm demands earned effects emails endangered estimates excluding fmr fund gear giant
    ##      gov't growing hostage imposed inappropriate inspector @jba lack lauer launch lawsuit matt
    ##      mccullough misled mystery nafw phil practitioners provocative q3 scarborough shift st
    ##      terminating territory tha troop unsolved waste workplace 1k abound advancing associates
    ##      bahrain bright citations commercial condemned contempt credit destiny destructive documents
    ##      doubled #dow24k finalizing funny glowing glum guidelines harm installed justicedepartment key
    ##      mcculloughfmr milestone organ partners pipe preserve purchase reclaim regard rocket seeks
    ##      @senorrinhatch shed @theresa @theresamay virtual withholding witness beliefs block christ
    ##      committing comprehensive corporate disagree economists enactment exoneration felon generations
    ##      humanity ignite jesus merry overhaul speculating steinle travesty verdict weakly alan
    ##      dershowitz legal puppet refusal scholar bay bears ears escalante modify pensacola
    ##      proclamations staircase utah aviv directing jerusalem #periscope preparation tel battlefields
    ##      cry filled flies foes gratitude half harrowing infamy kilmeade oceans pauses
    ##      #pearlharborremembranceday sacrifice skies surviving tall wear witnesses bank customers didn
    ##      fargo fines guns #happyhanukkah incorrectly margin penalties period planning regs
    ##      @secretaryacosta severe @usdol dumbest hours lemon msnbc abortion accusations begging
    ##      contributions disloyal gillibrand #hanukkah healthy kirsten ring wasted deck margins meadia
    ##      omarosa originally played proved razor retractions stacked thin approximately dawn explorers
    ##      factories industries innovate inventors maze meantime pages patriotism pioneers red shining
    ##      task voices academy corps graduating helicopter hmx quantico squadron accident bridges century
    ##      closes conquering coordinating crumble declaring derailment design dupont exploration jet lose
    ##      maria mention miles motivate occurred pillars races railways scene supersonic tens tire train
    ##      travelled tunnels wa watches #wrightbrothers appreciated cathy confirmation expensing
    ##      greatness kevin mccarthy mcmorris rescinding rodgers scope unnamed wavered defeated meaningful
    ##      partner repealed strategically bosses desperate distract formula holidays influential
    ##      absolutely accomplishments appointments bipartisan bonuses charlie compiling desantis fighter
    ##      foolishly harvard judicial kirk list phenomenon rage showering source terminated truman
    ##      unexpected yale bogus develop essentially eventually @foxandfrlends garbage pile poised
    ##      repeals tainted approx beach estimated fighters firefighters inaugurated @jamiejmcintyre
    ##      occupy palm paramedics remaining roughly square apologizing arrests backwards bending ch
    ##      @dcexaminer disappointed dreamt est friendly fundraiser grief handed minor oil solution vanity
    ##      bit bundle charging coldest desperately dumber global packages poorer warming civilian
    ##      resources richly achievement agents ally authorization couldn dianne erna ero feinstein hsi
    ##      #lawenforcementappreciationdaypresident monumental norway occasions opposing possibly reversed
    ##      simplifying sneaky solberg stupidity underhanded combat controversial deadly disproven employ
    ##      fisa flow guys hidden huntsville #interdictact overdose personally previous produce quinnipiac
    ##      scourge smashed surveil suv activities announces blamed bonus canceled civic congressmen
    ##      defund derogatory disrespectful federalist haiti haitians historical holiday inflow intent
    ##      king's language levine located location martin observe opposite outlandish peanuts plenty
    ##      proclaim proposal relocating reversing ribbon risking setback shutting sweetheart trend
    ##      troubled truck trust born charges contrived convicted covering doj kazakhstan nazarbayev
    ##      nursultan pertains random reforms apple beating bump #congressionalgoldmedal dole forecasts
    ##      operations presentation recession witnessed booming chip conceived directly evolved indirectly
    ##      intended march natural necessity overcome reimbursement rivers saccone stars surplus
    ##      wastelands 45th abused accidents calif collection cr die #lovesaveslives marc @march mick
    ##      mulvaney preliminary reauthorize theme wrongly blakeman brad grade grading likewise powerless
    ##      schoen services acosta cave caved dancing gambled honesty jim lovers negotiating strzok table
    ##      texts worth zone bailey blaming bold chase cope county disney @govmattbevin holt humiliating
    ##      lisa marshall messages morgan peter powerhouse preston reduction samsung text understands
    ##      usher affirming boundaries davos eternal @nasa quest switzerland #wef18 00ame @alain berset
    ##      confederation #davos2018 increasingly @joesquawk kagame republic revival rwanda @squawkcnbc
    ##      swiss @wef @hhsgov #sotu approaching compliments tuning accounts average convince
    ##      @customsborder disgust employers file fitton hide jw ll misleadingly orator outlines
    ##      politicized rank sheet sterling targeting unthinkable @uscis closed confidential devin eagles
    ##      endure exposing grit leaves liars nunes paychecks recognized someday soooo uk universal
    ##      untruthful warner 21st cancels @colts edwin experiences includes linebacker outdated
    ##      preventable secures senselessly tragedies 40th bombshells @elonmusk #falconheavy ingenuity
    ##      inspiration @olympics sequester @spacex 45pm allegiance breakfast burnett creator currency
    ##      declaration declares eyes grace independence invoked kissinger #nationalprayerbreakfast
    ##      producer recite depletion extensive fame fortunately fraudulent included negotiations oligarch
    ##      paper steele tied @cpac #cpac2018 kruczek reconnect sue vain puzzle wouldn depleted unsafe
    ##      urgent bars catastrophe catch classmates collins creates disturbed doesn erratic expands
    ##      expelled forces grassley instances mentally neighbors parkland rounds sustain abandoned
    ##      bravest fronts impacted recipients shattered accusation brody cameras failures fit front
    ##      generic kissed lamb lobby map massively @mittromney mortgage @orrinhatch quote schlapp space
    ##      successor suggest timeline whining worthy #americanheroes background billy checks christians
    ##      decoration grandchildren lifelong ministry mourning newscasters religions scammed adept arrive
    ##      assets coaches concealed cowards deterrent emphasis gold #goteamusa guards hockey instantly
    ##      lasts mental midst mood @nra offensive #olympics pain possibility savage sicko solved students
    ##      takes trained training wayne devoted el ice killers motto profound quit restoring salvador
    ##      scripted @secretservice survivor town turnbull voice abbott ac aftermath boosting christi
    ##      comptroller confident consumers craddick criminality dan exercising glenn greg hegar
    ##      implausible jonathan @judgenapolitano lc miller patrick paxton railroad @senatorwicker sid
    ##      skeptical starr suggests supporter ted terms treasure trove turley angeles bible binion blown
    ##      conservative faster hundred isn lord los madison nationwide potentially proceed prosecutorial
    ##      rejected ruling schools sections sixteen spectrum stadium tent thomas volume yankee conviction
    ##      decimated dire emerge emerged fastest filings pace targets zones agony alec baldwin cute
    ##      darrell dieing funnier hammond impersonation jobless mediocre snl wars addicted #america
    ##      relocation renegotiation restrictive tariffs watergate wrongdoing concerned dialogue effort
    ##      hills inaction kidding maryland oscars parties perfection accumulated appointment choose
    ##      intellectual @markburnetttv property @realromadowney swiftly wisely adjusting cooperation
    ##      flexibility #internationalwomensday freeze impose @repbost @saccone4pa18 talked testing
    ##      township armed @astros barriers conceal danger eliminating european expert gazette houston
    ##      #houstonstrong mandated mildly ross wilbur aliens chosen coordination curbing depth gina
    ##      grants haspel importation jurisdictions miramar pompeo prey prohibit raging risk straight
    ##      studies tend unconstitutional blocked crumbling fundamentals #infrastructureinamerica securing
    ##      @senatecommerce testifying tools unfilled walked aviators bridge expanding fiu justin kudlow
    ##      larry leo monitor rushed unparalleled varadkar danny dean delivers heller nevada shamrock
    ##      tarkanian unopposed conflicts continent founded #nationalagricultureday tamed austin believing
    ##      bombing chemistry crazed drivers excoriate mueller probable professor selection smarts sought
    ##      suspect urged wether @ambjohnbolton assault biden crying drawing forthcoming giveaways
    ##      handover lawful outstanding physical physically threatens banning comment contacts devices
    ##      findings inconsistent insufficient item legalized omnibus prevent provided @howiecarrshow
    ##      inaccurate journalist modern nh underlying veteran voluminous wrote admiral affairs
    ##      california's defending dod hon intend interim korus maintained maximum md orange principle
    ##      ronny stevens @ustraderep wilkie boy concerns delivery disrepair forged governments postal
    ##      refined autism awareness bandwagon broadcasting caravans closing criticize easter egg
    ##      embarrassment fools northern option roll sinclair stolen stores superior unredacted walking
    ##      aid @anna anniversaries #autismawarenessday awards #balticsummit barack bourne briefed
    ##      broadcast bruno caravan cash cheatin costing cow crossing estonia fakers generous giaritelli
    ##      @grybauskaite honduras hq jeopardy @kerstikaljulaid knowingly lands latvia #lightitupblue
    ##      lithuania nominations obama's percent phenomenal procedure score sends @vejonis worried
    ##      youtube zuker 3rd 50th assassination #mlk50 represented rising uniting wing adds defiant
    ##      headlines salaries typically willingness @wvgovernor @77wabcradio advantages @bernieandsid bet
    ##      coffers considered developing epa founder organization perks pruitt radio surprised wto doral
    ##      masters reed secured stupid tariff client crimson enlightenment ncaa #rolltide senior tarrifs
    ##      technology tide transfers unwavering 12th 18th blood bolton businessman calculated conflicted
    ##      ellipse enjoys gas kills loyalists @nscsafety's partnering prescribed pundit raid rosenstein
    ##      shouldn skype speaker welfare agricultural approach atrocity brown clintons cobb cooperative
    ##      dignity disciplined hashoah hears historically #holocaustremembranceday jews memembers
    ##      #neveragain newspaper purpose slaughtered #taxcuts ball botch cradling den diego handling
    ##      issued lowlifes offered prosecuted slime surrounding thieves tpp devaluation disgruntled rates
    ##      aggravated cheated employment felons loopholes online pollsters porous traditional @abeshinzo
    ##      belongs breeding brunson concept contingencies deeper efficient formed infested japanese @jpn
    ##      nonexistent pastor persecuted pmo profitable quick revolution round sketch slippery trial bus
    ##      deploy deputies existed @gcsoflorida interagency #jiatfsouth @marshablackburn medicine
    ##      @naskeywest @norad northcom obstructing portion 'sanctuary southcom @southcomwatch student
    ##      #success threw 25th accepted annapolis artificially commencement counter debbie loaded memos
    ##      naval opec pakistani permanent schultz sea shadey ships test tests wasserman wendy condition
    ##      instructed maxing naive stamps stat #az08 brigitte @debbielesko french image @jimrenacci
    ##      linked republics soul ticking timeless tonight's #torontostrong abuzz cook cool detail dining
    ##      kanye lesko maga silent tim bid cup conspired coordinated define furious launches nevins
    ##      obtained performed pills prescription sailor #takebackday urge accomplishing alive bombed
    ##      bonkers cede comedian correspondents critics defying entrepreneurs filthy freshman headline
    ##      hilton hunts ineffective invade meyers migrant @nextrevfnc operate pledges seth stamina tester
    ##      triumphs unending analyst branch cargo chapter constitution crashed digenova dowd frame
    ##      granted hercules illicit intrusion jarrett outrageous powers redacting savannah screwing
    ##      @secpompeo setup trap unequal unfettered 4yrs admitting affair agreements ainsley arbitration
    ##      avail aware celebrities clifford cohen common damages daniels detailed disclosure distant
    ##      earnhardt entered extortionist hostages marks monthly nda negotiate retainer spring
    ##      transaction abiding disarm liberals releasing baker blankenship created diplomacy grenell
    ##      impacts injustice jenkins kerry lover mid morrisey pounds probe probers qualified revolt ric
    ##      salena shadow shortest till todd unrevealed wrongfully zito acquiring alway blew defective
    ##      praised sponsor topics awol candace criticized dewine excitement flying gentlemen greeting
    ##      guests represents socialist thinkers captured singapore att division launching medication
    ##      moved ongoing pharmacy buys compatible eastern exaggeration loving peaceful reflective
    ##      sickness spirits traitors #usembassyjerusalem walter wishers zte @cortessteve deb examiner
    ##      fischer nebraska solemn ultimate unsourced ballot barletta bobby box caregivers casey confirm
    ##      #confirmgina elizabeth exceptionally folding lou mirziyoyev uzbekistan warren embedded farm
    ##      imbedded informant liu macri mauricio premier requirements transforming unleashing unwarranted
    ##      vice abundance animals ant apparently asman begrudgingly believes blessed chances cox
    ##      dominance fe @greggjarrett grieve guterres hot hour implanted lowering nio personnel
    ##      #prisonreform redemption santa terry withdraw arkansas asa @asahutchinson bongino briefs
    ##      debacle denies destruction filtering genesis journal panicking prevention tight jae
    ##      @martintruex @nascar abandon autoworkers bethpage boot capitals completion crippling
    ##      describing duped enabled heartbreak infiltrate lahren napolitano nicely normal practice
    ##      rendered resistance roe #sbagala @sbalist scandals seemingly spygate spying structure tomi
    ##      wade worlds admitted britt inflicted jung master #medalofhonor operator ranks regulatory
    ##      #s2155 seal slabinski #spygate surprisingly warfare alluding anchors aweigh commencment
    ##      competing editor enduring expenditure flourish glory godspeed guarantees hemingway legally
    ##      mollie @navalacademy nurtured protest reopen rip rooting sets sole spies touch widespread aide
    ##      backfires cages chairman chol focusing gathering investigating mistakenly scandalous tweet
    ##      wiretap afterward arena barr comments cure disparage experimental expressing frustrated
    ##      frustration gee gowdy iger illnesses improve @kimkardashian moments patients recusal
    ##      @repdandonovan #righttotry rockin roseanne sentencing shown staten threatening treatments trey
    ##      valerie betrayal dinesh infiltrating infiltration informants offended pardon souza unforced
    ##      weren bee lumber samantha timber #500days 500th freeman judgeships scholars soybeans astros
    ##      band busted championship chorus clemson cubs engagement escaping faulkner harris music
    ##      needless proposed recuse rescissions @rogerwicker ruined separating shares sport track weaker
    ##      74th acceptance airplanes allied burglary ceejay charged cop craft dana dating executed
    ##      facelift farrell federally grave ill invasion jumped locally managed merrily metcalf nixon
    ##      pale pundits recess referring rohrabacher sighting spotted steer surgery tradecraft undermine
    ##      #vamissionact violations wave accused alice awan divert fairness hook humiliatingly imbalance
    ##      ironic listed monetary multi noon payoff plea proves reciprocity target trading utilities
    ##      captain informing ovechkin popping practiced stanley straightening superstar 17b bragging
    ##      clobbered female fraction hsien laugh loong losers minimum missle slowly staffs stoped
    ##      surpluses 30pm arrington boxers drunk duluth iq katie laxalt mia movies niro punch punchy
    ##      sanford #singaporesummit sites unhelpful wake wednesday assuming claire conciliation corey
    ##      downplay embraces flew games hawley heads heidi josh kaine @kevincramer limit luxurious
    ##      mccaskill promulgated roof rv safer shout sleep stewart stiff upset ushered 243rd charity
    ##      disciples #flagday guts lingered loudly remembering schneiderman settle @usarmy agent breakup
    ##      claude cruel dynasty forcing ig implying instincts jean juncker lawn levin minions mob raised
    ##      texted tusk unannounced berlin bias boarder counterparts initial rocking spin strozk tenuous
    ##      violently affect arrest #changethelaws horowitz infest migrants @nfib #nfib75 obsolete
    ##      suggesting applauded breeds correctly explaining fabulous felipe laughed letizia mentioning
    ##      @petestauber queen recommending revealed @secnielsen separation #stopthebias vi weakest
    ##      affects columbia endrosement goodlatte jacket mititary password phrase @potus's reciprocating
    ##      refers solves consistent exports fairest hoping ignores intention jeanine loyalty martha
    ##      output overrun pathetically permanently pirro placing reliable rely roby separated believed
    ##      canopies claiming clay @clayhiggins18 cleaner cleaning complicated davidson disfunctional door
    ##      doors drunken expose extraordinarily facilities famous harley hating heat hen huckabee
    ##      humanized inside jimmy @jimmyfallon manythousands max maxine messed mukasey paint patient
    ##      @repclayhiggins restaurant sarah selling tracks ultimately uninvolved waters whimpering
    ##      windows aura chided chronicle conner defended eliminate finishing footprint garlin
    ##      @henrymcmaster lieutenant murl rightful surrendered thailand unbalanced unlimited upholds
    ##      anthony belong colleges competitors conclusive cramer crowley deciding donovan fear gary hater
    ##      kennedy marcelo mitt #ndsen nicer peggy player portugal rants rebelo @repkevincramer romney
    ##      scheels @senjohnhoeven sousa turmoil turnberry universities addresses amy @amykremer anarchy
    ##      discovered electronics elimination examine gig groundbreaking kremer leftwing posting publicly
    ##      selfless servants shady shameless @sharkgregnorman unrelated conscience @govwalker newsroom
    ##      pulled raises retirement shocked walker andres benefit netherlands obrador rutte ability
    ##      anonymous capitalize capitalized charities citizenship classic cycle demeaning discgrace
    ##      elelments endured flee greenbrier infestation introduction iranians irregularities journalism
    ##      noted pore priding privacy quoting ranting raving resort ripping rough sweat tweets
    ##      administrator agency assume deputy duties environmental insane reasonable resignation wheeler
    ##      bunch claim crazies figured filed liberating montana smears wild bargain basement breast
    ##      #centuryofservice denied exerting feeding guardians handshake increasing indispensable
    ##      malnutrition ny posture poverty warrant applaud blueprint brett delinquent emerson epoll feet
    ##      freed happily hikes impossible kavanaugh navy reimburse rolling seals @secazar thai
    ##      treacherous beans defied destroying fashioned fell influenced layer loses ol pipeline request
    ##      soy subpoena traffickers consequential dodging instance latinos minority #nato2018
    ##      #natosummit2018 note subsidize gaetz brighter exclusively finland foolishness helsinki
    ##      #helsinki2018 obligations pursuit partisan vast abolish acknowledged appropriately bothered
    ##      boxing cheer derangement dobbs flood fruition gates improved kemp @kilmeade prove
    ##      @repkevinyoder syndrome wished yoder backup confrontation google implementing midterms
    ##      novartis proliferation recklessly @rt slapped competitive currencies downward hurts
    ##      manipulating manipulation penalized recapture severely tightening antitrust avoided cautious
    ##      consequences cooperated designed document @judicialwatch misconduct negatively pretext rouhani
    ##      suffer butas heels #jobs negotiates #perkinscte piggy planet robbed showcase unknown vfw
    ##      #vfwconvention @vfwhq #workforce abruptly believers benefiting brilliantly clients covers fcc
    ##      feeling @irf #irfministerial @junckereu marchionne merger prosperous #religiousfreedom seller
    ##      snipping @statedept subsidies taped tribune understanding @alandersh buying clarity complaints
    ##      detainment discriminatory dubuque granite @iagovernor illinois impeaching lng prominent
    ##      reynolds @u 7th alene balderson cabs cast distinction duerk emotional graduate greeted jam
    ##      @johnjamesmi kustoff notoriety reopening retained servicemen taxi troy @troy turnaround
    ##      abolishing @giuseppeconteit medicare beaten brothers circles decent dry globalist koch
    ##      overrated plastic ramifications @repdesantis reuniting @senjohnbarrasso tampa viability
    ##      wyoming adp alfonse barre capone confinement darling dirt estimate goals illusion improper
    ##      july lovely objectivity operative pastors payrolls #pledgetoamericasworkers proactively
    ##      recused smocking smoking solitary stain thiessen wilkes aug beloved blowing honolulu
    ##      incredibly @nrcc participants predictable soil stivers blackburn extinguishers @greggutfeld
    ##      letting marsha nasa overseas praises 'pro spacex @thefive type compromising connected daily
    ##      diverted farming fires gather kobach kremlin kris magnified material obtain ocean readily
    ##      spreading stabenow tree utilized 236th biting purple ratchet vanguard @lenaepstein schuette
    ##      depends ellis entrapment @grahamledger jenna @lindseygrahamsc overt whatsoever yoho 4th
    ##      authorized distance doubling knee lira slides suspended turkish veered brooks communication
    ##      drum @johnkasich koepka legitimate lowlife majors miss pete redone sham significant stauber
    ##      tamping unpopularity warfighters animus attributed bruce conducted criminally fundamental
    ##      interviews machinations nelly ohr pretending prosecutors quotes toughness underpinnings
    ##      vocabulary activity assistant bryan circle competitor correcting cutter demoted enters haywire
    ##      leah maker originators permission proceeds skinny stefanowski steil swecker vukmir wrongs
    ##      aretha attempted betrayed boston creep globe gong inflation interfering koreans notes outlets
    ##      papers plain shopped strip unfortunate 11th actress admits assessment blinders blunder breach
    ##      burr celebratory chair chocked choked cmte conduct constitutes cuomo's hightax intellectually
    ##      judgement limited nc outcome payne purport pushback quarterly ridiculously section sobbingly
    ##      survive twenty windfall brings enjoying fentanyl firings firmly hack heroin issa oversight
    ##      poison poisonous ruining synthetic accomplishment admonished debating deblasio dennard denying
    ##      @dianeharkey easier foster friess gubernatorial handle imagination mudd odds @parisdennard
    ##      phillip rails reiterate revoked slogan taxing tired unglued unpleasant weird yorker applied
    ##      assemblyman bull counts deporting dov gracious hikind #hurricanelane longtime @nwshonolulu
    ##      plead resident retain settled ten updates africa @cindyhydesmith expropriations fights forgot
    ##      @mikepompeo4usa seizing seizures additionally censorship considerations contractor @drudge
    ##      giants gravely improperly potatoes raves redaction resolved respecting silencing sufficient
    ##      ducey endlessly @jimbrownnfl32 kenya kenyatta margaret tuesdays @ukenyatta villains addressed
    ##      conservatives controlling hiding pops smile suppressing viewing admit bond @carlbernstein
    ##      chooses clearances commodities considerable differences disputes email employed exercises
    ##      fertilizer focuses fuel gillum housepresident mcgahn mcsally missteps morphing nonetheless
    ##      pilot providing rival rotten scary sloppy tearing torn 401k braun @breitbartnews @bretbaier
    ##      clouded collier davis degrees evansville fluent frantically fudging function haul lanny lester
    ##      nellie oklahoma perpetrate portray presiding purposelyso referral rosemary smooth stitt suck
    ##      throwback unbelievably wildest blatantly picking violated decorated deserving despicable
    ##      excuses fumbling #gordon inflict intense journalistic jurists kyl @nhc @presssec renowned
    ##      scrutiny timing unethical weinstein woodward amir billings bolster boring boycotts divide
    ##      exact #farmbill 'fiction gutless kuwait literally mdt nike 'product retarded retribution
    ##      sleepy snap someone's southerner treason atlanta cosumer declassification detractors draining
    ##      faithfully journalists justices remarkable rosendale unnerving vehicle writer convictions
    ##      deport felony frequently onslaught quoted trick assaults barrage caralle choking cooper
    ##      directions engine flailing grain katelyn magic minus mobilizing precautions salt
    ##      @savannahguthrie storms @todayshow wand warnings breitbart documentation giuliani holder
    ##      reveal rudy @saracarterdc #september11th shanksville skill collaborated demonstrated dipping
    ##      electricity evacuation florence hitting #hurricaneflorence impeach imperative inaccessible
    ##      laundered pluses relates snuck supplied unappreciated aptitude banker bankers cesspool
    ##      collapsing deaths dimon jamie mad nail nervous successfully tooth @usnationalguard #cajunnavy
    ##      couched detriment #florencehurricane2018 registration undercut #americanpatientsfirst answered
    ##      awful bargaining chargeable clauses enterprises flowing formal gag medium morici pharmacist
    ##      profit profits redacted registering remove tariffed thriving #trumptime unnoticeable 71st
    ##      actively andrzej attacking basically biodefense biological defenses disclosed disproportionate
    ##      duda @prezydentpl range retaliation targeted warrants chaffetz depression dismantle expect
    ##      ferrara inspections jason pad playbook presence recommits studying closure identified
    ##      @jaywebbernj lacking mcdaniel nash nominated perdue pfc vernon vibrant william bang
    ##      @dannytarkanian @deanheller declassify perceived pow purposefully recognition reputation
    ##      selflessly served @alsisiofficial ashley @moonriver365 73rd acquisitions array board's
    ##      favorability gallup goodbye hassan kiss requests avenatti breaking des jump moines propaganda
    ##      register wheeling zeldin riveting deficiencies reduces usmca braved shurer tristan 102nd
    ##      anytime blowout bryant formidable glassdoor @petesessions recovered @sandyhook separately wage
    ##      bishop counterterrorism harsh intellect @jason2cd @jimhagedornmn @karinhousley lowered sheriff
    ##      @tomemmer treatment uncorroborated widen advance elevator identical rochester screamers soros
    ##      #troublemakers adventure @agpambondi app columbus darkened deserted doorsteps expanse
    ##      #fortifyfl hardened #iacp2018 leos navigator statewide voyage 30am algae cat elements
    ##      everglades haley #hurricanemichael nelson nikki okeechobee pan relentless reservoir a17
    ##      colorado erie hvvhsmbg7s lined reseach stapleton talents en highway jacksonville overflow
    ##      panhandle route shake troopers 10pme chants lacrosse mentor #nationalfarmersday nepotism
    ##      safely #usmca visiting arabian ben citizen enrollment outlaw priced screens seniors shapiro
    ##      venues 8x astonishing channel cherokee color consulate @cvpayne danials dna fees glen headway
    ##      hindrance horseface indian minors pawn perpetrating pleading regan revealing simpson slammed
    ##      stormy suggestion throws trish unaccompanied versus ap barometer beto blow burdensome canley
    ##      dunn @dunncampaign entering guatemala hammered horrendous midterm neal protections rourke
    ##      supply truthfulness unimpeded wpost balance behaved dave @davebratva7th hardest #magarally
    ##      missoula population saudis 2a biggs desires @gregformontana #jobsnotmobs leftists
    ##      @mattformontana retire 23rd alerted @andrews attempting births cleanest easterners emergy
    ##      immigrant jba mixed presides @rondesantisfl routinely shock @toyotacenter erik faso institute
    ##      paulsen @reploubarletta unpaid webber wrecking abrams advocate concluded @leahvukmir
    ##      @scottwalker sooooo stacey wholeheartedly apply armistice barracks brandon @brucepoliquin
    ##      careers cellphone cellphones centennial chanded commemoration description enlisted hateful
    ##      @johnchrin judd lloyd @lloydsmuckerpa maine @mikedunleavygov murdered occasion refer reverent
    ##      theaters usage apprehended attorneys blimp bomb bombs comparing custody figures fills lowly
    ##      mailed momentum obstacle org patriotic profile ship slows spate stifled update #ybls2018
    ##      bolsonaro brazil flame harmony hostility jair newly rothfus substantial tallahassee thief
    ##      @cathymcmorris cedar clone cordray @denver4va @denverriggleman disillusioned dovish @erik
    ##      @erikpaulson ingraham @jasonlewis @kevinyoder laura @mikedewine pause produces rapids rod
    ##      @rodblum @troybalderson wren anchor babies birthright compassion drives inspiring jurisdiction
    ##      mobilized opinions reid sane sincere sorrow staged thereof treaty warmly breckenridge burdens
    ##      cable combined enormous hospitals polis strain tops undermines @walkerstapleton carol
    ##      @carolmillerwv @hawleymo knight @morriseywv southport allen @andrewgillum automatic booth cal
    ##      claudia @claudiatenney coliseum crashing @debbiestabenow grad halt hottest nightmare #ny22
    ##      overtaxed radar requesting rhetoric ridden screeching sudden sums @votemarsha @braun4indiana
    ##      cape deception epstein faithful gem girardeau hugin hultgren locations randy rumor sovereignty
    ##      tireless waltz awed campaigner capitalist @davidasmanfox disapprove embracing improves
    ##      incumbent matthew owe pulls stein whitaker bar addressing @briankempga broward #broward
    ##      conceded counties elias falsify finding insulting miraculously signatures stealing subsidizes
    ##      suggested tabulation unelectable ballots breath count gasparino hallowed hasn headaches
    ##      infected mighty portions prospect resting struggle alleviate bases buddhists cemetary cemetery
    ##      characters concede diwali diya emmanuel festival fly german hindu jains lights nationalist
    ##      observed rain rightfully sikhs suffers visibility wine wines appreciates fresh genius heroism
    ##      poured steelworkers accountable envy horribly landmark @mbwdc mistreat nuts ruin shouting
    ##      workings sitting bin climbers climbing fence footage laden osama pakistan argentine discovery
    ##      eventual activism afflicting circuits copy detain dividing garza hector imitation judiciary
    ##      loudon @mgoodwin michelle nypost @realdrgina roberts sanity shocking unwise wolf contrary
    ##      jealous @senchuckschumer @stevenmnuchin1 @ustreasury @60minutes innocents jails kinds mumbai
    ##      portraying preamble raffensperger seperation @votebradraff waving worldwide actuality builds
    ##      @gm hyde prosecutor smith acid brand brenda chicken dress dynamics honorable intimating joseph
    ##      joy mill #nctl2018 snipes southwest studied tactician ton trucks washed wearing arthur
    ##      atrocious bmw capturing @dbongino experienced gorka @judgejeanine originals previously
    ##      recommend renovating sebastian @stephenmoore subversive #trumponomics views crafting decide
    ##      demonstrates expense fundraising lightly oversaw partisans ronna roving spare whiff
    ##      beneficiary leap @lopezobrador purchasing reduced stating taxis uncontrollable blair
    ##      conclusion decorations elegance explain fatally flawed lighthizer navarro polluters pollution
    ##      precision protestors #remembering41 strides whitewashing collision historian @iiimef periods
    ##      @usforcesjapan wead ballgame bracing bye clears communications corsi demanded jerome lazy
    ##      leakin lyin molly nauert perished predecessor prosecuting prosecution protocol scathing
    ##      spokeswoman surge @trish violate walled weaponizing weissman ayers certainty contribution
    ##      interviewing tying acknowledging announcements anton bleed confirming disease exploded forming
    ##      makeshift reveals strictly urging vying 1pme 382nd airing banned brzezinski catching embarrass
    ##      embarrassed #guard382 @harrisfaulkner misstatements overrode @richardgrenell scare temporarily
    ##      reply slower @tim burning deductible facilitation implement unjustified useable dopey examined
    ##      grounds illiquid isikoff meaningless names reputations sensational slowed yahoo artistically
    ##      bi cats concrete dogs @dougducey earn @marthamcsally politic rant rave rebuilt rent slats
    ##      yorkers amuck carafano definition doomed #emmys #firststepact policeman refugees retiring
    ##      sight surprise #tbt tenure western barrier conjunction cooked countering counting dark
    ##      detection domestic geniuses protesting redesignates slat @stevedaines tata tech wheel
    ##      withdrawal appointee bells drones golfer immensely lashed loading mcgurk mile militaries
    ##      populism putt regimes reminder shutdowns subsidizing sympathizers tammy wealthy whistles asad
    ##      actors exceeded expecting hat hats manhunt neighborhood pesident realized supteme
    ##      traffickinggang universe looses parnell reaction saddled bite endless immoral prefer remnants
    ##      tactics wound payback achievements motive overdue pillsbury portrayed shanahan webb charts
    ##      declare lunatics prudent compete glyptis normalized appreciation forest forrest apologies
    ##      cancelling contents custom intransigence overdrive politely respectfully slamming @stevedoocy
    ##      tantrum temper assured cheaper dear declined diary drastically feature h1 holders @kusinews
    ##      options replaces simplicity skilled variation advise #afbf100 beer bighorn bozo brees buchanan
    ##      cease chill combination crazier deregulated disunity dressed drew garb gentle kitchen
    ##      militarized orleans portrait qb rested saints sex smash unsustainable chattanooga disgusted
    ##      drone eaters hamberders hamburgers volkswagen countrty corke cross excursion farms grapes
    ##      @jiminhofe maccallum mouths perjury ports rancher rugs smuggled spewing unreal captivated
    ##      catholic corrupted covington encounter inquiries involvement judgements marist native npr oppo
    ##      pbs prevents rudely sandman sandmann smeared sussmann symbols teen unusual acknowledge
    ##      baseball deroy #entersandman export exporting @fitnessgov guaido #hof2019 illegitimate maduro
    ##      mariano murdock rivera stoppable unanimously venezuelan barely cleaners @dougwead dreamed fdr
    ##      lbj prerogative rockets sotu thorns venue antonio conman coyotes devote classes expanded faced
    ##      @gillianhturner headwinds howard introducing @jillianturner @johnrobertsfox literacy revived
    ##      starbucks cliff colder dick gofer insider midwest pretended sims staffer temperatures waming
    ##      windchill assumption caliphate contemplating passive proceeding protests regain reinforced
    ##      revenues separate contributor crises inflicting mischaracterized murders negotiators
    ##      termination unresolved bestowed bust conclusively cory gardner gifts gou mysterious ousted
    ##      portman rediscover rob telephone bernhardt interior criticizing displays festivals gatherings
    ##      hopes joyful lunar aspect continuation pols #wgdp aspen astronaut developed dingell intensive
    ##      manager robinson solomon strived @thehill #40yearsoffailure checkers factors @jessebwatters
    ##      loosen mix paradise paso produced repression righteous @tvanews viable aspects backgrounds
    ##      birthplace creed hooked parameters religion shelby finances @mcsheriffs @mjrcitieschiefs angel
    ##      brass bullet educators ensuring memory pretends recommit reviewing aurora circumvent grabbing
    ##      obey bathroom bureaucrats checker coup dictatorship hanson jt meadow overruns overthrow
    ##      overturn plotting socialism victor absurd basic documented #finishthewall francisco hoda memo
    ##      muthana schedule shortening substantiate suing tantamount 5g 6g advanced cornyn insulted
    ##      @jussiesmollett lagging racist technologies earliest @kimstrassel uplifted visionaries
    ##      accomplish arab burch captives composite cutsetc diplomatic @djohnsonpga emirates fragile pga
    ##      recovering reflects relax spike stages thrown yemen executing hanoi infant newborn awesome
    ##      denuclearize disbarred guessing #hanoisummit hourly resuming vietnamese hosts phuc trong
    ##      contradictions elmendorf manuscript misinterpreted misrepresentations mistreatment pork
    ##      publishers richardson substantive symbol testified traunch @arifleischer contributed
    ##      @devinnunes drills fraudster @governorkayivey @govivey mtg nk reimbursed sore tornadoes walk
    ##      admission aw #bebest bezos cured degraded @ericbolling expedition extinct fat fda fishing
    ##      gottlieb harass hiv ilhan @lauraingram lobbying mccarthyism mccathyism nadler net omar
    ##      overreach petition pt purchased renewal shucks sorely @starparker subjected suicide
    ##      unregistered urban @westgatevegas widest accusing atrocities condemn constitutionality copying
    ##      hans inconceivable invaded issuing precedent punctuation semitism shameful shapes sizes
    ##      spakovsky subpoenas von weirdo apprehend contradicts fiscal apprehending contracts
    ##      contradicted equally finalist instituted pled @repmiketurner shaping unimaginable daylight
    ##      disparagingly albert carbon climate complex complexity dioxide einstein greenpeace harassers
    ##      jexodus mit pilots pipko science split weather worsts comedy coumo executions impeachment
    ##      @jasoninthehouse leno misdemeanors overthinking photoshopped propelled successes topic
    ##      transcripts unanimous unproven carson constitutional deniers expressly frantic loftus scraps
    ##      @toyota untalented vetoing ardern bravely closures @govricketts jacinda massacre mosques @oann
    ##      sympathy zealand donate economies exceeds floods @govkristinoem invested legit lordstown
    ##      managing meets quarters salary suffolk tongue uaw yearly 250m amazingly believer defamation
    ##      dispel @thedailybeast brilliance conway dash flat @ford kellyanne lima marathon northwest
    ##      resounding significantly tank campuses emerges fades golan heights @kennedynation plot
    ##      regional stability #worlddownsyndromeday admire bahamas breathing caves dominican economist
    ##      glimmer hegseth lucia susceptible testament affiliated cooperate ideologies loyalties offers
    ##      recognizing @billhemmer @byronyork delusion reaffirms scorned sticking atkins 'collusion
    ##      comprehend dirtiest duly @google overstate screwup @sundarpichai travis amend breakdown
    ##      deductibility jussie legislatively smollett unlawfully unusable prize prizes pulitzer
    ##      quantitative throug timed asylum awaited census conclusions othershave @senbillcassidy
    ##      @senrickscott worthless afforded asd corruptly cueto del equal grossly healtcare @httweets
    ##      @jensstoltenberg miracles @nato overcapacity reaffirm satisfy shifty significance usable
    ##      breakthroughs 2yrs calexico carranza checked coverup hagedorn jovita magnificence mcmahon
    ##      meaningfully @sbagov shipped stops supersede treasurer trolling @wsjopinion apprehensions
    ##      baylor captures dame guide @katiepavlich kidnappers notre tourist uganda adversely airbus
    ##      assaulting b2 ballabon carrying congesswoman imprisoned manhattan rail slightly stephen
    ##      underneath zoning bibi crosby dramatic flags indicted stunning treasonous waived afford
    ##      breathe brexit disarray @erictrump @laraleatrump mindset occur reeling craig gregory
    ##      legislating @repdougcollins 'right 'spying strassel tiny wsj's branding bureaucratic cathedral
    ##      @cher cops features huawei instigated populist puff rebrand semitic sports @stevehiltonx
    ##      tankers telecom @themasters @tigerwoods 27th audience cdt column concocted contingency
    ##      dealings devils @donnabrazile drought formalize resch river @senmcsallyaz smiley bethlehem
    ##      finalists icc offering @pontifex realclearpolitics studio stuffed trumps chose framed gaslight
    ##      nolte cain closest conte exculpatory giuseppe herman @johnkerry lanka logan maithripala
    ##      @marklevinshow ranil reserve sirisena sri tables wickremesinghe adult assisting beg criticism
    ##      els ernie fairer fdn immune @jack krugman #nationalautismawarenessmonth nosedived obsessed
    ##      offset outreach reciprocate recreation rewarded slot struggled @thebig therapy @twitter
    ##      unsuccessful writings accuses achieving caldwell cartels cortez costa diversionary glove lay
    ##      legislate ocasio occasio recommended @rxsummit #rxsummit2019 @senmikelee smugglers tactic
    ##      adverse composed gate cheif circumstances gasoline negotiator #nraam projections q1 relevant
    ##      smashing terroist 1970s apparatus cartoon chabad dues expenses finger goldstein internal
    ##      litigation membership poway print rabbi sucking yisroel cuban dose easing embargo incessantly
    ##      @joeylogano labs militia nearby peacefully penske promptly stimulus @team wonderfully carrier
    ##      decommission grips overriding updated goodness inflationary innovative strengthened unite
    ##      wondrous attempts avoid consuming diamond falls fred @govparsonmo @govpritzker ia il keller mo
    ##      platforms rebelling rebuffed reverse silk watson albence @armywp extent gazan grab hamas jihad
    ##      knights misery pending measures british 'case casino depreciation developers economically
    ##      estate 'forgotten grieves grown intervening mount @newtgingrich offs pullback rushmore
    ##      @secbernhardt shelter @steveforbesceo trace unspeakable usitc workhorse abuses donation gilead
    ##      prep @redsox supplemental uninsured admirable automatically bottom candid carly congenial
    ##      constructive fiorina quicker redo reneged sells sleepycreepy soared spouse stick waivers
    ##      amputee arm baton degree deserved donor hip illustrated insensitive intergovernmental jessica
    ##      lakes lotte mars miserable misuse noticed peck quadruple retaliate shin @specialolympics tlaib
    ##      @tomfitton transplant unexpectedly updating upholding watershed beneficiaries buyers dumped
    ##      formation g20 hackberry osaka pumping sempra superpower @tomcottonar wealthier brain sharper
    ##      adopted @billdeblasio transform unveil comfortable hombres loophole realtors scattershot
    ##      sentences sourced telease americas batch cubans deutschebank error heap idly montoursville
    ##      pulling redos @secretarysonny subvert trash venezuelans 40m aerospace attitude barrett bevin
    ##      corporation #flyfightwin johnny megan shahira appearing boardtoday chases copious declining
    ##      demolition disappointment drama fish footings forgiven goodwin goose irrelevant @mattbevin
    ##      @newyorkpost nothin prop pseudo simultaneously suspected assess bjp damaging disappointing
    ##      equipped #flagsin geared helm modi @nws overs polite refinish resilient squander store
    ##      @usacehq woke accordance @arlingtonnatl congratulated cotton delegated departed diminish
    ##      emperor institutions lows majesty predicate @quinnipiacpoll recommendation rollins sellers
    ##      @shannonbream solider spot stammers cleanup @govmikedewine @govstitt idiot managers overnight
    ##      softer struck 10th 4am acquisition conspire crossed disappeared disclaimer gradually
    ##      graduation ratcliffe remedied thad uss @af amway decriminalize dozens equals execute
    ##      homosexuality imprison lgbt lords navajo orientation pinto punish talker talkers uncovered
    ##      @att cameron firms hassett height kahn reminds royal @sadiqkhan shackles shipment shipping
    ##      subscribing visible #usstatevisit @bettemidler bluff bluffing budge commemorate corrective
    ##      corrup darkest #dday75 flops measure nechama organized plagiarism resume reuven rivlin scammer
    ##      stature ca cadets #dday75thanniversary ga legend lived @maddow ne promotes ramps unyielding
    ##      errors factual incapable omissions anticipate body borderswhich eastman illigal @ims @indycar
    ##      reinstated @simonpagenaud assure bashing bates cows dagan damian deleting devalue devalued
    ##      disadvantage e15 ethanol euro forging harden lessen powered relocate revenue similar sorial
    ##      suppress ticket agata ali argued ayatollah bribery compelling constitutionally criteria ds
    ##      entertainer exemplary inmates ip issuance khamenei kornhauser loco @markwarnerva mclaughlin
    ##      @ncgop oman permissable plays pollster powell properity purporting @repadamschiff shafted
    ##      sidney spell thread trusted wales whales whatley advisement #armybday cancer coatesville
    ##      glitch guest harp hewson hras lockhead @lockheedmartin marillyn @markwarner natalie objectives
    ##      ridiclious @sikorsky @abcnews defy motley stockpile swing woodland anemic @business dax draghi
    ##      ecb escalating esper guitar illicitly incidents kickoff mario naming pat respective #trump2020
    ##      cowardice depraved fate hicks morally ratify reprehensible #rigged spine virtue bailed cocked
    ##      compliant perpetrated proportionate ratified sir strike unmanned weakened yelled compensation
    ##      #keepamericagreat lanes slowing sponsoring stubborn bellavia efp fallujah ied ignorant
    ##      mightiest #moh obliteration squad stepped thom tillis checking corrected 'crisis #demdebate
    ##      #g20 incentives ing invaluable @joebiden #kag @kayleighmcenany leagues lotter manufactured
    ##      @marc @meganrapino @mpinoe @parscale predator rapid #rtm2019 smuggling soccer @teamtrump
    ##      @timmurtaugh @trumpwarroom @chucktodd depict flyers unprofessional withdrawn credited dmz
    ##      exhausted #g20osakasummit advocated bludgeoning cap harassing kazianis legislators mercedes
    ##      @mercedesschlapp persistent sand stuck @wallstreetjour bestselling conspicuously directors
    ##      donating economics empower flyovers founding generosity grucci hotels indemnify judy louis
    ##      modulated phantom reconstruction shelton tells unfreedom waller zoldan aghast andrea bitten
    ##      breached centers detentions donated dropping dummies eddie enrich fourth gallagher greats
    ##      nurses planes refitted @robertjeffress #salutetoamerica tanks violating visitors 19th ari
    ##      #july4th monument daytime earthquakes exercise fundamentally @gavinnewsom hired shep doocy
    ##      estuaries foisted hrobak participation pathway pompous stamp strained @tamimbinhamad
    ##      tremendously accepting charitable depot emoluments enriching expire flagged followers
    ##      grassroots hydromatic induced kidney marcus @rudygiuliani shop #usmcanow 1024th 24th alfred
    ##      @alfredeneuman99 barrack bites blacks brits compromised deutsche discrimination disloyalty
    ##      @drgregmurphy1 embarrassing endorsing escalator explore germans hovered humming inhumane
    ##      @jonvoight maligned minorities #nc03 newman prestigious sikorsky #socialmediasummit stable
    ##      stimulate tale banking bernadette bitcoin censors challenging charter crypto cryptocurrencies
    ##      dependability dependable derco dominant drumbeat duck facilitate gatekeepers graciously
    ##      hagerty jane jessie lawrence libra lurch @pagop pizzella @sebgorka seizure similarly tabas
    ##      tendered triumphant unlawful unregulated victoy volatile accuse adversaries antiquated aoc
    ##      camps communist communists concentration condone confronted congresswomen criminalization
    ##      detention devaluing foul progressives qaeda reject spewed unbridled unchallenged
    ##      unrepresentative billionaire bone controversy investor laced petrified rebuke thiel wedded
    ##      apocalypse appalled bitterness bottle cranks exerted falling horsewomen ignore #maga2020
    ##      mastermind origins predicting shampoo wack wicked amphibious apprise banner boxer charlotte
    ##      #cheer4usa circumstance defensive deported enterprise hormuz incident indictedand paycheck
    ##      rivals robbing @specialolyusa strait #ussboxer waves @worldgamesad acted carrie chant chin
    ##      columns faulty friedman gene incarceration @kanyewest mouthed rhode rocky scalia severino sort
    ##      unofficially williams advisors cheat compromise dilapidated inexpensive inexperienced
    ##      manipulate meant misguided needlessly pump reams tightened troublemakers wildly ain athletic
    ##      boris dearly @esperdod foot morgenthau pill polled remittance riders semite softball swalwell
    ##      @tpusa @trishregan trumper @abcpolitics fabricate impeded nature pinning reclaiming renewing
    ##      shambles #shiftyschiff specifically @terrymoran @chrisplanteshow #freerocky fven harassed
    ##      @jaysekulow rooted stefan tolerated tossed conservatism @dhsmcaleenan digital gianni gianno
    ##      guatemalan injunction lamestream liberalism mac neck overturns q2 richest sincerely waiver
    ##      wavers wrapped zoom baltimore commonwealth cummings daniel @djaycameron elijah equated favors
    ##      forgetting gilroy intimidated labeled measured pickens pocket shaped sharpton staggering
    ##      statistics @thomashomanice troublemaker whites @andypuzder arresed assumed cards congresses
    ##      divorced hampshire highways id insure jamestown juice klacik marriage married posing preside
    ##      pumped sanctity screams somali stiffs transportation tucson uncomfortable @voteridplease
    ##      wikileaks aggressive difficulty generel @govofco @govrondesantis hadn insinuated lengthy
    ##      lesser neutral rescind sinkhole socialists spencer tubes unbiased admirably @americanewsroom
    ##      bullard drooling #kentucky quantities undo chariman @chuckgrassley dni #kag2020 pupils
    ##      @senjoniernst shook slander balanced dayton decisively depress heal marrying propose
    ##      punishment responsibility weaken wounds asian boosted cernekee @cnbc corner engineer homan
    ##      @jimcramer @kevinolearytv @peterschweizer reign sandy sounded suppressed ttump antifa
    ##      assailing astounding bore central clause clicks contribute curve immeasurable meddle
    ##      misrepresenting obligation quiet racism resemblance shepard sherrod trounced urges vetoed
    ##      whaley wide yoo 15th amazed blagojevich brien castro caterpillar coats coincides commanded
    ##      commuting deere gordon joaquin juaquin maguire mouth sentenced signals slam trips elite
    ##      inflame racists explosion negop scaramucci skyfall columnist cursing curt fredo hong kong
    ##      lunatic pitcher schilling uninterested adjustment clueless deferral dependent eating grant
    ##      humanely identification inverted manchester reaping reciprocated rewards upside vitiello
    ##      capture disinvestment enlightened entertainers envelope falter forgive holy indication
    ##      indicator investigator speculate supervision wounding canterbury elton fraternal fuming
    ##      grandmother grandstanded idaho israeli obnoxiously proclaimed rashida risch #snhuarena yoes
    ##      @borisjohnson buddy cohesion enhanced @g7 gross kashmir manipulated nut selfish shellaced
    ##      shellacked snhu tensions unstable wheedled wreck dope dorothy frederiksen greenland
    ##      @iowastatefair lasted mette postponing rescheduling treading allyn #amvets75th @amvetshq
    ##      borrow @charlespayne2nd curb descendants driver effectively engines entries execs gays
    ##      @newsmax regulators resulting root sloan smoother squeeze 25k cancellation @charleshurt
    ##      competes cousy engaging facilitating hopeful loan preventing quicksand @realwayneroot spicer
    ##      yields carriers damaged deliveries frankly hickenlooper @jairbolsonaro motivated moulton powel
    ##      prospects rainforest responding @sencorygardner ups axios biarritz du #g7biarritz palais
    ##      segment shore appalachian bedbugs destined geeeee giuseppi italian left's perpetuate
    ##      powerfully radicals remake revenge sat stooges zogby aides anyplace bedbug bret britain corbyn
    ##      dorian giddy hopeless jeremy labour miserably outlet quitting @sandrasmithfox stephens
    ##      tracking tropical trumpers aids donnell guarantee instructions oligarchs pardons refineries
    ##      vitally
    ##  [ reached 'max' / getOption("max.print") -- omitted 651 rows ]
