Lab: Tokenization, Wrangling, EDA
================
Haohan Chen (HKU)
2023-03-27

## Introduction

This notebook demonstrate Sentiment Analsyis

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   1.0.1
    ## ✔ tibble  3.1.8     ✔ dplyr   1.1.0
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
d_fulltext = read_rds("data/fulltext.rds")
# Change the date variable to "date" format
d_fulltext = d_fulltext %>% mutate(date_of_speech = dmy(date_of_speech))
```

## Tokenization

``` r
if (!require("tidytext")) install.packages("tidytext")
```

    ## Loading required package: tidytext

``` r
library(tidytext) # Full introduction: http://tidytextmining.com/
```

``` r
d_tokenized = d_fulltext %>%
  select(uid, date_of_speech, text) %>%
  unnest_tokens(word, text)

head(d_tokenized, 20)
```

    ## # A tibble: 20 × 3
    ##    uid   date_of_speech word      
    ##    <chr> <date>         <chr>     
    ##  1 A1    2022-05-30     moving    
    ##  2 A1    2022-05-30     steadily  
    ##  3 A1    2022-05-30     forward   
    ##  4 A1    2022-05-30     along     
    ##  5 A1    2022-05-30     path      
    ##  6 A1    2022-05-30     to        
    ##  7 A1    2022-05-30     normalcy  
    ##  8 A1    2022-05-30     amid      
    ##  9 A1    2022-05-30     stabilised
    ## 10 A1    2022-05-30     covid     
    ## 11 A1    2022-05-30     19        
    ## 12 A1    2022-05-30     epidemic  
    ## 13 A1    2022-05-30     having    
    ## 14 A1    2022-05-30     gone      
    ## 15 A1    2022-05-30     through   
    ## 16 A1    2022-05-30     the       
    ## 17 A1    2022-05-30     turbulent 
    ## 18 A1    2022-05-30     time      
    ## 19 A1    2022-05-30     in        
    ## 20 A1    2022-05-30     early

``` r
# Simple?
```

## Wrangling: Remove Stop Words

``` r
# Load Stopwords
data("stop_words")

head(stop_words, 20)
```

    ## # A tibble: 20 × 2
    ##    word        lexicon
    ##    <chr>       <chr>  
    ##  1 a           SMART  
    ##  2 a's         SMART  
    ##  3 able        SMART  
    ##  4 about       SMART  
    ##  5 above       SMART  
    ##  6 according   SMART  
    ##  7 accordingly SMART  
    ##  8 across      SMART  
    ##  9 actually    SMART  
    ## 10 after       SMART  
    ## 11 afterwards  SMART  
    ## 12 again       SMART  
    ## 13 against     SMART  
    ## 14 ain't       SMART  
    ## 15 all         SMART  
    ## 16 allow       SMART  
    ## 17 allows      SMART  
    ## 18 almost      SMART  
    ## 19 alone       SMART  
    ## 20 along       SMART

``` r
# Remove stopwords
d_tokenized_s = d_tokenized %>%
  anti_join(stop_words, by = "word")
# anti_join: whatever appearing in the stop_words dataframe, we remove it.
```

## Wrangling \[Optional\]: Stemming

``` r
if (!require(SnowballC)) install.packages("SnowballC")
```

    ## Loading required package: SnowballC

``` r
library(SnowballC)
```

``` r
d_tokenized_s = d_tokenized_s %>%
  mutate(stem = wordStem(word))

head(d_tokenized_s, 20)
```

    ## # A tibble: 20 × 4
    ##    uid   date_of_speech word       stem    
    ##    <chr> <date>         <chr>      <chr>   
    ##  1 A1    2022-05-30     moving     move    
    ##  2 A1    2022-05-30     steadily   steadili
    ##  3 A1    2022-05-30     forward    forward 
    ##  4 A1    2022-05-30     path       path    
    ##  5 A1    2022-05-30     normalcy   normalci
    ##  6 A1    2022-05-30     amid       amid    
    ##  7 A1    2022-05-30     stabilised stabilis
    ##  8 A1    2022-05-30     covid      covid   
    ##  9 A1    2022-05-30     19         19      
    ## 10 A1    2022-05-30     epidemic   epidem  
    ## 11 A1    2022-05-30     turbulent  turbul  
    ## 12 A1    2022-05-30     time       time    
    ## 13 A1    2022-05-30     march      march   
    ## 14 A1    2022-05-30     arrival    arriv   
    ## 15 A1    2022-05-30     dawn       dawn    
    ## 16 A1    2022-05-30     april      april   
    ## 17 A1    2022-05-30     wave       wave    
    ## 18 A1    2022-05-30     epidemic   epidem  
    ## 19 A1    2022-05-30     control    control 
    ## 20 A1    2022-05-30     daily      daili

## Sentiment Analysis

``` r
if (!require(textdata)) install.packages("textdata")
```

    ## Loading required package: textdata

``` r
library(textdata)
```

### Load Sentiment Dictionary

``` r
dict_afinn = get_sentiments("afinn")
dict_bing = get_sentiments("bing")
dict_nrc = get_sentiments("nrc") 
# Learn more https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
```

Note, if you run this function for the first time, you will get a prompt
in the console asking you to confirm that you are willing to download
the sentiment dataset. The prompt looks as follows. Type “1” to install
the dictionaries.

    Do you want to download:

    Name: AFINN-111

    URL: http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010

    License: Open Database License (ODbL) v1.0

    Size: 78 KB (cleaned 59 KB)

    Download mechanism: https

    1: Yes

    2: No

### Calculate the Simplest Sentiment Scores

``` r
# Merge your tokenized documents with the sentiment dictionary
d_tokenized_s_afinn = d_tokenized_s %>%
  select(uid, date_of_speech, word) %>%
  inner_join(dict_afinn, by = "word")

# Aggregate the sentiment score for each document
d_tokenized_s_afinn_agg = d_tokenized_s_afinn %>%
  group_by(uid, date_of_speech) %>%
  summarise(sentiment_score = sum(value))
```

    ## `summarise()` has grouped output by 'uid'. You can override using the `.groups`
    ## argument.

``` r
d_tokenized_s_afinn_agg = d_fulltext %>%
  select(uid) %>%
  left_join(d_tokenized_s_afinn_agg) %>%
  mutate(sentiment_score = replace_na(sentiment_score, 0))
```

    ## Joining with `by = join_by(uid)`

``` r
# Change of sentiment over time?
d_tokenized_s_afinn_agg %>%
  ggplot(aes(x = date_of_speech, y = sentiment_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  labs(
    title = "Sentiment Scores of Hong Kong CE's Speeches and Articles"
  ) +
  xlab("Date") + ylab("Sentiment Scores")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](/Users/haohanchen/Library/CloudStorage/GoogleDrive-haohanch@gmail.com/My%20Drive/TEACHING/CSS/TextMining_NUS_230327/note/5_Sentiment_Analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# To do it better, we can normalize the sentiment scores by document lengths

# Merge your tokenized documents with the sentiment dictionary
d_tokenized_s_afinn = d_tokenized_s %>%
  group_by(uid) %>% mutate(doc_length = n()) %>%
  ungroup() %>%
  select(uid, date_of_speech, word, doc_length) %>%
  inner_join(dict_afinn, by = "word") %>%
  ungroup()

# Aggregate the sentiment score for each document
d_tokenized_s_afinn_agg = d_tokenized_s_afinn %>%
  group_by(uid, date_of_speech) %>%
  summarise(sentiment_score = sum(value) / mean(doc_length))
```

    ## `summarise()` has grouped output by 'uid'. You can override using the `.groups`
    ## argument.

``` r
d_tokenized_s_afinn_agg = d_fulltext %>%
  select(uid) %>%
  left_join(d_tokenized_s_afinn_agg) %>%
  mutate(sentiment_score = replace_na(sentiment_score, 0))
```

    ## Joining with `by = join_by(uid)`

``` r
# Change of sentiment over time?
d_tokenized_s_afinn_agg %>%
  ggplot(aes(x = date_of_speech, y = sentiment_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  labs(
    title = "Sentiment Scores of Hong Kong CE's Speeches and Articles"
  ) +
  xlab("Date") + ylab("Sentiment Scores (Normalized)")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](/Users/haohanchen/Library/CloudStorage/GoogleDrive-haohanch@gmail.com/My%20Drive/TEACHING/CSS/TextMining_NUS_230327/note/5_Sentiment_Analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Calculate Scores of Emotions

``` r
dict_nrc
```

    ## # A tibble: 13,872 × 2
    ##    word        sentiment
    ##    <chr>       <chr>    
    ##  1 abacus      trust    
    ##  2 abandon     fear     
    ##  3 abandon     negative 
    ##  4 abandon     sadness  
    ##  5 abandoned   anger    
    ##  6 abandoned   fear     
    ##  7 abandoned   negative 
    ##  8 abandoned   sadness  
    ##  9 abandonment anger    
    ## 10 abandonment fear     
    ## # … with 13,862 more rows

``` r
d_tokenized_s_nrc = d_tokenized_s %>%
  inner_join(dict_nrc, by = "word", multiple = "all")

d_tokenized_s_nrc_agg = d_tokenized_s_nrc %>%
  group_by(uid, date_of_speech, sentiment) %>%
  count() %>%
  pivot_wider(names_from = "sentiment", values_from = "n", 
              names_prefix = "sentiment_score_")

names(d_tokenized_s_nrc_agg)
```

    ##  [1] "uid"                          "date_of_speech"              
    ##  [3] "sentiment_score_anger"        "sentiment_score_anticipation"
    ##  [5] "sentiment_score_disgust"      "sentiment_score_fear"        
    ##  [7] "sentiment_score_joy"          "sentiment_score_negative"    
    ##  [9] "sentiment_score_positive"     "sentiment_score_sadness"     
    ## [11] "sentiment_score_surprise"     "sentiment_score_trust"

``` r
# Change of sentiment over time?
d_tokenized_s_nrc_agg %>%
  ggplot(aes(x = date_of_speech, y = sentiment_score_sadness)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  labs(
    title = "Sentiment Scores of Hong Kong CE's Speeches and Articles"
  ) +
  xlab("Date") + ylab("Sadness Scores (Normalized)")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 14 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 14 rows containing missing values (geom_point).

![](/Users/haohanchen/Library/CloudStorage/GoogleDrive-haohanch@gmail.com/My%20Drive/TEACHING/CSS/TextMining_NUS_230327/note/5_Sentiment_Analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# Normalize the sentiment scores
d_tokenized_s_nrc = d_tokenized_s %>%
  group_by(uid) %>%
  mutate(doc_length = n()) %>%
  ungroup() %>%
  inner_join(dict_nrc, by = "word", multiple = "all")

d_tokenized_s_nrc_agg = d_tokenized_s_nrc %>%
  group_by(uid, date_of_speech, sentiment) %>%
  summarise(n = n() / mean(doc_length)) %>%
  pivot_wider(names_from = "sentiment", values_from = "n", 
              names_prefix = "sentiment_score_")
```

    ## `summarise()` has grouped output by 'uid', 'date_of_speech'. You can override
    ## using the `.groups` argument.

``` r
# Change of sentiment over time?
d_tokenized_s_nrc_agg %>%
  ggplot(aes(x = date_of_speech, y = sentiment_score_sadness)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  labs(
    title = "Sentiment Scores of Hong Kong CE's Speeches and Articles"
  ) +
  xlab("Date") + ylab("Sadness Scores (Normalized)")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 14 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 14 rows containing missing values (geom_point).

![](/Users/haohanchen/Library/CloudStorage/GoogleDrive-haohanch@gmail.com/My%20Drive/TEACHING/CSS/TextMining_NUS_230327/note/5_Sentiment_Analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
