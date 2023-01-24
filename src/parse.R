library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(tidyverse)
library(stringi)

app_name <- "COVID-19 SSA"
consumer_key <- "liWAcmuVJlqgUAtVPQfw0PfFu"
consumer_secret <- "jZlerwkPjYDyjVm93TYWyxMah7PeKCXyWLiHGRCOurFJsDC88Y"
access_token <- "1446444869191352326-VWXmIXdfUEMPT0csOS3AtJZiVA1J3B"
access_secret <- "30Qm0jkD4EX5VEPGoJHsEif4krG3nYhO6hI8NmEdZpHDg"

create_token(
  app = app_name,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret
)
## toDate <- format(Sys.time() - 60 * 60 * 24 * 7, "%Y%m%d%H%M")
# Get COVID-19 Tweets
# TODO: Uncomment if need to get more tweets
covid_tweets <- search_tweets2("#COVID-19", n = 5000, include_rts = TRUE, lang = "en", retryonratelimit = TRUE)
# filtered <- covid_tweets %>% select(screen_name, created_at, text)
# write.csv(filtered, "tweets.csv", row.names=FALSE)
# covid_tweets <- search_30day("#COVID-19", n = 1000, token = consumer_key)
# covid_tweets <- search_30day("#COVID-19", n = 300, env_name="sandbox", fromDate = "2020-10-10", toDate = "2020-10-11", parse=TRUE)
# covid_tweets
# head(covid_tweets %>% select(text))
# head(covid_tweets %>% select(screen_name, created_at, text))
max(covid_tweets$created_at)
min(covid_tweets$created_at)

tweets.covid_tweets <- covid_tweets %>% select(screen_name, created_at, text)
## head(tweets.covid_tweets)

# Using the AFINN Lexicon
tweet_sentiments <- tweets.covid_tweets %>%
  unnest_tokens(word, text, token = "tweets") %>%
  inner_join(get_sentiments("afinn"))

# tweet_scores <- tweets.covid_tweets %>%
#   left_join(tweet_sentiments %>%
#     group_by(screen_name) %>%
#     summarise(value = sum(value))) %>%
#   replace_na(list(score = 0))

## display_afinn <- tweet_sentiments %>%
##   count(word, value, sort = TRUE) %>%
##   group_by(value) %>%
##   # top_n(10) %>%
##   ungroup() %>%
##   mutate(value = reorder(value, n)) %>%
##   ggplot(aes(value, n)) +
##   geom_col(show.legend = FALSE) +
##   # facet_grid(value ~ .) +
##   labs(
##     title = "Sentiment Strength of Tweets containing #COVID-19. (Pulled 1000 Tweets and Using AFINN Lexicon)",
##     y = "Number of Words",
##     x = NULL
##   ) +
##   theme_bw()

# Using the Bing Lexicon
## tweets.covid_tweets$stripped_text1 <- gsub(
##   "https\\S+", "", tweets.covid_tweets$text
## )
## tweets.covid_tweets$stripped_text1 <- gsub(
##   "[\r\n]", " ", tweets.covid_tweets$text
## )
## tweets.covid_tweets$stripped_text1 <- gsub(
##   "( ,)|(,,)", " ", tweets.covid_tweets$

## tweets.covid_tweets$stripped_text1 <- stri_replace_all_regex(tweets.covid_tweets$text, c("http\\S+", "[\r\n]", "( ,)", "(,,)", "(, )"), c("", " ", "", "", ""), vectorize=F)
tweets.covid_tweets$stripped_text1 <- stri_replace_all_regex(tweets.covid_tweets$text, c("http\\S+", "[\r\n]", ","), c("", " ", ""), vectorize = F)

want_tweets <- tweets.covid_tweets %>% select(screen_name, created_at, stripped_text1)

# write.csv(want_tweets, "tweets.csv", row.names=FALSE)
write.table(want_tweets, "tweets.csv", sep = ",", append = T, col.names = !file.exists("tweets.csv"), row.names = FALSE)

## tweets.covid_tweets_stem <- tweets.covid_tweets %>%
##   select(stripped_text1) %>%
##   unnest_tokens(word, stripped_text1)



## cleaned_tweets.covid_tweets <- tweets.covid_tweets_stem %>%
##   anti_join(stop_words)



## bing_tweets <- cleaned_tweets.covid_tweets %>%
##   inner_join(get_sentiments("bing")) %>%
##   count(word, sentiment, sort = TRUE) %>%
##   ungroup()

## bing_tweets %>%
##   group_by(sentiment) %>%
##   top_n(10) %>%
##   ungroup() %>%
##   mutate(word = reorder(word, n)) %>%
##   ggplot(aes(word, n, fill = sentiment)) +
##   geom_col(show.legend = FALSE) +
##   facet_wrap(~sentiment, scales = "free_y") +
##   labs(
##     title = "Tweets containing #COVID-19",
##     y = "Contribution to sentiment",
##     x = NULL
##   ) +
##   coord_flip() +
##   theme_bw()


## tweets.covid_tweets$stripped_text1 <- gsub(
##   "https\\S+", "", tweets.covid_tweets$text
## )
## tweets.covid_tweets_stem <- tweets.covid_tweets %>%
##   select(stripped_text1) %>%
##   unnest_tokens(word, stripped_text1)

## cleaned_tweets.covid_tweets <- tweets.covid_tweets_stem %>%
##   anti_join(stop_words)
## head(cleaned_tweets.covid_tweets)

## display_cleaned <- cleaned_tweets.covid_tweets %>%
##   count(word, sort = TRUE) %>%
##   top_n(10) %>%
##   mutate(word = reorder(word, n)) %>%
##   ggplot(aes(x = word, y = n)) +
##   geom_col() +
##   xlab(NULL) +
##   coord_flip() +
##   theme_classic() +
##   labs(
##     x = "Count",
##     y = "Unique Words",
##     title = "Unique word counts found in #COVID-19 tweets (Pulled 1000 Tweets and No Lexicon)"
##   )

## bing_tweets <- cleaned_tweets.covid_tweets %>%
##   inner_join(get_sentiments("bing")) %>%
##   count(word, sentiment, sort = TRUE) %>%
##   ungroup()

## display_bing <- bing_tweets %>%
##   group_by(sentiment) %>%
##   top_n(10) %>%
##   ungroup() %>%
##   mutate(word = reorder(word, n)) %>%
##   ggplot(aes(word, n, fill = sentiment)) +
##   geom_col(show.legend = FALSE) +
##   facet_wrap(~sentiment, scales = "free_y") +
##   labs(
##     title = "Tweets containing #COVID-19 (Pulled 1000 Tweets and Using Bing Lexicon)",
##     y = "Contribution to sentiment",
##     x = NULL
##   ) +
##   coord_flip() +
##   theme_bw()

## # Choose which graph to display
## # display_cleaned
## # display_afinn
## # display_bing
