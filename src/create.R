library(rtweet)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(tidytext)
library(tidyverse)
library(janitor)
library(lubridate)

data <- read_csv("tweets.csv")

## date_data <- subset(data, created_at > "2021-10-30" & created_at < "2021-11-07")
date_data <- subset(data, created_at > "2021-11-07")
## date_data <- data

## data <- subset(data, created_at > "2021-11-06" & created_at < "2021-11-08")

## spec(data)
## colnames(data)
## problems(data)
## data <- read.table("tweets.csv", sep=",", colClasses=rep("character", 3), header=FALSE)
## data <- read.csv("tweets.csv", col.names = c("screen_name", "created_at", "stripped_text1"), strip.white = TRUE)
## data <- read_excel("cleaner.xlsx")
## head(data)
## head(data)
problems(data)
problems(date_data)

## split_data <- split(data, as.Date(data$created_at))

tweets.covid_tweets_stem <- date_data %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

cleaned_tweets.covid_tweets <- tweets.covid_tweets_stem %>%
  anti_join(stop_words)

display_cleaned <- cleaned_tweets.covid_tweets %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(
    x = "Count",
    y = "Unique Words",
    title = "Unique word counts found in #COVID-19 tweets (300k Tweets)"
  )

bing_tweets <- cleaned_tweets.covid_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

display_bing <- bing_tweets %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(
    title = "Tweets containing #COVID-19 (BING LEXICON)",
    y = "Contribution to sentiment",
    x = NULL
  ) +
  coord_flip() +
  theme_bw()

## ## head(split_data)

tweet_sentiments <- date_data %>%
  unnest_tokens(word, stripped_text1, token = "tweets") %>%
  inner_join(get_sentiments("afinn"))

tweet_scores <- date_data %>%
  left_join(tweet_sentiments %>%
    group_by(screen_name) %>%
    summarise(value = sum(value))) %>%
  replace_na(list(value = 0))

display_afinn <- tweet_sentiments %>%
  count(word, value, sort = TRUE) %>%
  group_by(value) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(value = reorder(value, n)) %>%
  ggplot(aes(value, n)) +
  geom_col(show.legend = FALSE) +
  facet_grid(value ~ .) +
  labs(
    title = "Sentiment Strength of Tweets containing #COVID-19 (AFINN Lexicon)",
    y = "Number of Words",
    x = NULL
  ) +
  theme_bw()

display_cleaned
display_bing
display_afinn

split(tweet_scores, as.Date(tweet_scores$created_at))

## ggplot() +
## geom_point(tweet_scores, mapping = aes(x = created_at,
##                                        y = value)) +
## geom_smooth(method = "lm", se = FALSE)

max(tweet_scores$value)
tweet_scores[which(tweet_scores$value == min(tweet_scores$value)),]

afinn_model <- tweet_scores %>%
  lm(`value` ~ created_at,
    data = .,
    na.action = na.exclude
  )

summary(afinn_model)

ggplot(tweet_scores, mapping = aes(
  x = created_at,
  y = value
)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Scatter Plot of Sentiment Values vs created_at date (AFINN Lexicon)",
    x = "Created At",
    y = "Sentiment Value"
  ) +
  theme_bw()

afinn_model %>%
    fortify(afinn_model$model) %>%
    ggplot(aes(x = .fitted,
               y = .resid))+
    geom_point() +
    geom_hline(yintercept=0)+
    labs(x = "Predicted Values",
         y = "Residuals",
         title = "Residulas vs. Predicted Values")

plot(1:length(afinn_model$residuals), afinn_model$residuals)

## afinn_model %>%
##     fortify(afinn_model$model) %>%
##     mutate(row = row_number()) %>%
##     ggplot(aes(x = row,
##                y = .resid))+
##     geom_point()+
##     geom_hline(yintercept = 0)+
##     labs(x = "Order of Occurence",
##          y = "Residuals",
##          title = "Residuls in Order of Occurence")

afinn_model %>%
    fortify(afinn_model$model)%>%
    ggplot(aes(x=.resid))+
    geom_histogram()+
    labs(x = "Residuals",
         title = "Histogram of Residuals")
