
  # PACKAGES ----

library(tidyverse)
library(lubridate)
library(textclean)
library(readxl)
library(igraph)
library(kableExtra)
library(RColorBrewer)
library(gplots)
library(tidytext)
library(vader)
library(radarchart)
library(htmlwidgets)
library(gganimate)
library(gifski)
library(png)

  # DATA SYNTHESIS ----

setwd("XXXX")
df <- tibble()
for (file_name in dir()) {
  df <- bind_rows(df, read_csv(file_name,
                               col_types = "cDcTccfclicccliiliiiDcTccccicc"))
}
    # df1 ----

df_numeric <- df %>%
  filter(tweet_language == "en") %>%
  mutate(id = ifelse(is.na(retweeted_from_id),
                     tweet_id, retweeted_from_id)) %>%
  group_by(id) %>%
  summarize(tweet = n(), retweet = mean(retweeted_times), .groups = "drop")

df_character <- df %>%
  filter(tweet_language == "en") %>%
  mutate(id = ifelse(is.na(retweeted_from_id),
                     tweet_id, retweeted_from_id)) %>%
  distinct(id, .keep_all = TRUE) %>%
  separate(tweet_hashtags, into = str_c("h", seq_len(18)), sep = " ") %>%
  pivot_longer(str_c("h", seq_len(18)), values_to = "tweet_hashtag") %>%
  filter(!is.na(tweet_hashtag)) %>%
  mutate(hashtag = str_to_lower(tweet_hashtag)) %>%
  select(id, hashtag, text = tweet_text) %>%
  mutate(text = str_replace_all(text, "@.+?\\b", "")) %>%
  mutate(text = str_replace_all(text, "#.+?\\b", "")) %>%
  mutate(text = str_replace_all(text, "https://t.co/.+?\\b", "")) %>%
  mutate(text = replace_non_ascii(text, replacement = "")) %>%
  mutate(text = replace_html(text)) %>%
  mutate(text = str_replace(text, "^RT : ", ""))

setwd("XXXX")
df_label <- read_excel("labels.xlsx", sheet = "list")

df1 <- df_character %>%
  left_join(df_numeric, by = "id") %>%
  left_join(df_label, by = "hashtag")

    # df2 ----

df2 <- df %>%
  filter(!is.na(retweeted_from_handle),
         str_detect(str_to_lower(tweet_hashtags), "womensmarch")) %>%
  distinct(retweeted_from_handle, user_handle, retweeted_from_id) %>%
  count(retweeted_from_handle, user_handle, sort = TRUE) %>%
  rename(from = retweeted_from_handle, to = user_handle, weight = n)

    # df3 ----

df3 <- df %>%
  filter(tweet_language == "en") %>%
  mutate(id = ifelse(is.na(retweeted_from_id),
                     tweet_id, retweeted_from_id)) %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(id, tweet_date) %>%
  summarize(retweet = mean(retweeted_times), hashtags = unique(tweet_hashtags),
            .groups = "drop") %>%
  filter(!is.na(hashtags)) %>%
  separate(hashtags, into = str_c("h", seq_len(18)), sep = " ") %>%
  pivot_longer(str_c("h", seq_len(18)), values_to = "hashtag") %>%
  filter(!is.na(hashtag)) %>%
  mutate(hashtag = str_to_lower(hashtag)) %>%
  left_join(df_label, by = "hashtag") %>%
  filter(primary == "Themes and Slogans") %>%
  group_by(hashtag, tweet_date) %>%
  summarize(retweet = sum(retweet), .groups = "drop")

  # LOCATION ----

df1 %>%
  filter(!is.na(location)) %>%
  select(state, country, tweet) %>%
  write.csv("location.csv", row.names = FALSE)

  # WOMEN'S MARCH ----

    # data ----

closeness_centrality <- df2 %>%
  mutate(weight = 1 / weight) %>%
  graph_from_data_frame() %>%
  estimate_closeness(mode = "out", cutoff = 10)
closeness_centrality <-
  tibble(handle = names(closeness_centrality),
         score = unname(closeness_centrality))
betweenness_centrality <- df2 %>%
  mutate(weight = 1 / weight) %>%
  graph_from_data_frame() %>%
  estimate_betweenness(directed = TRUE, cutoff = 10)
betweenness_centrality <-
  tibble(handle = names(betweenness_centrality),
         score = unname(betweenness_centrality))

    # visualization ----

betweenness_centrality %>%
  slice_max(order_by = score, n = 10) %>%
  mutate(name = c("Women's March", "Moms Demand Action",
                  "Meg Wears a Mask (she/her)", "Linda Hill",
                  "Women's March Sydney", "Alma Har'el", "Paola Mendoza",
                  "Emmeline Spankhurts", "Michael Skolnik", "Shit Country")) %>%
  select(handle, name, score) %>%
  kable(col.names = c("HANDLE", "NAME", "BETWEENNESS"),
        caption = "Users with Top Betweenness Centrality Scores") %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, font_size = 13) %>%
  row_spec(seq(0, 10), align = "c") %>%
  column_spec(1, width = "3.5cm") %>%
  column_spec(2, width = "6.5cm") %>%
  column_spec(3, width = "2.5cm") %>%
  row_spec(c(1, 2, 5), background = brewer.pal(1, "Greens")) %>%
  row_spec(c(3, 4, 6, 7, 8), background = "mistyrose") %>%
  row_spec(9, background = "azure") %>%
  row_spec(10, background = brewer.pal(1, "Purples")) %>%
  row_spec(c(1, 2, 6, 7, 9), bold = TRUE)
closeness_centrality %>%
  slice_max(order_by = score, n = 10)

closeness_centrality %>%
  slice_max(order_by = score, n = 10) %>%
  mutate(score = log(score, base = 10),
         name = c("Amanda Jackson", "CNN", "Nick Offerman", "Emma Watson",
                  "Hillary Clinton", "Antonio French", "Greg Hogben", "CAT",
                  "Bianca Alysse", "Ava DuVernay")) %>%
  select(handle, name, score) %>%
  kable(col.names = c("HANDLE", "NAME", "log(CLOSENESS)"),
        caption = "Users with Top Closeness Centrality Scores") %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, font_size = 13) %>%
  row_spec(seq(0, 10), align = "c") %>%
  column_spec(1, width = "3.5cm") %>%
  column_spec(2, width = "6.5cm") %>%
  column_spec(3, width = "2.5cm") %>%
  row_spec(2, background = brewer.pal(1, "Greens")) %>%
  row_spec(c(1, 4, 5, 8, 9, 10), background = "mistyrose") %>%
  row_spec(c(3, 6, 7), background = "azure") %>%
  row_spec(c(1, 2, 3, 4, 5, 6, 7, 10), bold = TRUE)

  # SOCIOPOLITICAL ISSUES ----

sociopolitical <- df1 %>%
  filter(primary == "Sociopolitical Issues") %>%
  group_by(hashtag, secondary) %>%
  summarize(tweet = sum(tweet), retweet = sum(retweet), .groups = "drop")

sociopolitical_colors <- brewer.pal(7, "Set1")[-6]
sociopolitical_colors <-
  c(sociopolitical_colors[seq_len(5)],
    col2hex(c("darkred", "navyblue", "darkgreen",
              "darkorchid", "darkgoldenrod")),
    rep(sociopolitical_colors[6], 8))

    # assign colors ----

sociopolitical <- sociopolitical %>%
  filter(!is.na(secondary)) %>%
  group_by(secondary) %>%
  summarize(n_tweet = sum(tweet), .groups = "drop") %>%
  arrange(desc(n_tweet)) %>%
  bind_rows(tibble(secondary = NA, n_tweet = NA)) %>%
  mutate(color = sociopolitical_colors) %>%
  select(secondary, color) %>%
  full_join(sociopolitical, by = "secondary")
write.csv(sociopolitical, "sociopolitical.csv", row.names = FALSE)

  # AMERICAN POLITICS ----

    # afinn ----

      # data ----

afinn <- df1 %>%
  filter(primary == "American Politics", !is.na(secondary)) %>%
  unnest_tokens("word", "text", token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  select(secondary, word, value, tweet, retweet) %>%
  group_by(secondary, word) %>%
  summarize(value = unique(value), tweet = sum(tweet), retweet = sum(retweet),
            .groups = "drop")
get_mean_afinn <- function(category, weight, size, times) {
  x <- afinn %>% filter(secondary == category)
  if (weight == "none") {
    x <- x %>% select(word, value)
  } else {
    x <- x %>% select(n = weight, word, value)
    x <- tibble(word = rep(x$word, x$n), value = rep(x$value, x$n))
  }
  tibble(category = category, weight = weight,
         mean = replicate(n = times, {mean(x$value[sample(nrow(x), size)])}))
}

afinn_parameters <-
  data.frame(category = rep(unique(afinn$secondary), each = 3)) %>%
  filter(!is.na(category)) %>%
  mutate(weight = rep(c("none", "tweet", "retweet"), 7),
         size = rep(c(35, 75, 93), 7))
afinn_data <- map_df(seq_len(nrow(afinn_parameters)), function(i) {
  set.seed(150)
  row <- afinn_parameters[i, ]
  get_mean_afinn(row$category, row$weight, row$size, 15000)
})

      # visualization ----

afinn_colors <- brewer.pal(8, "Set1")[-6] %>%
  .[c(1, 4, 2, 2, 3, 2, 5, 1, 3, 6, 3, 1, 4, 4, 6, 7, 5, 5, 6, 7, 7)]
afinn_data %>%
  mutate(category = reorder_within(category, by = mean, within = weight),
         weight = factor(weight, levels = c("none", "tweet", "retweet"))) %>%
  ggplot() + geom_boxplot(aes(x = category, y = mean, color = category),
                          show.legend = FALSE) +
  scale_color_manual(values = afinn_colors) +
  labs(title = "Mean AFINN Values of Topics in American Politics",
       x = "Topic Name", y = "Mean AFINN Value") +
  theme_bw() + theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_reordered() +
  facet_wrap(~ weight, nrow = 3, scales = "free_x",
             labeller = as_labeller(c("none" = "Unique Words",
                                      "tweet" = "Weighted by Tweets",
                                      "retweet" = "Weighted by Retweets")))

afinn %>%
  filter(secondary == "Barack Obama", value < 0) %>%
  mutate(weight_tweet = value * tweet, weight_retweet = value * retweet) %>%
  slice_max(order_by = weight_tweet * weight_retweet, n = 10) %>%
  mutate(retweet = ceiling(retweet), word = str_to_sentence(word)) %>%
  select(word, value, tweet, retweet) %>%
  kable(col.names = c("Word", "Value", "Tweets", "Retweets"),
        caption = "Top Negative Words for Barack Obama") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE, font_size = 13) %>%
  row_spec(seq(0, 10), align = "c") %>%
  column_spec(1, width = "2.5cm") %>%
  column_spec(c(2, 3, 4), width = "1.5cm")

afinn %>%
  filter(secondary == "Hillary Clinton", value > 0) %>%
  mutate(weight_tweet = value * tweet, weight_retweet = value * retweet) %>%
  slice_max(order_by = weight_tweet * weight_retweet, n = 10) %>%
  mutate(retweet = ceiling(retweet), word = str_to_sentence(word)) %>%
  select(word, value, tweet, retweet) %>%
  kable(col.names = c("Word", "Value", "Tweets", "Retweets"),
        caption = "Top Positive Words for Hillary Clinton") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE, font_size = 13) %>%
  row_spec(seq(0, 10), align = "c") %>%
  column_spec(1, width = "2.5cm") %>%
  column_spec(c(2, 3, 4), width = "1.5cm")

    # vader ----

      # data ----

vader_text <- df1 %>%
  filter(primary == "American Politics", !is.na(secondary)) %>%
  group_by(secondary, id) %>%
  summarize(text = unique(text), n = n(), tweet = sum(tweet),
            retweet = mean(retweet), .groups = "drop")
vader_score <- vader_df(unique(vader_text$text))
vader_score$compound[16975] <- -0.505
vader_score$compound[27253] <- -0.135
rm(list = c("incl_nt", "neu_set"))
vader <- as_tibble(vader_score) %>%
  filter(!is.na(compound)) %>%
  select(text, compound) %>%
  left_join(vader_text, by = "text") %>%
  select(secondary, n, tweet, retweet, compound)

get_mean_vader <- function(category, weight, size, times) {
  x <- vader %>% filter(secondary == category)
  if (weight == "n") {
    x <- rep(x$compound, x$n)
  } else if (weight == "tweet") {
    x <- rep(x$compound, x$tweet)
  } else {
    x <- rep(x$compound, x$retweet)
  }
  tibble(category = category, weight = weight,
         mean = replicate(n = times, {
           mean(x[sample(length(x), size)])
         }))
}
vader_parameters <-
  data.frame(category = rep(unique(vader$secondary), each = 3)) %>%
  filter(!is.na(category)) %>%
  mutate(weight = rep(c("n", "tweet", "retweet"), 7),
         size = rep(c(100, 250, 430), 7))
vader_data <- map_df(seq_len(nrow(vader_parameters)), function(i) {
  set.seed(150)
  row <- vader_parameters[i, ]
  get_mean_vader(row$category, row$weight, row$size, 15000) 
})

      # visualization ----

vader_colors <- brewer.pal(8, "Set1")[-6] %>%
  .[c(1, rep(2, 3), 5, 4, 1, 6, 6, 3, 3, 4, 3, 6, 1, 4, rep(7, 3), 5, 5)]
vader_data %>%
  mutate(category = reorder_within(category, by = mean, within = weight),
         weight = factor(weight, levels = c("n", "tweet", "retweet"))) %>%
  ggplot() + geom_boxplot(aes(x = category, y = mean, color = category),
                          show.legend = FALSE) +
  scale_color_manual(values = vader_colors) +
  labs(title = "Mean Compound VADER of Topics in American Politics",
       x = "Topic Name", y = "Mean Compound VADER") +
  theme_bw() + theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_reordered() +
  facet_wrap(~ weight, nrow = 3, scales = "free_x",
             labeller = as_labeller(c("n" = "Unweighted",
                                      "tweet" = "Weighted by Tweets",
                                      "retweet" = "Weighted by Retweets")))

    # nrc ----

nrc <- df1 %>%
  filter(secondary == "Donald Trump", !is.na(tertiary)) %>%
  unnest_tokens("word", "text", token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  group_by(tertiary, word) %>%
  summarize(n = n(), .groups = "drop") %>%
  inner_join(get_sentiments("nrc") %>%
               filter(sentiment != "positive", sentiment != "negative"),
             by = "word")
nrc_data <- nrc %>%
  group_by(tertiary, sentiment) %>%
  summarize(numerator = sum(n), .groups = "drop") %>%
  left_join(nrc %>%
              group_by(tertiary) %>%
              summarize(denominator = sum(n), .groups = "drop"),
            by = "tertiary") %>%
  mutate(sentiment = str_to_sentence(sentiment),
         proportion = numerator / denominator) %>%
  select(tertiary, sentiment, proportion) %>%
  pivot_wider(names_from = "tertiary", values_from = "proportion")

nrc_plot <- chartJSRadar(nrc_data, polyAlpha = 0.05, lineAlpha = 0.75,
                         colMatrix = col2rgb(brewer.pal(8, "Set1")[-6]))
saveWidget(nrc_plot, "nrc.html")

  # THEMES AND SLOGANS ----

hashtags_by_day <- df3 %>%
  group_by(tweet_date) %>%
  arrange(desc(retweet)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  group_by(hashtag) %>%
  mutate(peak_rank = min(rank)) %>%
  filter(peak_rank <= 3) %>%
  arrange(tweet_date) %>%
  mutate(cumulative_retweet = cumsum(retweet)) %>%
  mutate(cumulative_retweet = 
           ifelse(cumulative_retweet == 0, 0,
                  log(cumulative_retweet, base = 10))) %>%
  select(hashtag, tweet_date, cumulative_retweet) %>%
  pivot_wider(names_from = tweet_date, values_from = cumulative_retweet)
hashtags_by_day <- as.data.frame(hashtags_by_day)
for (i in seq_len(nrow(hashtags_by_day))) {
  row <- as.numeric(hashtags_by_day[i, -1])
  if (any(is.na(row))) {
    for (j in seq_along(row)) {
      if (is.na(row[j])) {
        row[j] <- ifelse(j == 1, 0, row[j - 1])
      }
    }
    hashtags_by_day[i, -1] <- row
  }
}
rm(list = c("i", "j", "row"))
hashtags_by_day <- as_tibble(hashtags_by_day) %>%
  pivot_longer(contains("2017"), names_to = "date", values_to = "retweet") %>%
  group_by(hashtag) %>%
  mutate(date = day(date), final_retweet = max(retweet)) %>%
  ungroup()

hashtags_by_day_plot <- hashtags_by_day %>% ggplot() +
  geom_col(aes(fct_reorder(hashtag, final_retweet), retweet, fill = hashtag)) +
  scale_fill_manual(values = c(brewer.pal(12, "Paired"), "#BDBDBD")) +
  theme_bw() +
  coord_flip() +
  transition_time(date) +
  labs(title = "January {frame_time}, 2017",
       x = "Hashtag", y = "Scaled Cumulative Retweets", fill = "Hashtag")
animate(hashtags_by_day_plot, nframes = 55, width = 1000, height = 800) +
  enter_grow() + exit_fade()
anim_save("slogans.gif")
