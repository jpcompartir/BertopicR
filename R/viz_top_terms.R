library(forcats)

library(dplyr)
library(janeaustenr)
library(tidytext)

words <- merged_df %>%
  tidytext::unnest_tokens(word, text_clean) %>%
  dplyr::count(topic, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

total_words <- words %>% 
  group_by(topic) %>% 
  summarize(total = sum(n))

topic_words <- left_join(words, total_words)

topic_tf_idf <- topic_words %>%
  tidytext::bind_tf_idf(word, topic, n)

topic_tf_idf %>%
  group_by(topic) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, forcats::fct_reorder(word, tf_idf), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
