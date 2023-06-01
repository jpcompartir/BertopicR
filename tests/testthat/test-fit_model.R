# dataframe

data <- bert_example_data %>%
  janitor::clean_names() %>% # clean column titles
  mutate(text_clean = message, .before = message) %>% # add column for clean text
  mutate(text_clean = tolower(text_clean)) %>% # all posts lower case
  limpiar_tags(text_var = text_clean, hashtag = F) %>%  # remove mentions
  mutate(text_clean = str_remove_all(text_clean, "@user"), # remove mentions
         text_clean = str_remove_all(text_clean, "#\\S+")) %>% # remove hashtags
  limpiar_url(text_var = text_clean) %>% # remove urls
  limpiar_emojis(text_var = text_clean, with_emoji_tag = TRUE) %>% # remove emojis
  mutate(text_clean = str_remove_all(text_clean, "\\S+[a-z]+_emoji")) %>%
  limpiar_spaces(text_var = text_clean) %>% # remove unnecessary spaces
  distinct(text_clean, .keep_all = TRUE)  # remove duplicates

# create umap model
umap <- reticulate::import("umap")
umap_model <- umap$UMAP(random_state = 42)

# create representation model
representation <- reticulate::import("bertopic.representation")
representation_model <- representation$MaximalMarginalRelevance(diversity = 0.3)


vectorizer <- reticulate::import("sklearn.feature_extraction.text")
vecrtorizer_model <- vectorizer$CountVectorizer(ngram_range = tuple(1L,3L),
                                                stop_words = "english")

# embeddings
sentence_transformers <- reticulate::import("sentence_transformers")
sentence_model <- sentence_transformers$SentenceTransformer("all-MiniLM-L6-v2")
embeddings <- sentence_model$encode(data$text_clean, device = "mps")

# initiate model
model_eval1 <- py$bertopic$BERTopic(vectorizer_model = vecrtorizer_model)
output <- model_eval1$fit_transform(data$text_clean)
# run function
model_test1 <- function(cleaned_text = data$text_clean,
                  min_topic_size = 20,
                  ngram_range = c(1,3),
                  embedding_model = "all-MiniLM-L6-v2",
                  accelerator = "mps",
                  diversity = 0.3,
                  stopwords = TRUE,
                  random_state = 42)

test_that("random state works", {
  expect_equal(model_eval1$get_topic_info(), model_test1$get_topic_info())
})