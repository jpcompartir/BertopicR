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
umap_model <- umap$UMAP(n_neighbors=15L, 
                        n_components=5L, 
                        min_dist=0.0, 
                        metric='cosine', 
                        random_state = 42L)

# create representation model
representation <- reticulate::import("bertopic.representation")
representation_model <- representation$MaximalMarginalRelevance(diversity = NULL)


vectorizer <- reticulate::import("sklearn.feature_extraction.text")
vectorizer_model <- vectorizer$CountVectorizer(ngram_range = tuple(1L,2L),
                                                stop_words = "english")

# embeddings
sentence_transformers <- reticulate::import("sentence_transformers")
sentence_model <- sentence_transformers$SentenceTransformer("all-MiniLM-L6-v2")
embeddings <- sentence_model$encode(data$text_clean, device = "mps")

# initiate model
model_eval1 <- py$bertopic$BERTopic(min_topic_size = 20L,
                                    umap_model = umap_model,
                                    representation_model = representation_model,
                                    vectorizer_model = vectorizer_model)

output <- model_eval1$fit_transform(data$text_clean,
                                    embeddings = embeddings)
time_model_test1 <- system.time({
  model_test1 <- fit_transform_model(cleaned_text = data$text_clean,
                  min_topic_size = 20,
                  ngram_range = c(1, 2),
                  embedding_model = "all-MiniLM-L6-v2",
                  accelerator = "mps",
                  diversity = NULL,
                  stopwords = TRUE,
                  random_state = 42)
})["elapsed"]

time_model_test2 <- system.time({
  model_test2 <- fit_transform_model(cleaned_text = data$text_clean,
                           min_topic_size = 20,
                           ngram_range = c(1, 2),
                           embedding_model = "all-MiniLM-L6-v2",
                           accelerator = NULL,
                           diversity = NULL,
                           stopwords = TRUE,
                           random_state = 42)
  })["elapsed"]

test_that("random state works", {
  # test titles the same
  expect_equal(model_eval1$get_topic_info()$Topic, model_test1$get_topic_info()$Topic)
  # test docs assigned to same topics
  expect_equal(model_eval1$get_document_info(data$text_clean)$Count, model_test1$get_document_info(data$text_clean)$Count)
})

test_that("min_topic_size works", {
  expect_true(any(model_test1$get_topic_info()$Count>20))
})

test_that("accelerator working", {
  expect_true(time_model_test2 > time_model_test1)
})

# test_that("ngram_range works", {
#   expect_true(any(model_test1$get_topic_info()$Count>20))
# })