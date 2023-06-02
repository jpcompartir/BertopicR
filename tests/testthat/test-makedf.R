data <- bert_example_data %>%
  janitor::clean_names() %>% # clean column titles
  mutate(text_clean = message, .before = message) %>% # add column for clean text
  mutate(text_clean = tolower(text_clean)) %>% # all posts lower case
  limpiar_tags(text_var = text_clean, hashtag = F) %>%  # remove mentions
  mutate(text_clean = str_remove_all(text_clean, "@user"), # remove mentions
         text_clean = str_remove_all(text_clean, "#\\S+")) %>% # remove hashtags
  limpiar_url(text_var = text_clean) %>% # remove urls
  limpiar_emojis(text_var = text_clean) %>% # remove emojis
  limpiar_spaces(text_var = text_clean) %>% # remove unnecessary spaces
  distinct(text_clean, .keep_all = TRUE)  # remove duplicates

# import sentence transformer 
sentence_transformers <- reticulate::import("sentence_transformers")

# choose sentence transformer to use and specify "mps" accelerator
sentence_model_gpu <- sentence_transformers$SentenceTransformer("all-MiniLM-L6-v2", device = "mps")

# embed text using sentence transformer
embeddings <- sentence_model_gpu$encode(data$text_clean)

# intiate model
model <- py$bertopic$BERTopic()

# fit the model
model_output <- model$fit_transform(documents = data$text_clean,
                                    embeddings = embeddings)
# run function
df <- makedf(model = model, 
             embeddings = embeddings, 
             original_text = data$message, 
             cleaned_text = data$text_clean, 
             date = data$created_time, 
             sentiment = data$sentiment, 
             permalink = data$permalink)

test_that("expected columns are present", {
  expect_true("text_clean" %in% colnames(df))
  expect_true("topic" %in% colnames(df)) # only checking 2 cols from get_document_info()
  expect_true("name" %in% colnames(df))
  expect_true("V1" %in% colnames(df))
  expect_true("V2" %in% colnames(df))
  expect_true("document" %in% colnames(df))
  expect_true("text_og" %in% colnames(df))
  expect_true("date" %in% colnames(df))
  expect_true("sentiment" %in% colnames(df))
  expect_true("permalink" %in% colnames(df))
})