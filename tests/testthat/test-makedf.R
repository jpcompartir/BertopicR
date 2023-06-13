data <- bert_example_data %>%
  janitor::clean_names() %>%
  dplyr::mutate(text_clean = message, .before = message) %>%
  ParseR::clean_text(text_var = text_clean,
                     tolower = TRUE,
                     hashtags = FALSE,
                     mentions = FALSE,
                     emojis = FALSE,
                     punctuation = TRUE,
                     digits = TRUE,
                     in_parallel = TRUE) %>%
  dplyr::distinct(text_clean, .keep_all = TRUE)

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
df <- makedf(df = data,
             model = model, 
             embeddings = embeddings,
             text_var = text_clean,
             date_var = created_time)

test_that("expected columns are present", {
  expect_true("text_clean" %in% colnames(df))
  expect_true("topic" %in% colnames(df))
  expect_true("V1" %in% colnames(df))
  expect_true("V2" %in% colnames(df))
  expect_true("document" %in% colnames(df))
  expect_true("topic_title" %in% colnames(df))
  expect_true("permalink" %in% colnames(df))
})