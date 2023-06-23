# # # library
# library(reticulate)
# # BertopicR:::import_bertopic() # should I be running this here?
#
# # prep data for benchmark
# data <- bert_example_data %>%
#   janitor::clean_names() %>%
#   dplyr::mutate(text_clean = message, .before = message) %>%
#   ParseR::clean_text(text_var = text_clean,
#                      tolower = TRUE,
#                      hashtags = FALSE,
#                      mentions = FALSE,
#                      emojis = FALSE,
#                      punctuation = TRUE,
#                      digits = TRUE,
#                      in_parallel = TRUE) %>%
#   dplyr::distinct(text_clean, .keep_all = TRUE)
#
#
# # create umap model
# umap <- reticulate::import("umap")
# umap_model <- umap$UMAP(n_neighbors=15L,
#                         n_components=5L,
#                         min_dist=0.0,
#                         metric='cosine',
#                         random_state = 42L)
#
# # create representation model
# representation <- reticulate::import("bertopic.representation")
# representation_model <- representation$MaximalMarginalRelevance(diversity = 0.1)
#
# # create vectorizer model
# vectorizer <- reticulate::import("sklearn.feature_extraction.text")
# vectorizer_model <- vectorizer$CountVectorizer(ngram_range = tuple(1L,2L),
#                                                 stop_words = "english")
#
# # create embeddings
# sentence_transformers <- reticulate::import("sentence_transformers")
# sentence_model <- sentence_transformers$SentenceTransformer("all-MiniLM-L6-v2")
# embeddings <- sentence_model$encode(data$text_clean, device = "mps")
#
# # initiate first evaluation model
# model_eval1 <- py$bertopic$BERTopic(min_topic_size = 20L,
#                                     umap_model = umap_model,
#                                     representation_model = representation_model,
#                                     vectorizer_model = vectorizer_model)
#
# output <- model_eval1$fit_transform(data$text_clean,
#                                     embeddings = embeddings)
#
# # for test that random_state, min_topic_size, and accelerator work
# time_model_test1 <- system.time({
#   model_test1 <- bt_fit_transform_model(cleaned_text = data$text_clean,
#                                      min_topic_size = 20,
#                                      ngram_range = c(1, 2),
#                                      embedding_model = "all-MiniLM-L6-v2",
#                                      accelerator = "mps",
#                                      diversity = 0.1,
#                                      stopwords = TRUE,
#                                      random_state = 42)
# })["elapsed"]
#
# # for test that accelerator work
# time_model_test2 <- system.time({
#   model_test2 <- bt_fit_transform_model(cleaned_text = data$text_clean,
#                                      min_topic_size = 20,
#                                      ngram_range = c(1, 2),
#                                      embedding_model = "all-MiniLM-L6-v2",
#                                      accelerator = NULL,
#                                      diversity = 0.1,
#                                      stopwords = TRUE,
#                                      random_state = 42)
#   })["elapsed"]
#
# # for test that nr_topics work
# model_test3 <- bt_fit_transform_model(cleaned_text = data$text_clean,
#                                    nr_topics = 10)
#
# test_that("random state works", {
#   # test titles the same
#   expect_equal(model_eval1$get_topic_info()$Topic, model_test1[[1]]$get_topic_info()$Topic)
#   # test docs assigned to same topics
#   expect_equal(model_eval1$get_document_info(data$text_clean)$Count, model_test1[[1]]$get_document_info(data$text_clean)$Count)
# })
#
# test_that("min_topic_size works", {
#   expect_true(any(model_test1[[1]]$get_topic_info()$Count>20))
# })
#
# test_that("accelerator working", {
#   expect_true(time_model_test2 > time_model_test1)
# })
#
# test_that("nr_topics working", {
#   expect_true(model_test3[[1]]$get_topic_info() %>% nrow() == 10)
# })
#
