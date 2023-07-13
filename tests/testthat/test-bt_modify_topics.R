test_that("bt_merge_topics only accepts correct objects", {

  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:200]
  model <- bt$BERTopic()
  model$fit(sentences)
  model_unfitted <- bt$BERTopic()

  # model detection is working:
  expect_error(bt_merge_topics(model = "test",
                               documents = sentences,
                               topics_to_merge = list(-1,0)),
               regexp = "model should be a BERTopic model")


  # fitted model detection is working:
  expect_error(bt_merge_topics(model = model_unfitted,
                               documents = sentences,
                               topics_to_merge = list(-1,0)),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")

  # topics to merge in correct format:
  expect_error(bt_merge_topics(model = model,
                               documents = sentences,
                               topics_to_merge = c(-1,0)),
               regexp = "topics_to_merge must be a list or where you want to perform multiple merges, a list of lists/vectors")


  # only accepts documents in correct form:
  expect_error(bt_merge_topics(model = model,
                               documents = c(-1,0),
                               topics_to_merge = list(-1,0)),
               regexp = "documents must be of type character")

  # accepts correct inputs
  expect_silent(bt_merge_topics(model = model,
                                documents = sentences,
                                topics_to_merge = list(-1,0)))
})

test_that("bt_merge_topics merges topics", {

  bt <- reticulate::import("bertopic") # do I need to run this again?
  sentences <- stringr::sentences[1:200]
  model <- bt$BERTopic()
  model$fit(sentences)

  # merges two topics:
  expect_equal(nrow(model$get_topic_info()),
               bt_merge_topics(model = model,
                               documents = sentences,
                               topics_to_merge = list(-1,0))$get_topic_info() %>%
                 nrow() + 1)

  model <- bt$BERTopic()
  model$fit(sentences)

  # merges multiple topics:
  expect_equal(nrow(model$get_topic_info()),
               bt_merge_topics(model = model,
                               documents = sentences,
                               topics_to_merge = list(c(-1, 0),
                                                      c(1, 2)))$get_topic_info() %>%
                 nrow() + 2)
})

test_that("bt_outlier_probs errors on incorrect input", {
  
  bt <- reticulate::import("bertopic") 
  sentences <- stringr::sentences[1:100]
  model <- bt$BERTopic()
  model$fit(sentences)
  model_unfitted <- bt$BERTopic()
  n_topics <- model$get_topic_info() %>% nrow()
  
  # model detection is working:
  expect_error(bt_outliers_probs(model = model_unfitted,
                                 documents = sentences,
                                 topics = list(rep(1, 100)),
                                 probability_matrix = matrix(data = rep(1, 200), nrow = 100),
                                 threshold = 0.1),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")
  
  expect_error(bt_outliers_probs(model = "text",
                                 documents = sentences,
                                 topics = list(rep(1, 100)),
                                 probability_matrix = matrix(data = rep(1, 200), nrow = 100),
                                 threshold = 0.1),
               regexp = "model should be a BERTopic model")
  
  # matrix detection is working:
  expect_error(bt_outliers_probs(model = model,
                                 documents = sentences,
                                 topics = list(rep(1, 100)),
                                 probability_matrix = list(100),
                                 threshold = 0.1),
               regexp = "topic-document probabilitiy matrix must be a matrix")
  
  
  # checks correct number of topics in matrix
  expect_error(bt_outliers_probs(model = model,
                                 documents = sentences,
                                 topics = replicate(100, 1),
                                 probability_matrix = matrix(data = rep(1, 100*(n_topics-2)), ncol = n_topics-2),
                                 threshold = 0.1))
  
  # checks correct number of documents in matrix
  expect_error(bt_outliers_probs(model = model,
                                 documents = sentences,
                                 topics = list(rep(1, 100)),
                                 probability_matrix = matrix(data = rep(1, 50*(n_topics-1)), ncol = n_topics-1),
                                 threshold = 0.1))
})

test_that("bt_outliers_probs returns correct output", {
  
  # setup
  bt <- reticulate::import("bertopic") 
  sentences <- stringr::sentences[1:100]
  model <- bt$BERTopic()
  model$fit(sentences)
  n_topics <- model$get_topic_info() %>% nrow()
  
  # run function
  df <- bt_outliers_probs(model = model,
                    documents = sentences,
                    topics = model$get_document_info(sentences)$Topic,
                    probability_matrix = matrix(runif(100 * (n_topics-1)), nrow = 100),
                    threshold = 0.01)
  
  # returns df with document, old topics, new topics:
  expect_true(all(df[2] == model$get_document_info(sentences)$Topic))
  expect_true((df %>% dplyr::filter(old_topics == -1) %>% nrow()) > (df %>% dplyr::filter(new_topics == -1) %>% nrow()))
  
})

test_that("bt_outlier_embeddings errors on incorrect input", {

  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:100] # docs
  model <- bt$BERTopic() # intiate model
  model$fit(sentences) # fit model
  model_unfitted <- bt$BERTopic() # unfitted model
  n_topics <- model$get_topic_info() %>% length() # number topics

  # model detection is working:
  expect_error(bt_outliers_embeddings(model = model_unfitted,
                                 documents = sentences,
                                 topics = list(rep(1, 100)),
                                 embeddings = array(runif(500), dim = c(100, 5)),
                                 threshold = 0.1),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")

  
  # checking for unfitted model
  expect_error(bt_outliers_embeddings(model = "text",
                                 documents = sentences,
                                 topics = list(rep(1, 100)),
                                 embeddings = array(runif(500), dim = c(100, 5)),
                                 threshold = 0.1),
               regexp = "model should be a BERTopic model")


  # checks correct number of docs embedded
  expect_error(bt_outliers_embeddings(model = model,
                                 documents = sentences,
                                 topics = replicate(100, 1),
                                 embeddings = array(runif(500), dim = c(50, 10)),
                                 threshold = 0.1))

  # checks same number of topics for docs
  expect_error(bt_outliers_embeddings(model = model,
                                 documents = sentences,
                                 topics = list(rep(1, 50)),
                                 embeddings = array(runif(500), dim = c(100, 5)),
                                 threshold = 0.1))

})

test_that("bt_outliers_embeddings returns correct output", {
  
  # setup
  bt <- reticulate::import("bertopic") 
  sentences <- stringr::sentences[1:100]
  model <- bt$BERTopic()
  model$fit(sentences)
  n_topics <- model$get_topic_info() %>% nrow()
  
  # run function
  df <- bt_outliers_probs(model = model,
                          documents = sentences,
                          topics = model$get_document_info(sentences)$Topic,
                          probability_matrix = matrix(runif(100 * (n_topics-1)), nrow = 100),
                          threshold = 0.01)
  
  # returns df with document, old topics, new topics:
  expect_true(all(df[2] == model$get_document_info(sentences)$Topic))
  expect_true((df %>% dplyr::filter(old_topics == -1) %>% nrow()) > (df %>% dplyr::filter(new_topics == -1) %>% nrow()))
  
})

test_that("bt_outlier_tokenset_similarity errors on incorrect input", {
  
  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:100] # docs
  model <- bt$BERTopic() # intiate model
  model$fit(sentences) # fit model
  model_unfitted <- bt$BERTopic() # unfitted model
  n_topics <- model$get_topic_info() %>% length() # number topics
  
  # model detection is working:
  expect_error(bt_outliers_tokenset_similarity(model = model_unfitted,
                                      documents = sentences,
                                      topics = list(rep(1, 100)),
                                      threshold = 0.1),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")
  
  
  # checking for unfitted model
  expect_error(bt_outliers_tokenset_similarity(model = "text",
                                      documents = sentences,
                                      topics = list(rep(1, 100)),
                                      threshold = 0.1),
               regexp = "model should be a BERTopic model")
  
  
  # checks same number of topics for docs
  expect_error(bt_outliers_tokenset_similarity(model = model,
                                      documents = sentences,
                                      topics = list(rep(1, 50)),
                                      threshold = 0.1))
})

test_that("bt_outliers_tokenset_similarty returns correct output", {
  
  # setup
  bt <- reticulate::import("bertopic") 
  sentences <- stringr::sentences[1:100]
  model <- bt$BERTopic()
  model$fit(sentences)
  n_topics <- model$get_topic_info() %>% nrow()
  
  # run function
  df <- bt_outliers_tokenset_similarity(model = model,
                          documents = sentences,
                          topics = model$get_document_info(sentences)$Topic,
                          threshold = 0.01,
                          window = 5,
                          stride = 2,
                          batch_size = 400,
                          padding = TRUE)
  
  # returns df with document, old topics, new topics:
  expect_true(all(df[2] == model$get_document_info(sentences)$Topic))
  expect_true((df %>% dplyr::filter(old_topics == -1) %>% nrow()) > (df %>% dplyr::filter(new_topics == -1) %>% nrow()))
  
})

test_that("bt_outlier_ctfidf only accepts correct objects", {
  
  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:100] # docs
  model <- bt$BERTopic() # intiate model
  model$fit(sentences) # fit model
  model_unfitted <- bt$BERTopic() # unfitted model
  n_topics <- model$get_topic_info() %>% length() # number topics
  
  # model detection is working:
  expect_error(bt_outliers_ctfidf(model = model_unfitted,
                                               documents = sentences,
                                               topics = list(rep(1, 100)),
                                               threshold = 0.1),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")
  
  
  # checking for unfitted model
  expect_error(bt_outliers_ctfidf(model = "text",
                                               documents = sentences,
                                               topics = list(rep(1, 100)),
                                               threshold = 0.1),
               regexp = "model should be a BERTopic model")
  
  
  # checks same number of topics for docs
  expect_error(bt_outliers_ctfidf(model = model,
                                               documents = sentences,
                                               topics = list(rep(1, 50)),
                                               threshold = 0.1))

  
  
})

test_that("bt_outliers_tokenset_similarty returns correct output", {
  
  # setup
  bt <- reticulate::import("bertopic") 
  sentences <- stringr::sentences[1:100]
  model <- bt$BERTopic()
  model$fit(sentences)
  n_topics <- model$get_topic_info() %>% nrow()
  
  # run function
  df <- bt_outliers_ctfidf(model = model,
                            documents = sentences,
                            topics = model$get_document_info(sentences)$Topic,
                            threshold = 0.01)
  
  # returns df with document, old topics, new topics:
  expect_true(all(df[2] == model$get_document_info(sentences)$Topic))
  expect_true((df %>% dplyr::filter(old_topics == -1) %>% nrow()) > (df %>% dplyr::filter(new_topics == -1) %>% nrow()))
  
})
  