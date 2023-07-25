test_that("bt_merge_topics errors on unpermitted input", {

  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:50]
  embeddings <- array(runif(100), dim = c(50, 2))
  model <- bt$BERTopic(
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
  )$fit(sentences, embeddings = embeddings)
  model_unfitted <- bt$BERTopic()

  # model detection is working:
  expect_error(bt_merge_topics(fitted_model = "test"),
               regexp = "Model should be a BERTopic model")


  # fitted model detection is working:
  expect_error(bt_merge_topics(fitted_model = model_unfitted),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")

  # topics to merge in correct format:
  expect_error(bt_merge_topics(fitted_model = model,
                               topics_to_merge = c(-1,0)),
               regexp = "topics_to_merge must be a list or where you want to perform multiple merges, a list of lists/vectors")

  expect_error(bt_merge_topics(fitted_model = model,
                               topics_to_merge = list("topic-1","topic 0")),
               regexp = "opics must be numeric")
  
  # only accepts documents in correct form:
  expect_error(bt_merge_topics(fitted_model = model,
                               documents = c(-1,0),
                               topics_to_merge = list(-1,0)),
               regexp = "documents must be of type character")
})

test_that("bt_merge_topics merges topics", {

  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:200]
  embeddings <- array(runif(500), dim = c(200, 2))
  model <- bt$BERTopic(
    min_topic_size = 2L,
    nr_topics = 4L,
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
  )$fit(sentences, embeddings = embeddings)
  n_topics <- model$get_topic_info() %>% nrow()
  merged_model <- bt_merge_topics(fitted_model = model,
                                  documents = sentences,
                                  topics_to_merge = list(c(-1, 1),
                                                         c(0, 2)))

  # merges topics:
  expect_equal(n_topics,
               merged_model$get_topic_info() %>% 
                 nrow() + 2)
}) 

test_that("bt_probability_matrix outputs matrix and errors appropriately", {
  
  bt <- reticulate::import("bertopic")
  skl <- reticulate::import("sklearn.cluster")
  
  sentences <- stringr::sentences[1:50]
  embeddings <- array(runif(100), dim = c(50, 2))
  model_hdb <- bt$BERTopic(
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
  )$fit(sentences, embeddings = embeddings)
  
  model_skl <- bt$BERTopic(
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
    hdbscan_model = skl$KMeans()
  )$fit(sentences, embeddings = embeddings)
  model_unfitted <- bt$BERTopic()
  
  # recognises model
  expect_error(bt_probability_matrix(fitted_model = model_unfitted),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")
  
  expect_error(bt_probability_matrix(fitted_model = "text"),
               regexp = "Model should be a BERTopic model")
  
  expect_error(bt_probability_matrix(fitted_model = model_skl),
               regexp = "^Clustering method is")
  
  expect_true(is.array(bt_probability_matrix(fitted_model = model_hdb)))
  
})

test_that("bt_outlier_probs errors on incorrect input", {

  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:50]
  embeddings <- array(runif(100), dim = c(50, 2))
  model <- bt$BERTopic(
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
  )$fit(sentences, embeddings = embeddings)
  model_unfitted <- bt$BERTopic()
  n_topics <- model$get_topic_info() %>% nrow()
  prob_matrix <- matrix(data = rep(1, 50*(n_topics-1)), ncol = n_topics-1)

  # model detection is working:
  expect_error(bt_outliers_probs(fitted_model = model_unfitted),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")

  expect_error(bt_outliers_probs(fitted_model = "text"),
               regexp = "Model should be a BERTopic model")

  # matrix detection is working:
  expect_error(bt_outliers_probs(fitted_model = model,
                                 probability_matrix = list(100)),
               regexp = "topic-document probabilitiy matrix must be a matrix")

  # checks correct number of topics in matrix
  expect_error(bt_outliers_probs(fitted_model = model,
                                 probability_matrix = prob_matrix))

  # checks correct number of documents in matrix
  expect_error(bt_outliers_probs(fitted_model = model,
                                 probability_matrix = prob_matrix))

  # checks same number of docs and topics
  expect_error(bt_outliers_probs(fitted_model = model,
                                 documents = sentences,
                                 topics = replicate(10, 1),
                                 probability_matrix = prob_matrix,
                                 threshold = 0.3))
})

test_that("bt_outliers_probs returns correct output", {

  # setup
  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:50]
  embeddings <- array(runif(100), dim = c(50, 2))
  model <- bt$BERTopic(
    min_topic_size = 2L,
    nr_topics = 3L,
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
  )$fit(sentences, embeddings = embeddings)
  n_topics <- model$get_topic_info() %>% nrow()
  prob_matrix <- matrix(data = rep(1, length(sentences)*(n_topics-1)), ncol = n_topics-1)

  # run function
  df <- bt_outliers_probs(fitted_model = model,
                    documents = sentences,
                    topics = model$get_document_info(sentences)$Topic,
                    probability_matrix = prob_matrix,
                    threshold = 0.01)

  # returns df with document, old topics, new topics:
  expect_true(all(df[2] == model$get_document_info(sentences)$Topic))
  expect_true((df %>% dplyr::filter(current_topics == -1) %>% nrow()) >
                (df %>% dplyr::filter(new_topics == -1) %>% nrow()))

})

test_that("bt_outlier_embeddings errors on incorrect input", {

  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:50] # docs
  model <- bt$BERTopic() # initiate model
  model$fit(sentences) # fit model
  model_unfitted <- bt$BERTopic() # unfitted model
  n_topics <- model$get_topic_info() %>% length() # number topics

  # model detection is working:
  expect_error(bt_outliers_embeddings(fitted_model = model_unfitted),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")

  # checking for unfitted model
  expect_error(bt_outliers_embeddings(fitted_model = "text"),
               regexp = "Model should be a BERTopic model")


  # checks same number of topics as docs
  expect_error(bt_outliers_embeddings(fitted_model = model,
                                      documents = sentences,
                                      topics = list(rep(1, 10))))

  # checks correct number of docs embedded
  expect_error(bt_outliers_embeddings(fitted_model = model,
                                 documents = sentences,
                                 topics = replicate(100, 1),
                                 embeddings = array(runif(500), dim = c(50, 10))))


})

test_that("bt_outliers_embeddings returns correct output", {

  # setup
  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:50]
  embeddings <- array(runif(100), dim = c(50, 2))
  model <- bt$BERTopic(
    min_topic_size = 2L,
    nr_topics = 3L,
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
  )$fit(sentences, embeddings = embeddings)
  n_topics <- model$get_topic_info() %>% nrow()

  # run function
  df <- bt_outliers_probs(fitted_model = model,
                          documents = sentences,
                          topics = model$get_document_info(sentences)$Topic,
                          probability_matrix = embeddings,
                          threshold = 0.01)

  # returns df with document, old topics, new topics:
  expect_true(all(df[2] == model$get_document_info(sentences)$Topic))
  expect_true((df %>% dplyr::filter(current_topics == -1) %>% nrow()) > (df %>% dplyr::filter(new_topics == -1) %>% nrow()))

})

test_that("bt_outlier_tokenset_similarity errors on incorrect input", {

  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:50] # docs
  embeddings <- array(runif(100), dim = c(50, 2))
  model <- bt$BERTopic(
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
  )$fit(sentences, embeddings = embeddings)
  model_unfitted <- bt$BERTopic() # unfitted model
  n_topics <- model$get_topic_info() %>% length() # number topics

  # model detection is working:
  expect_error(bt_outliers_tokenset_similarity(fitted_model = model_unfitted),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")


  # checking for unfitted model
  expect_error(bt_outliers_tokenset_similarity(fitted_model = "text"),
               regexp = "Model should be a BERTopic model")


  # checks same number of topics for docs
  expect_error(bt_outliers_tokenset_similarity(fitted_model = model,
                                      documents = sentences,
                                      topics = list(rep(1, 50))))
})

test_that("bt_outliers_tokenset_similarty returns correct output", {

  # setup
  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:100]
  model <- bt$BERTopic(nr_topics = 2L)
  n_topics <-  model$fit(sentences)$get_topic_info() %>% nrow()

  # run function
  df <- bt_outliers_tokenset_similarity(fitted_model = model,
                          documents = sentences,
                          topics = model$get_document_info(sentences)$Topic,
                          threshold = 0.01,
                          window = 5,
                          stride = 2,
                          batch_size = 400,
                          padding = TRUE)

  # returns df with document, old topics, new topics:
  expect_true(all(df[2] == model$get_document_info(sentences)$Topic))
  expect_true((df %>% dplyr::filter(current_topics == -1) %>% nrow()) > 
                (df %>% dplyr::filter(new_topics == -1) %>% nrow()))

})

test_that("bt_outlier_ctfidf only accepts correct objects", {

  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:50] # docs
  embeddings <- array(runif(100), dim = c(50, 2))
  model <- bt$BERTopic(
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
  )$fit(sentences, embeddings = embeddings)  
  model_unfitted <- bt$BERTopic() # unfitted model
  n_topics <- model$get_topic_info() %>% length() # number topics

  # model detection is working:
  expect_error(bt_outliers_ctfidf(fitted_model = model_unfitted),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")


  # checking for unfitted model
  expect_error(bt_outliers_ctfidf(fitted_model = "text"),
               regexp = "Model should be a BERTopic model")


  # checks same number of topics for docs
  expect_error(bt_outliers_ctfidf(fitted_model = model,
                                 documents = sentences,
                                 topics = list(rep(1, length(sentences) - 10))))
})

test_that("bt_outliers_tokenset_similarty returns correct output", {

  # setup
  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:100]
  model <- bt$BERTopic(nr_topics = 2L)
  n_topics <- model$fit(sentences)$get_topic_info() %>% nrow()

  # run function
  df <- bt_outliers_ctfidf(fitted_model = model,
                            documents = sentences,
                            topics = model$get_document_info(sentences)$Topic,
                            threshold = 0.01)

  # returns df with document, old topics, new topics:
  expect_true(all(df[2] == model$get_document_info(sentences)$Topic))
  expect_true((df %>% dplyr::filter(current_topics == -1) %>% nrow()) > (df %>% dplyr::filter(new_topics == -1) %>% nrow()))

})

test_that("bt_update_topics errors on incorrect input", {
  
  # setup
  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:50]
  embeddings <- array(runif(100), dim = c(50, 2))
  model <- bt$BERTopic(
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
  )$fit(sentences, embeddings = embeddings)
  model_unfitted = bt$BERTopic()
  vectoriser_model <- bt_make_vectoriser(ngram_range = c(1, 3))
  ctfidf <- bt_make_ctfidf(reduce_frequent_words = TRUE, bm25_weighting = FALSE)
  
  # model detection is working:
  expect_error(bt_merge_topics(fitted_model = "test"),
               regexp = "Model should be a BERTopic model")
  
  # fitted model detection is working:
  expect_error(bt_merge_topics(fitted_model = model_unfitted),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")
  
  expect_error(bt_update_topics(fitted_model = model,
                     documents = "text",
                     vectoriser_model = "test"))
  
  expect_error(bt_update_topics(fitted_model = model,
                     documents = "text",
                     ctfidf_model = "test"))
  
})

test_that("bt_update_topics updates topics", {
  
  # setup
  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:50]
  new_topics <- sample(1:5, 50, replace = TRUE)
  embeddings <- array(runif(100), dim = c(50, 2))
  model <- bt$BERTopic(
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
  )$fit(sentences, embeddings = embeddings)
  vectoriser_model <- bt_make_vectoriser(ngram_range = c(1, 3), min_frequency = 1)
  
  expect_equal(bt_update_topics(fitted_model = model,
                                documents = sentences,
                                new_topics = new_topics,
                                vectoriser_model = vectoriser_model)$get_document_info(sentences)$Topic,
               new_topics)

})