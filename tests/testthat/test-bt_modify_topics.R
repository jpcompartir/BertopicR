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
