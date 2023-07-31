test_that("bt_representation_keybert errors on bad input", {
  
  expect_error(bt_representation_keybert(top_n_words = "test"), "is.numeric.*top_n_words")
  expect_error(bt_representation_keybert(top_n_words = 5,
                                         nr_docs = "test"), "is.numeric.*nr_docs")
  expect_error(bt_representation_keybert(top_n_words = 5,
                                         nr_candidate_words = "test",
                                         nr_docs = 6), "is.numeric.*nr_candidate_words")
  expect_error(bt_representation_keybert(top_n_words = 5,
                                         nr_samples = "test",
                                         nr_candidate_words = 7,
                                         nr_docs = 6), "is.numeric.*nr_samples")
  expect_error(bt_representation_keybert(top_n_words = 5,
                                         random_state = "test",
                                         nr_samples = 5,
                                         nr_candidate_words = 7,
                                         nr_docs = 6), "is.numeric.*random_state")
})

test_that("bt_representation_keybert returns correct output on correct input", {
  representation_keybert <- bt_representation_keybert(top_n_words = 10,
                                                      nr_docs = 10,
                                                      nr_samples = 10,
                                                      nr_candidate_words = 10,
                                                      random_state = 10)
  
  expect_equal(representation_keybert$top_n_words, 10)
  expect_equal(representation_keybert$nr_repr_docs, 10)
  expect_equal(representation_keybert$nr_samples, 10)
  expect_equal(representation_keybert$nr_candidate_words, 10)
  expect_equal(representation_keybert$random_state, 10)
})

test_that("bt_representation_mmr errors on bad input", {
  
  expect_error(bt_representation_mmr(diversity = "test"), "is.numeric.*diversity")
  expect_error(bt_representation_mmr(diversity = 1.2), "diversity <=")
  expect_error(bt_representation_mmr(diversity = 0.1,
                                     top_n_words = "test"), "is.numeric.*top_n_words")
})

test_that("bt_representation_mmr returns correct output on correct input", {
  representation_mmr <- bt_representation_mmr(diversity = 0.1,
                                              top_n_words = 10)
  
  expect_equal(representation_mmr$top_n_words, 10)
  expect_equal(representation_mmr$diversity, 0.1)
})

test_that("bt_representation_openai errors on bad input", {

  # standard inputs
  expect_error(bt_representation_openai(openai_model = 6),
               "is.character.*openai_model.")
  expect_error(bt_representation_openai(chat = "test"),
               "is.logical.*chat")
  expect_error(bt_representation_openai(nr_docs = "test"),
               "is.numeric.*nr_docs")
  expect_error(bt_representation_openai(api_key = "test"),
               "str_detect.*api_key")
  expect_error(bt_representation_openai(exponential_backoff = "test"),
               "is.logical.*exponential_backoff")
  expect_error(bt_representation_openai(delay_in_seconds = "test"),
               "is.numeric.*delay_in_seconds")
  expect_error(bt_representation_openai(prompt = 8),
               "is.character.*prompt")
  expect_error(bt_representation_openai(openai_model = "test",
                                        api_key = Sys.getenv("OPENAI_API_KEY")),
               "The input model, test, is not an available OpenAI model.")
  expect_error(bt_representation_openai(openai_model = "gpt-3.5-turbo",
                                        api_key = Sys.getenv("OPENAI_API_KEY"),
                                        chat = FALSE),
               "If using a gpt model, you must specify chat = TRUE")
  
  # extra inputs
  expect_error(bt_representation_openai(test_input = "test",
                                        api_key = Sys.getenv("OPENAI_API_KEY")),
               "Bad argument\\(s\\) attempted to be sent to OpenAI\\(\\): test_input")
})

test_that("bt_representation_openai returns correct output on correct input", {
  
  representation_openai <- bt_representation_openai(api_key = Sys.getenv("OPENAI_API_KEY"),
                                        delay_in_seconds = 1,
                                        nr_docs = 1,
                                        exponential_backoff = TRUE,
                                        prompt = "test",
                                        diversity = 0.1)
  
  expect_equal(representation_openai$model, "text-ada-001")
  expect_equal(representation_openai$chat, FALSE)
  expect_equal(representation_openai$nr_docs, 1)
  expect_equal(representation_openai$exponential_backoff, 1)
  expect_equal(representation_openai$delay_in_seconds, 1)
  expect_equal(representation_openai$prompt, "test")
  expect_equal(representation_openai$diversity, 0.1)
})

test_that("bt_representation_hf errors on bad input", {
  
  bt <- reticulate::import("bertopic")
  model_unfitted <- bt$BERTopic()
  
  sentences <- stringr::sentences[1:50] # docs
  embeddings <- array(runif(100), dim = c(50, 2))
  model <- bt$BERTopic(
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
  )$fit(sentences, embeddings = embeddings) 
  
  # standard inputs
  expect_error(bt_representation_hf(task = 6),
               "is.character.*task")
  
  expect_error(bt_representation_hf(task = "text",
                                    hf_model = 6),
               "is.character.*hf_model")
  
  expect_error(bt_representation_hf(task = "text",
                                    hf_model = "text",
                                    documents = 6),
               "is.character.*documents")
  
  expect_error(bt_representation_hf(task = "text",
                                    hf_model = "text",
                                    documents = "text",
                                    default_prompt = NULL),
               "!is.null.*default_prompt.*custom_prompt")
  
  expect_error(bt_representation_hf(task = "text",
                                    hf_model = "text",
                                    documents = "text",
                                    default_prompt = 6),
               "is.character.*default_prompt.*is.null.*default_prompt")
  
  expect_error(bt_representation_hf(task = "text",
                                    hf_model = "text",
                                    documents = "text",
                                    custom_prompt = 6),
               "is.character.*custom_prompt.*is.null.*custom_prompt")
  
  expect_error(bt_representation_hf(task = "text",
                                    hf_model = "text",
                                    documents = "text",
                                    nr_samples = "test"),
               "is.numeric.*nr_samples")
  
  expect_error(bt_representation_hf(task = "text",
                                    hf_model = "text",
                                    documents = "text",
                                    nr_repr_docs = "test"),
               "is.numeric.*nr_repr_docs")
  
  expect_error(bt_representation_hf(task = "text",
                                    hf_model = "text",
                                    documents = "text",
                                    diversity = "test"),
               "is.numeric.*diversity")
  
  # model detection is working:
  expect_error(bt_representation_hf(task = "text",
                                    hf_model = "text",
                                    documents = "text",
                                    topic_model = "test"),
               "Model should be a BERTopic model")
  
  expect_error(bt_representation_hf(task = "text",
                                    hf_model = "text",
                                    documents = "text",
                                    topic_model = model_unfitted),
               regexp = "BERTopic model is not fitted, use bt_fit_model to fit.")
  
  
  # extra inputs
  expect_error(bt_representation_hf(test_input = "test",
                                    task = "text",
                                    hf_model = "text",
                                    documents = "text",
                                    topic_model = model),
               "Bad argument\\(s\\) attempted to be sent to pipeline\\(\\): test_input")
})



test_that("bt_representation_openai returns correct output on correct input", {
  
  bt <- reticulate::import("bertopic")
  sentences <- stringr::sentences[1:100] # docs
  embeddings <- array(runif(200), dim = c(100, 2))
  model <- bt$BERTopic(
    embedding_model = bt_base_embedder(),
    umap_model = bt_base_reducer(),
    nr_topics = 2L
  )$fit(sentences, embeddings = embeddings) 
  
  nr_topics <- model$get_topic_info() %>% nrow()
  
  representation_hf <- bt_representation_hf(task = "text-generation",
                                                 hf_model = "gpt2",
                                                 documents = sentences,
                                                 topic_model = model)
  
  expect_equal(nr_topics, length(representation_hf))
  expect_true(is.character(unlist(representation_hf)))
  expect_true(representation_hf[[1]] != representation_hf[[2]])
})
