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

# test_that("bt_representation_openai errors on bad input", {
#   # expect_error(bt_representation_openai(), "str_detect.*api_key") # make sure to enter an api key that does not start with
#   
#   # Override readline to use mock inputs
#   with_mocked_bindings(
#     readline <- function(prompt = "") {
#       return("test")
#     },
#     {expect_error(bt_representation_openai(), "str_detect.*api_key")
#       }
#   )
#   
#   # Call the function and check for error
#   
# })
# 
# test_that("bt_representation_openai errors on bad input", {
#   # Mock the readline function to return a fixed string
#   with_mocked_bindings(input = list(readline = function(prompt = "") { return("test") }),
#                        {
#                          # Call the function and check for error
#                          expect_error(bt_representation_openai(), "str_detect.*api_key")
#                        })
# })
# 
# 

