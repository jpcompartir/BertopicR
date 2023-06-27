test_that("bt_fit accepts a bertopic model, raises an error if not",{
  bertopic <- reticulate::import('bertopic')
  documents <- stringr::sentences[1:40]
  embeddings <- array(runif(n = 240), dim = c(40, 6))

  #Returns error if NULL object fed in to model
  empty_model <- NULL
  expect_error(bt_fit_model(empty_model,documents, embeddings))

  empty_model <- bertopic$BERTopic()
  expect_silent(bt_fit_model(empty_model,documents, embeddings))
})

test_that("bt_fit_model raises an error if the dimensions of embeddings and documents don't match up, and doesn't if they do", {
  bertopic <- reticulate::import("bertopic")
  model <- bertopic$BERTopic()

  documents <- stringr::sentences[1:100]
  incorrect_embeddings <- array(runif(100), dim = c(50, 2))

  #Raise an error when they don't match up.
  expect_error(bt_fit_model(model = model, documents = documents, embeddings = incorrect_embeddings), regexp = "dimensions of your documents and embeddings do not")

  correct_embeddings <- array(runif(200), dim = c(100, 2))

  #Run without raising an error
  expect_silent(bt_fit_model(model = model, documents = documents, embeddings = correct_embeddings))

  #Check we error if not array or data.frame provided
  expect_error(bt_fit_model(model = model, documents = "hello",
                        embeddings = "hello"))

  expect_error(
    bt_fit_model(model = model, documents = 1, embeddings = array()),
    regexp = "is\\.character\\(documents"
  )
})


