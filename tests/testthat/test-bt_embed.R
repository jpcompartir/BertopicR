#testing bt_make_embedder ----
test_that("bt_make_embedder fails when it should and succeeds otherwise",{
  expect_error(bt_make_embedder("all-minilm-l6-v10"),
               regexp = "Repository Not Found")

  expect_silent(bt_make_embedder("all-minilm-l6-v2"))

})

test_that("bt_make_embedder returns the appropriate object", {
  embedder <- bt_make_embedder("all-minilm-l6-v2")

  expect_true(grepl("^sentence_tran",class(embedder)[[1]]))
  expect_true("encode" %in% names(embedder))

  #Check we have an attribute named embedding model
  embedder_attributes <- attributes(embedder)
  expect_true("embedding_model" %in% names(embedder_attributes))

})

#Testing bt_do_embedding ----

test_that("bt_do_embedding function is raising errors when it should", {

  embedder <- bt_make_embedder("all-minilm-l6-v2")
  expect_error(
    bt_do_embedding(
      embedder = 1L,
      documents = "text",
      ),
    regexp = 'embedder should be a sentence transformers model'
    )

  expect_error(
    bt_do_embedding(
      embedder = embedder,
      documents = "text",
      accelerator = 1),
    regexp = "is.character\\(accelerator\\) | is.null\\(accelerator\\) is not TRUE")

  expect_error(
    bt_do_embedding(
      embedder = embedder,
      documents = "text",
      accelerator = "mps",
      progress_bar = 1L),
    regexp = "is.logical\\(progress_bar\\) is not TRUE"
  )

})

test_that("bt_do_embedding function returns an array when fed correct parameters and has appropriate attributes", {

  embedder <- bt_make_embedder("all-minilm-l6-v2")

  #Function takes one text
  test_embeddings <- bt_do_embedding(
    embedder = embedder,
    documents = "text",
    accelerator = "mps",
    progress_bar = TRUE)

  expect_equal(384, dim(test_embeddings))

  #Function takes a vector of texts
  embeddings_list <- bt_do_embedding(
    embedder = embedder,
    documents = c("this is some text", "this is more text"))

  expect_true(all(class(embeddings_list) == c("matrix", "array")))

  #Function passes ... appropriately, here we pass batch_size through ..., as batch_size is not a named argument in our function, we'll know ... is behaving as expected if we're able to pass batch_size and it then has the desired behaviour.
  ellipsis <- bt_do_embedding(
    embedder = embedder,
    documents = c("this is some text", "this is more text"),
    batch_size = 1L)

  expect_equal(class(ellipsis), c('matrix', "array"))

  #Check our ellipsis object has 3 attributes
  expect_equal(length(attributes(ellipsis)), 2)

  #Check n_documents attributes is as it should be (n_doc doesn't have to exact match)
  ellipsis_docs <- attr(ellipsis, "n_doc")
  expect_equal(ellipsis_docs, 2)
})

test_that("embedding_model attribute persists when it should and doesn't break stuff wen it doesn't",{
  embedder <- bt_make_embedder("all-minilm-l6-v2")
  expect_true("embedding_model" %in% names(attributes(embedder)))

  embeddings <- bt_do_embedding(embedder, "text")
  expect_true("embedding_model" %in% names(attributes(embeddings)))

  attr(embedder, "embedding_model") <- NULL
  expect_message(bt_do_embedding(embedder, "text"), regexp = "No embedding_mod")
})
