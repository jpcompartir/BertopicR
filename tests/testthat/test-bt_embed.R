test_that("bt_embed function is raising errors when it should", {

  expect_error(
    bt_embed(
      documents = "text",
      embedding_model = 1L
      ),
    regexp = 'is.character\\(embedding_model\\) is not TRUE'
    )

  expect_error(
    bt_embed(
      documents = "text",
      embedding_model = "all-MiniLM-L6-v2",
      accelerator = 1),
    regexp = "is.character\\(accelerator\\) | is.null\\(accelerator\\) is not TRUE")

  expect_error(
    bt_embed(
      documents = "text",
      embedding_model = "all-MiniLM-L6-v2",
      accelerator = "mps",
      progress_bar = 1L),
    regexp = "is.logical\\(progress_bar\\) is not TRUE"
  )

})

test_that("bt_embed function returns an array when fed correct parameters", {

  #Funcion takes one text
  test_embeddings <- bt_embed(documents = "text",
           embedding_model = "all-MiniLM-L6-v2",
           accelerator = "mps",
           progress_bar = TRUE)

  expect_equal(384, dim(test_embeddings))

  #Function takes a vector of texts
  embeddings_list <- bt_embed(documents = c("this is some text", "this is more text"))

  expect_true(all(class(embeddings_list) == c("matrix", "array")))

  #Function passes ... appropriately, here we pass batch_size through ..., as batch_size is not a named argument in our function, we'll know ... is behaving as expected if we're able to pass batch_size and it then has the desired behaviour.
  ellipsis <- bt_embed(documents = c("this is some text", "this is more text"), batch_size = 1L)

  expect_equal(class(ellipsis), c('matrix', "array"))

  ellipsis_model <- attributes(ellipsis)[["embedding_model"]]
  #Check attributes have been assigned
  expect_equal("all-MiniLM-L6-v2",ellipsis_model)
})

