#Testing this is actually pretty sophisticated, so first cover the named arguments, then ellipsis separately, then an output.

test_that("bt_reducer stops when it's given bad arguments",{

  #Make an empty array to feed into the mandatory argument (which should be array or df)
  array <- array()
  expect_error(bt_reducer(embeddings = array, return_value = "reduces"), "should be one of")
  expect_error(bt_reducer(embeddings = array, n_neighbors = "15"), "is.numeric.*n_neighb")
  expect_error(bt_reducer(embeddings = array, n_components = TRUE), "is.numeric.*n_components")
  expect_error(bt_reducer(embeddings = array, min_dist = "12"))
  expect_error(bt_reducer(embeddings = array, random_state = "42"), "is.numeric.*random_state")
  expect_error(bt_reducer(embeddings = array, metric = 2), "is.character.*metric" )
})

test_that("bt_reducer accepts an array or a data frame as embeddings", {
  set.seed(12)
  digits <- runif(n = 368*20, min = 0, max = 1)

  #Make an array and a data frame for this test
  my_array <- array(digits, dim = c(20, 368))
  my_df <- as.data.frame(my_array)

  #Check we have a 20x5 array after inputting an array
  reducer_array <- bt_reducer(my_array, verbose = FALSE)
  expect_equal(dim(reducer_array$reduced_embeddings), c(20, 5))

  #Check we have a 20x5 array after inputting a df
  reducer_df <- bt_reducer(my_df, verbose = FALSE)
  expect_equal(dim(reducer_df$reduced_embeddings), dim(reducer_array$reduced_embeddings))
})

test_that("bt_reducer's return_value argument is working as expected", {
  digits <- runif(n = 368*20, min = 0, max = 1)

  #Make an array and a data frame for this test
  my_array <- array(digits, dim = c(20, 368))
  my_df <- as.data.frame(my_array)

  reducer <- bt_reducer(embeddings = my_array, return_value = c("reducer"), n_neighbors = 5L, n_components = 2L, min_dist = 0.001, metric = "cosine", random_state = 32L, verbose = FALSE)

  expect_equal(reducer$min_dist, 0.001)
  expect_equal(reducer$verbose, FALSE)
  expect_equal(reducer$metric, "cosine")
  expect_equal(reducer$n_neighbors, 5L)
  expect_equal(reducer$n_components, 2L)

  reduced <- bt_reducer(embeddings = my_array, return_value = "reduced_embeddings", verbose = FALSE)

  #Now check our base model has fit and transform
  expect_true(all(names(reduced$base_reducer) == c("fit", "transform")))

  x <- reduced$base_reducer$fit("xyz")
  expect_true(grepl("^bertopic", class(x)[[1]]))

  #Check it doeosn't just match anything
  expect_false("xyz" == reduced$base_reducer$transform(X = "XYZ"))
  #Check it matches the input
  expect_equal("xyz", reduced$base_reducer$transform(X = "xyz"))

})

#Integration test with bt_embed
test_that("bt_embed and bt_reducer work together",{
  documents <- bert_example_data %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(message)) %>%
    head(10) %>%
    dplyr::pull(message)

  embeddings <- bt_embed(documents = documents)
  reducer <- bt_reducer(embeddings, return_value = "reduced_embeddings", n_neighbors = 2L, verbose = FALSE)

  expect_true(all(names(reducer) == c("reduced_embeddings", "base_reducer")))

  expect_true(attributes(reducer$reduced_embeddings)[["reduced"]])
  expect_equal(attributes(reducer$reduced_embeddings)[["original_dim"]], c(10, 384))

  })



