#Testing this is actually pretty sophisticated, so first cover the named arguments, then ellipsis separately, then an output.

test_that("bt_make_reducer stops when it's given bad arguments",{

  expect_error(bt_make_reducer(reducer_type = "beans"), "should be one of")

  expect_silent(bt_make_reducer(reducer_type = "base"))

  expect_error(bt_make_reducer(reducer = "parametric", n_neighbors = "15"), "is.numeric.*n_neighb")

  expect_error(bt_make_reducer(reducer = "parametric", n_components = TRUE), "is.numeric.*n_components")
  expect_error(bt_make_reducer(reducer = "parametric", min_dist = "12"))
  expect_error(bt_make_reducer(reducer = "parametric", random_state = "42"), "is.numeric.*random_state")
  expect_error(bt_make_reducer(reducer = "parametric", metric = 2), "is.character.*metric" )
})

test_that("bt_make_reducer's arguments are working as expected", {
  digits <- runif(n = 368*20, min = 0, max = 1)

  #Make an array and a data frame for this test
  my_array <- array(digits, dim = c(20, 368))
  my_df <- as.data.frame(my_array)

  reducer <- bt_make_reducer(n_neighbors = 5L, n_components = 2L, min_dist = 0.001, metric = "cosine", random_state = 32L, verbose = FALSE)

  expect_equal(reducer$min_dist, 0.001)
  expect_equal(reducer$verbose, FALSE)
  expect_equal(reducer$metric, "cosine")
  expect_equal(reducer$n_neighbors, 5L)
  expect_equal(reducer$n_components, 2L)
  expect_equal(reducer$random_state, 32L)

})

test_that("bt_make_reducer's base argument is working as intended", {

  reducer <- bt_make_reducer(reducer_type = "base")
  #Now check our base model has fit and transform
  expect_true(all(names(reducer) == c("fit", "transform")))

  x <- reducer$fit("xyz")
  expect_true(grepl("^bertopic", class(x)[[1]]))

  #Check it doeosn't just match anything
  expect_false("xyz" == reducer$transform(X = "XYZ"))
  #Check it matches the input
  expect_equal("xyz", reducer$transform(X = "xyz"))
})

test_that("bt_do_reducing accepts an array or a data frame as embeddings", {
  set.seed(12)
  digits <- runif(n = 368*20, min = 0, max = 1)

  #Make an array and a data frame for this test
  my_array <- array(digits, dim = c(20, 368))
  my_df <- as.data.frame(my_array)

  reducer <- bt_make_reducer("parametric", verbose = FALSE)

  expect_silent(bt_do_reducing(reducer, my_array))
  expect_silent(bt_do_reducing(reducer, my_df))
})

#Integration test with bt_embed
test_that("bt_embed and bt_reducer work together",{
  documents <- bert_example_data %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(message)) %>%
    head(10) %>%
    dplyr::pull(message)

  embedder <- bt_make_embedder("all-minilm-l6-v2")
  embeddings <- bt_do_embedding(embedder = embedder, documents = documents)
  reducer <- bt_make_reducer(reducer_type = "parametric", n_neighbors = 2L, verbose = FALSE)
  reduced <- bt_do_reducing(reducer = reducer, embeddings = embeddings )

  expect_true(attributes(reduced)[["reduced"]])
  expect_equal(attributes(reduced)[["original_dim"]], c(10, 384))

})



