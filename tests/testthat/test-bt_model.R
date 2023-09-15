test_that("bt_compile_model works with Python objects and doesn't with non-Python objects",{
  expect_error(
    bt_compile_model(embedding_model = "hello"), 
    "test_is_python_object.*is\\.null.*is not TRUE")
  
  expect_error(
    bt_compile_model(reduction_model = "goodbye"),
    "test_is_python_object.*is\\.null.*is not TRUE")
  
  expect_error(
    bt_compile_model(clustering_model = "why strings?"),
    "test_is_python_object.*is\\.null.*is not TRUE")
  
  expect_error(
    bt_compile_model(bad_input = 5),
    "Bad argument.*bad_input")


  model <- bt_compile_model(
    calculate_probabilities = FALSE,
    embedding_model = bt_empty_embedder(),
    reduction_model = bt_empty_reducer(),
    clustering_model = bt_empty_clusterer()
  )

  expect_true(test_is_python_object(model))
})

test_that("bt_compile_model accepts the inputs from the various bt_make_* functions and the parameters are set appropriately", {

  embedding_model <- bt_make_embedder("all-minilm-l6-v2")

  reduction_model <- bt_make_reducer_umap(n_neighbours = 10L, n_components = 4L)
  clustering_model <- bt_make_clusterer_kmeans()
  vectoriser_model <- bt_make_vectoriser(ngram_range = c(1, 3))

model <- bt_compile_model(embedding_model = embedding_model, reduction_model = reduction_model, vectoriser_model = vectoriser_model, clustering_model = clustering_model)

#Check each model has a parameter other than what would be expected if we were using defaults
expect_true(grepl("Transformer model:", model$embedding_model$state_dict))
expect_equal(model$umap_model$n_components, 4)
expect_true(model$vectorizer_model$ngram_range[[2]] == 3)
expect_true(grepl(pattern = "KMean", model$hdbscan_model))
expect_false(grepl("hdbscan", model$hdbscan_model))

model <- bt_compile_model(ctfidf_model = bt_make_ctfidf(reduce_frequent_words = FALSE))

#by a stronger premise the ctfidf_model we want has been added to the model
expect_false(model$ctfidf_model$reduce_frequent_words)
})
