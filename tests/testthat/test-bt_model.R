test_that("bt_compile_model works with Python objects and doesn't with non-Python objects",{
  expect_error(bt_compile_model(embedding_model = "hello", reduction_model = "goodbye", clustering_model = "why strings?"))

  #Doesn't fail with base models:
  expect_silent(bt_compile_model(
    embedding_model = bt_base_embedder(),
    reduction_model = bt_base_reducer(),
    clustering_model = bt_base_clusterer()
  ))

  model <- bt_compile_model(
    embedding_model = bt_base_embedder(),
    reduction_model = bt_base_reducer(),
    clustering_model = bt_base_clusterer()
  )

  #model's representation model is not null
  # expect_true(!is.null(model$representation_model))
})

test_that("bt_compile_model accepts the inputs from the various bt_make_* functions and the parameters are set appropriately", {

  embedding_model <- bt_make_embedder("all-minilm-l6-v2")

  reduction_model <- bt_make_reducer(n_neighbors = 10L, n_components = 4L)
  clustering_model <- bt_make_clusterer(clustering_method = "kmeans")
  vectoriser_model <- bt_make_vectoriser(ngram_range = c(1, 3))

expect_silent(model <- bt_compile_model(embedding_model = embedding_model, reduction_model = reduction_model, vectoriser_model = vectoriser_model, clustering_model = clustering_model))

#Check each model has a parameter other than what would be expected if we were using defaults
expect_true(grepl("Transformer model:", model$embedding_model$state_dict))
expect_equal(model$umap_model$n_components, 4)
expect_true(model$vectorizer_model$ngram_range[[2]] == 3)
expect_true(grepl(pattern = "KMean", model$hdbscan_model))
expect_false(grepl("hdbscan", model$hdbscan_model))

})
