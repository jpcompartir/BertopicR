test_that("bt_make_clusterer returns the right type of clustering model", {

  #match.arg is working:
  expect_error(bt_make_clusterer(clustering_method = "beans"))

  #No error raised with correct inputs
  expect_silent(bt_make_clusterer(clustering_method = "hdbscan"))

  #hdbscan model has appropriate class
  hdbscan <- bt_make_clusterer(clustering_method = "hdbscan")
  expect_true(any(grepl("hdbscan", class(hdbscan))))

  #Base model has appropriate class and doesn't have inappropriate
  base <- bt_make_clusterer(clustering_method = "base")
  expect_true(any(grepl("BaseCluster", class(base))))
  expect_false(any(grepl("hdbscan", class(base))))

  #kMeans model has appropriate class
  kmeans <- bt_make_clusterer(clustering_method = "kmeans")
  expect_true(any(grepl("kmeans", class(kmeans))))

  #Agglomerative model has appropriate class
  agglomerative <- bt_make_clusterer(clustering_method = "agglomerative", n_clusters = 10L)
  expect_true(any(grepl("Agglomerative", class(agglomerative))))
})

test_that("bt_make_clusterer and bt_do_clustering are working properly", {

  random_embeddings <- array(runif(500), dim = c(50, 10))
  py_kmeans <- bt_make_clusterer_kmeans(n_clusters = 5L)
  hdbscan <- bt_make_clusterer_hdbscan(min_samples = 2L, cluster_selection_method = "leaf", min_cluster_size = 3L)

  kmeans_clusters <- bt_do_clustering(py_kmeans, random_embeddings)
  expect_true(is.numeric(kmeans_clusters))
  expect_true(length(kmeans_clusters) == 50)

  hdbscan_clusters <- bt_do_clustering(hdbscan, random_embeddings)
  expect_true(length(hdbscan_clusters) == 50)
  expect_true(is.numeric(hdbscan_clusters))
})

test_that("bt_make_clusterer_hdbscan changes inputs appropriately", {
  hdbscan_default <- bt_make_clusterer_hdbscan()
  hdbscan_leaf <- bt_make_clusterer_hdbscan(cluster_selection_method = "leaf", min_cluster_size = 5L, min_samples = 3L)
  expect_false(hdbscan_default$min_cluster_size == hdbscan_leaf$min_cluster_size)
  expect_false(hdbscan_default$cluster_selection_method == hdbscan_leaf$cluster_selection_method)
  expect_true(hdbscan_leaf$cluster_selection_method == "leaf")
  expect_true(hdbscan_leaf$min_samples == 3L)
})

