test_that("bt_make_clusterer_hdbscan error on invalid input", {
  expect_error(bt_make_clusterer_hdbscan(min_cluster_size = "test"), "is.numeric.*min_cluster_size")
  expect_error(bt_make_clusterer_hdbscan(min_samples = "test"), "is.numeric.*min_samples")
  expect_error(bt_make_clusterer_hdbscan(metric = 14), "is.character.*metric")
  expect_error(bt_make_clusterer_hdbscan(cluster_selection_method = 14), "cluster_selection_method.*is not TRUE")
  expect_error(bt_make_clusterer_hdbscan(prediction_data = 14), "is.logical.*prediction_data")
  expect_error(bt_make_clusterer_hdbscan(test_arg = "test"), "Bad argument\\(s\\) attempted to be sent to HDBSCAN\\(\\): test_arg")
})

test_that("bt_make_clusterer_hdbscan returns correct output", {
  clusterer <- bt_make_clusterer_hdbscan(min_cluster_size = 5, 
                                         gen_min_span_tree = FALSE, 
                                         min_samples = 3,
                                         metric = "cosine")
  
  expect_equal(clusterer$min_cluster_size, 5)
  expect_equal(clusterer$gen_min_span_tree, FALSE)
  expect_equal(clusterer$min_samples, 3)
  expect_equal(clusterer$metric, "cosine")
})

test_that("bt_make_clusterer_kmeans errors on invalid input", {
  expect_error(bt_make_clusterer_kmeans(n_clusters = "test"), "is.numeric.*n_clusters")
  expect_error(bt_make_clusterer_kmeans(test_arg = "test"), "Bad argument\\(s\\) attempted to be sent to KMeans\\(\\): test_arg")
})

test_that("bt_make_clusterer_kmeans returns correct output", {
  clusterer <- bt_make_clusterer_kmeans(n_clusters = 10L, 
                                        copy_x = FALSE, 
                                        max_iter = 200L)
  
  expect_equal(clusterer$n_clusters, 10)
  expect_equal(clusterer$copy_x, FALSE)
  expect_equal(clusterer$max_iter, 200)
})

test_that("bt_make_clusterer_agglomerative errors on invalid input", {
  expect_error(bt_make_clusterer_agglomerative(n_clusters = "test"), "is.numeric.*n_clusters")
  expect_error(bt_make_clusterer_agglomerative(test_arg = "test"), "Bad argument\\(s\\) attempted to be sent to AgglomerativeClustering\\(\\): test_arg")
})

test_that("bt_make_clusterer_agglomerative returns correct output", {
  clusterer <- bt_make_clusterer_agglomerative(n_clusters = 10L, 
                                        metric = "manhattan")
  
  expect_equal(clusterer$n_clusters, 10)
  expect_equal(clusterer$metric, "manhattan")
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
