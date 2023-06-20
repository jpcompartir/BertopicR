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

