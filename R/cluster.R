bt_make_clusterer <- function(..., clustering_method = c("hdbscan", "kmeans", "agglomerative", "base")) {

  #Match the clustering_method argument & set 'hdbscan' as defualt (first position)
  clustering_method <- match.arg(clustering_method)

  ellipsis::check_dots_used()
  clustering_model <- switch(clustering_method,
                             "kmeans" = bt_make_clusterer_kmeans(...),
                             "hdbscan" = bt_make_clusterer_hdbscan(...),
                             "base" = bt_base_clusterer())

  return(clustering_model)
}

bt_make_clusterer_kmeans <- function(n_clusters = 10L) {

  #Import library to access kmeans inside function scope
  skcluster <- reticulate::import("sklearn.cluster")

  #Instantiate clustering model making sure n_clusters is an integer
  clustering_model <- skcluster$KMeans(n_clusters = as.integer(n_clusters))

  return(clustering_model)
}

bt_make_clusterer_hdbscan <- function(..., min_cluster_size = 10L, metric = "euclidean", cluster_selection_method = "eom", prediction_data = NULL) {

  #link2docs https://hdbscan.readthedocs.io/en/latest/
  #How it works https://hdbscan.readthedocs.io/en/latest/how_hdbscan_works.html

  #Import the hdbscan library inside function scope
  hdbscan <- reticulate::import("hdbscan")

  #Convert the dots to list for checking purposes
  dots <- rlang::list2(...)

  #Instantiate empty model to get valid arguments
  empty_model <- hdbscan$HDBSCAN()

  if(any(!names(dots) %in% names(empty_model))){

    bad_args <- names(dots)[!names(dots) %in% names(empty_model)]
    stop(paste("Bad argument(s) attempted to be sent to HDBSCAN():", bad_args, sep = ' '))
  }

  #Instantiate a model, with some named arguments and ellipsis for users who wish to change other arguments
  clustering_model <- hdbscan$HDBSCAN(
    min_cluster_size = as.integer(min_cluster_size),
    metric = metric,
    cluster_selection_method = cluster_selection_method,
    prediction_data = prediction_data,
    ...)

  return(clustering_model)
}
