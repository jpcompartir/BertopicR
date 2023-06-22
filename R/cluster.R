#' Create a clustering model
#'
#' Instantiates a clustering model which can be fed into a BertopicR pipeline.
#'
#' Available clustering models are 'hdbscan', 'kmeans', 'agglomerative', and 'base'. For each of these options, see either the hdbscan, or sklearn official documentation for lists of arguments which can be fed to each clustering model. or type "?bt_make_clusterer_hdbscan", "?bt_make_clusterer_kmeans" etc. to see which named arguments have been supplied as a default for each clustering model.
#'
#'Executing "bt_make_clusterer(clustering_method = 'base')" is the equivalent of calling `"bt_base_clusterer()" and returns an empty model which is used for streamlining the topic modelling process.
#'
#' @param ... Arguments fed to the the clustering function determined by `clustering_method =`
#' @param clustering_method A string defining which clustering method to use
#'
#' @return A clustering model (Python object)
#' @export
#'
#' @examples
#' empty_clustering_model <- bt_make_clusterer(clustering_method = "base")
#'
#' hdbscan_model <- bt_make_clusterer(clustering_method = "hdbscan", min_cluster_size = 10L)
#'
#' kmeans_model <- bt_make_clusterer(clustering_method = "kmeans", n_clusters = 10L)
bt_make_clusterer <- function(..., clustering_method = c("hdbscan", "kmeans", "agglomerative", "base")) {

  #Match the clustering_method argument & set 'hdbscan' as defualt (first position)
  clustering_method <- match.arg(clustering_method)

  ellipsis::check_dots_used()
  clustering_model <- switch(clustering_method,
                             "kmeans" = bt_make_clusterer_kmeans(...),
                             "hdbscan" = bt_make_clusterer_hdbscan(...),
                             "agglomerative" = bt_make_clusterer_agglomerative(...),
                             "base" = bt_base_clusterer())

  return(clustering_model)
}

#' Create a kmeans clustering model
#'
#' @param n_clusters number of clusters to search for (enter as integer by typing L after the number)
#'
#' @return A kMeans clustering model (Python object)
#' @export
#'
#' @examples
#' clustering_model <- bt_make_clusterer_kmeans(15L)
#'
#' kmeans_model <- bt_make_clusterer_kmeans(n_clusters = 10L)
bt_make_clusterer_kmeans <- function(n_clusters = 10L) {

  #Import library to access kmeans inside function scope
  skcluster <- reticulate::import("sklearn.cluster")

  #Instantiate clustering model making sure n_clusters is an integer
  clustering_model <- skcluster$KMeans(n_clusters = as.integer(n_clusters))

  return(clustering_model)
}

#' Create an Agglomerative Clustering clustering model
#'
#' @param n_clusters number of clusters to search for (enter as integer by typing L after the number)
#'
#' @return An Agglomerative Clustering clustering model (Python object)
#' @export
#'
#' @examples
#' clustering_model <- bt_make_clusterer_agglomerative(15L)
#'
#' agglomerative_model <- bt_make_clusterer_agglomerative(n_clusters = 10L)
bt_make_clusterer_agglomerative <- function(n_clusters = 20L) {

  #Import library to access AgglomerativeClustering inside function scope
  skcluster <- reticulate::import("sklearn.cluster")

  #Instantiate clustering model making sure n_clusters is an integer
  clustering_model <- skcluster$AgglomerativeClustering(n_clusters = as.integer(n_clusters))

  return(clustering_model)
}

#' Create an HDBSCAN clustering model
#'
#' Instantiates an HDBSCAN clustering model using the hdbscan Python library.
#'
#' @param ... Additional arguments sent to hdbscan.HDBSCAN()
#' @param min_cluster_size Minimum number of data points for each cluster, enter as integer by adding L to number
#' @param metric Distance metric to calculate clusters with
#' @param cluster_selection_method The method used to select clusters. Default is "eom".
#' @param prediction_data Logical
#'
#' @return An instance of the HDBSCAN clustering model (Python object.

#'
#' @seealso
#'  \url{https://hdbscan.readthedocs.io/en/latest/}
#'  \url{https://hdbscan.readthedocs.io/en/latest/how_hdbscan_works.html}
#'  \url{https://hdbscan.readthedocs.io/en/latest/basic_hdbscan.html#what-about-different-metrics}
#'
#' @export
#'
#' @examples
#' clustering_model <- bt_make_clusterer_hdbscan(metric = "minkowski")
bt_make_clusterer_hdbscan <- function(..., min_cluster_size = 10L, metric = "euclidean", cluster_selection_method = "eom", prediction_data = FALSE) {

  #Import the hdbscan library inside function scope
  hdbscan <- reticulate::import("hdbscan")

  #Convert the `...` (dot-dot-dot or ellipsis) to list for checking purposes
  dots <- rlang::list2(...)

  #Instantiate empty model to get valid arguments
  empty_model <- hdbscan$HDBSCAN()

  #Stop function early if bad arguments fed with ellipsis and send message to user pointing out which arguments were bad
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


bt_do_clustering <- function(clustering_model, embeddings) {

  # Early stopping
  stopifnot(is.array(embeddings) | is.data.frame(embeddings))

  #use fit method as agglomerative doesn't have all the same methods(?) check this
  fitted_model <- clustering_model$fit(embeddings)

  labels <- fitted_model$labels_

  return(labels)

}
