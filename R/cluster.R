#' Create a kmeans clustering model
#'
#' @param ... Additional arguments sent to sklearn.cluster.KMeans()
#' @param n_clusters number of clusters to search for (enter as integer by typing L after the number)
#'
#' @return A kMeans clustering model (Python object)
#' @export
#'
#' @examples
#' clustering_model <- bt_make_clusterer_kmeans(n_clusters = 15L)
#'
#' kmeans_model <- bt_make_clusterer_kmeans(n_clusters = 10L)
bt_make_clusterer_kmeans <- function(..., n_clusters = 10L) {
  
  # input validation ----
  
  stopifnot(is.numeric(n_clusters))
  
  #Import library to access kmeans inside function scope
  skcluster <- reticulate::import("sklearn.cluster")
  
  #Convert the `...` (dot-dot-dot or ellipsis) to list for checking purposes
  dots <- rlang::list2(...)
  
  #Instantiate empty model to get valid arguments
  empty_model <- skcluster$KMeans()
  
  #Stop function early if bad arguments fed with ellipsis and send message to user pointing out which arguments were bad
  if(any(!names(dots) %in% names(empty_model))){
    
    bad_args <- names(dots)[!names(dots) %in% names(empty_model)]
    stop(paste("Bad argument(s) attempted to be sent to KMeans():", bad_args, sep = ' '))
  }
  
  # end of input validation ----

  #Instantiate clustering model making sure n_clusters is an integer
  clustering_model <- skcluster$KMeans(n_clusters = as.integer(n_clusters),
                                       ...)

  return(clustering_model)
}

#' Create an Agglomerative Clustering clustering model
#'
#' @param ... Additional arguments sent to sklearn.cluster.AgglomerativeClustering()
#' @param n_clusters number of clusters to search for (enter as integer by typing L after the number)
#'
#' @return An Agglomerative Clustering clustering model (Python object)
#' @export
#'
#' @examples
#' clustering_model <- bt_make_clusterer_agglomerative( n_clusters = 15L)
#'
#' agglomerative_model <- bt_make_clusterer_agglomerative(n_clusters = 10L)
bt_make_clusterer_agglomerative <- function(..., n_clusters = 20L) {
  
  # input validation ----
  
  stopifnot(is.numeric(n_clusters))
  
  #Import library to access kmeans inside function scope
  skcluster <- reticulate::import("sklearn.cluster")
  
  #Convert the `...` (dot-dot-dot or ellipsis) to list for checking purposes
  dots <- rlang::list2(...)
  
  #Instantiate empty model to get valid arguments
  empty_model <- skcluster$AgglomerativeClustering()
  
  #Stop function early if bad arguments fed with ellipsis and send message to user pointing out which arguments were bad
  if(any(!names(dots) %in% names(empty_model))){
    
    bad_args <- names(dots)[!names(dots) %in% names(empty_model)]
    stop(paste("Bad argument(s) attempted to be sent to AgglomerativeClustering():", bad_args, sep = ' '))
  }
  
  # end of input validation ----

  #Instantiate clustering model making sure n_clusters is an integer
  clustering_model <- skcluster$AgglomerativeClustering(n_clusters = as.integer(n_clusters),
                                                        ...)

  return(clustering_model)
}

#' Create an HDBSCAN clustering model
#'
#' Instantiates an HDBSCAN clustering model using the hdbscan Python library.
#'
#' @param ... Additional arguments sent to hdbscan.HDBSCAN()
#' @param min_cluster_size Minimum number of data points for each cluster, enter as integer by adding L to number
#' @param min_samples Controls the number of outliers generated, lower value = fewer outliers. 
#' @param metric Distance metric to calculate clusters with
#' @param cluster_selection_method The method used to select clusters. Default is "eom".
#' @param prediction_data Set to TRUE if you intend on using model with any functions from hdbscan.prediction eg. if using bt_outliers_probabilities
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
#' # using minkowski metric for calculating distance between documents - when using minkowski metric, a value for p must be specified as an additional argument
#' clustering_model <- bt_make_clusterer_hdbscan(metric = "minkowski", p = 1.5)
#' 
#' # specify integer numeric inputs as integer, using additional gen_min_span_tree argument
#' clusterer = bt_make_clusterer_hdbscan(min_cluster_size = 5L, gen_min_span_tree = TRUE)
#' 
#' # not specifying numeric inputs as integers (converted to integers internally)
#' clusterer = bt_make_clusterer_hdbscan(min_cluster_size = 5, cluster_selection_method = "leaf")
#' 
bt_make_clusterer_hdbscan <- function(..., min_cluster_size = 10L, min_samples = 10L, metric = "euclidean", cluster_selection_method = c("eom", "leaf"), prediction_data = FALSE) {

  # input validation ----
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
  
  stopifnot(is.numeric(min_cluster_size),
            is.numeric(min_samples),
            is.character(metric),
            cluster_selection_method %in% c("eom", "leaf"),
            is.logical(prediction_data))

  # end of input validation ----
  
  cluster_selection_method <- match.arg(cluster_selection_method)
  
  #Instantiate a model, with some named arguments and ellipsis for users who wish to change other arguments
  clustering_model <- hdbscan$HDBSCAN(
    min_cluster_size = as.integer(min_cluster_size),
    metric = metric,
    cluster_selection_method = cluster_selection_method,
    prediction_data = prediction_data,
    min_samples = as.integer(min_samples),
    ...)

  return(clustering_model)
}


#' Cluster your data
#'
#' @param clustering_model Python object, output of bt_make_clusterer*
#' @param embeddings Embeddings, output of bt_do_embedding or bt_do_reducing
#'
#' @return Cluster labels for each document
#' @export
#' 
#' @examples
#' # create clustering model
#' clusterer <- bt_make_clusterer_kmeans(n_clusters = 2)
#' 
#' # mock embeddings
#' embeddings_test <-  matrix(runif(50), nrow = 25, ncol = 2)
#' 
#' # create clusters
#' clusters <- bt_do_clustering(clusterer, embeddings_test)
#' 
bt_do_clustering <- function(clustering_model, embeddings) {

  # Early stopping
  stopifnot(is.array(embeddings) | is.data.frame(embeddings))
  
  fitted_model <- clustering_model$fit(embeddings)
  
  labels <- fitted_model$labels_ #Should we add attributes? Would need to be model specific, so have to extract the model type with class() and then use switch or separate functions or adding attributes for each model?
  
  sort_index <- order(-table(labels[labels != -1])) # want to label groups in order of size - get correct index
  
  label_mapping <- stats::setNames(-1:(length(sort_index) - 1), c(-1, names(table(labels[labels != -1]))[sort_index])) # map previous labels to ordered labels
  
  ordered_labels <- label_mapping[as.character(labels)] # reassign labels

  return(ordered_labels) 
}
