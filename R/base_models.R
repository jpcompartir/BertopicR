#' Create a base reducer for skipping dimensionality reduction step of bertopic pipeline
#'
#' @return an empty dimensionality reduction model (Python class)
#' @export
#'
#' @examples
#' base_reducer <- bt_empty_reducer()
#'
#' reducer <- bt_empty_reducer()
bt_empty_reducer <- function() {

  #Import appropriate library
  btd <- reticulate::import("bertopic.dimensionality")

  #Instantiate empty dim reduction model to skip the step in the pipeline:
  base_reducer  <- btd$BaseDimensionalityReduction()

  #return empty model (already called as a function)
  return(base_reducer)

}
#' Create a base embedder for skipping embedding step of bertopic pipeline
#'
#' @return an empty embedding model (Python class)
#' @export
#'
#' @examples
#' base_emebdder <- bt_empty_embedder()
#'
#' embedder <- bt_empty_embedder()
bt_empty_embedder <- function()  {

  #Import appropriate library
  bte <- reticulate::import("bertopic.backend")

  #Instantiate an empty embedder
  base_embedder <- bte$BaseEmbedder()

  #Return empty embedder (already called as a function)
  return(base_embedder)

}

#' Create a base clusterer for skipping clustering step of bertopic pipeline
#'
#' @return an empty clustering model (Python class)
#' @export
#'
#' @examples
#' base_clusterer <- bt_empty_clusterer()
#'
#' clusterer <- bt_empty_clusterer()
bt_empty_clusterer <- function() {

  #Instantiate empty cluster model to skip the step in the pipeline:
  btc <- reticulate::import("bertopic.cluster")

  #return as a function call, so user doesn't have to.
  base_clusterer <- btc$BaseCluster()

  return(base_clusterer)
}

# misread the docs, this isn't a base model so delete
#'
#' #' Create a base c-tf-idf model for skipping vectorisation step of bertopic pipeline
# #'
# #' @return an empty c-tf-idf model (Python class)
# #' @export
# #'
# #' @examples
# #' base_ctf <- bt_empty_ctfidf()
# #'
# #' base_ctfidf <- bt_empty_ctfidf()
# #'
# #' ctfidfer <- bt_empty_ctfidf()
# bt_empty_ctfidf <- function() {
#
#   #Import appropriate library
#   btv <- reticulate::import("bertopic.vectorizers")
#
#   #Instantiate an empty c-tf-idf vectoriser
#   base_vectoriser <- btv$ClassTfidfTransformer()
#
#   return(base_vectoriser)
#
# }
#
#


