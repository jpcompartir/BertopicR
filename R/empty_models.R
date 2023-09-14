#' Create an empty reducer for skipping dimensionality reduction step of bertopic pipeline
#' 
#' @details
#' This refers to the BaseDimensionalityReduction() function in the Python BERTopic library 
#' and can be used to skip over the dimensionality reduction step that occurs as part of
#' fitting the bertopic model to the text data. This is convenient as it allows you to perform
#' dimensionality reduction of document embeddings externally to the model which gives full 
#' transparency in terms of reduced embedding values and can save time if trialing fitting the model 
#' with various parameters.
#' 
#'
#' @return an empty dimensionality reduction model (Python class)
#' @export
#'
#' @examples
#' empty_reducer <- bt_empty_reducer()
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
#' Create an empty embedder for skipping embedding step of bertopic pipeline
#' 
#' @details
#' This refers to the BaseEmbedder() function in the Python BERTopic library 
#' and can be used to skip over the document embedding step that occurs as part of
#' fitting the bertopic model to the text data. This is convenient as it allows you to perform
#' document embedding externally to the model which gives full 
#' transparency in terms of embedding values and can save time if trialing fitting the model 
#' with various parameters.
#'
#' @return an empty embedding model (Python class)
#' @export
#'
#' @examples
#' empty_emebdder <- bt_empty_embedder()
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

#' Create an empty clusterer for skipping clustering step of bertopic pipeline
#'
#' @details
#' This refers to the BaseCluster() function in the Python BERTopic library 
#' and can be used to skip over the clustering step that occurs as part of
#' fitting the bertopic model to the text data. Predicting topics is achieved via
#' clustering and if using this function to skip over the clustering step, pre-determined topics
#' will need to be passed to the bertopic model via the y parameter in the bt_fit_model() function.
#' This can be convenient in cases where you already know the topics to which your documents belong.
#' Creating a bertopic model in this way allows you to utilise the other functionality of a 
#' bertopic model for scenarios where you do not need to employ the topic discovery capabilities of the 
#' bertopic workflow.
#' 
#' @return an empty clustering model (Python class)
#' @export
#'
#' @examples
#' empty_clusterer <- bt_empty_clusterer()
#'
#' clusterer <- bt_empty_clusterer()
bt_empty_clusterer <- function() {

  #Instantiate empty cluster model to skip the step in the pipeline:
  btc <- reticulate::import("bertopic.cluster")

  #return as a function call, so user doesn't have to.
  base_clusterer <- btc$BaseCluster()

  return(base_clusterer)
}
