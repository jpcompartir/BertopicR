#' Reduce dimensions of your embeddings
#'
#' This function wraps the UMAP functionality from Python's umap-learn package for use in R via reticulate. It allows users to perform dimension reduction on high-dimensional data, its intended use is in a BertopicR pipeline, you can choose to return reduced embeddings, the reducer (UMAP model) or both.
#'
#' If you're concerned about processing time, you most likely will only want to reduce the dimensions of your dataset once. In this case, you should set `return_value = reduced_embeddings`, feed your reduced_embeddings into either a `BERTopic()` call or the `fit_transform()` function, and instantiate a base dimensionality reduction model (so that the calculation is effectively skipped.)
#'
#' @param embeddings A matrix or data frame to be reduced. Each row is considered a separate data point, and each column is a separate dimension.
#' @param ... Additional parameters to pass to the UMAP function.
#' @param return_value Whether to return the reduced embeddings, the model, or both
#' @param n_neighbors The size of local neighborhood (in terms of number of neighboring data points) used
#'        for manifold approximation (default: 15).
#' @param n_components The number of dimensions to reduce to (default: 5).
#' @param min_dist The minimum distance between points in the low-dimensional representation (default: 0.0).
#' @param metric The metric to use for distance computation (default: "euclidean").
#' @param random_state The seed used by the random number generator (default: 42).
#' @param verbose Logical flag indicating whether to report progress during the dimension reduction (default: TRUE).
#'
#' @return A matrix or data frame of the dimension-reduced data. The number of rows will be the same
#'         as `embeddings`, and the number of columns will be `n_components`.
#'         This object will also have attributes "original_dim", "n_neighbors", "metric", and "random_state"
#'         that store corresponding inputs.
#'
#' @examples
#' \dontrun{
#' bt_reducer(your_data, n_neighbors = 10, n_components = 2, metric = "correlation")
#' }
#'
#' @export
bt_reducer <- function(embeddings, ..., return_value = c("reduced_embeddings", "reducer", "both"), n_neighbors = 15L, n_components = 5L, min_dist = 0.0, metric = "euclidean", random_state = 42L, verbose = TRUE){

  return_value <- match.arg(return_value)

  #Early stopping and input validation ----
  stopifnot(is.logical(verbose),
            is.array(embeddings) | is.data.frame(embeddings), #Might be bad # was bad, check for array or data.frame
            is.numeric(n_neighbors),
            is.numeric(n_components), is.numeric(random_state),
            is.numeric(min_dist),
            is.character(metric))

  #Import umap library for reduction ----
  umap <- reticulate::import("umap")

  #Instantiate a UMAP model with the given parameters
  reducer <- umap$UMAP(n_neighbors = as.integer(n_neighbors),
                       n_components = as.integer(n_components),
                       min_dist = min_dist,
                       metric = metric,
                       random_state = as.integer(random_state),
                       verbose = verbose,
                       ... #Allows user to give additional arguments to the umap$UMAP function.
                       )

  if(return_value == "reducer") {return(reducer)}

  #Should we return the model or just the reduced embeddings
  #Try to reduce dimensions, and if unsuccessful we'll check what happened and ry again. This follows the _bertopic.py implementation
  reduced_embeddings <- try(reducer$fit_transform(embeddings))

  #Try again, this time setting Y = Y (reducer$fit(X =, Y =)), this is to mimic code from he Python implementation
  if(any(class(reduced_embeddings) == "try-error")){
    reduced_embeddings <-
      try(reducer$fit_transform(X = embeddings,Y = Y))
  }

  #If neither call worked, stop the function with an error message
  if(any(class(reduced_embeddings) == "try-error")){
    stop("Error in UMAP call, are your inputs correctly formatted?")
    }


  #Add additional attributes which may be helpful to track later on ----
  attr(reduced_embeddings, "reduced") <- TRUE
  attr(reduced_embeddings, "original_dim") <- dim(embeddings)
  attr(reduced_embeddings, "n_neighbors") <- n_neighbors
  attr(reduced_embeddings, "metric") <- metric
  attr(reduced_embeddings, "random_state") <- random_state

  #Return either the reduced embeddings or both the embeddings and the model (if the return_value = reducer, the function will have already finished running.) ----
  return_val <- switch(return_value,
                       "reduced_embeddings" = reduced_embeddings,
                       "both" = list(
                         "reduced_embeddings" = reduced_embeddings,
                         "reducer" = reducer
                         ))

return(return_val)

}
