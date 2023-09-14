#' Create umap dimensionality reduction model
#'
#' This function wraps the UMAP functionality from Python's umap-learn package for use in R via reticulate. It allows you to perform dimension reduction on high-dimensional data, its intended use is in a BertopicR pipeline/
#'
#' If you're concerned about processing time, you most likely will only want to reduce the dimensions of your dataset once. In this case, when compiling your model with bt_compile_model you should call `reducer <- bt_base_reducer()`.
#'
#' low_memory = TRUE is currently inadvisable as trial and error suggests the results are not as robust in later clustering.
#'
#' @param ... Sent to umap.UMAP python function for adding additional arguments
#' @param n_neighbours The size of local neighbourhood (in terms of number of neighboring data points) used
#'        for manifold approximation (default: 15).
#' @param n_components The number of dimensions to reduce to (default: 5).
#' @param min_dist The minimum distance between points in the low-dimensional representation (default: 0.0).
#' @param metric The metric to use for distance computation (default: "euclidean").
#' @param random_state The seed used by the random number generator (default: 42).
#' @param low_memory Loogical, use a low memory version of UMAP (default: FALSE)
#' @param verbose Logical flag indicating whether to report progress during the dimension reduction (default: TRUE).
#'
#' @return A UMAP Model that can be input to bt_do_reducing to reduce dimensions of data
#'
#' @export
#' 
#' @seealso
#'  \url{https://umap-learn.readthedocs.io/en/latest/api.html}
#'  \url{https://umap-learn.readthedocs.io/en/latest/basic_usage.html}
#'  \url{https://umap-learn.readthedocs.io/en/latest/parameters.html}
#'  
#' @examples
#' # using euclidean distance measure and specifying numeric inputs as integers
#' reducer <- bt_make_reducer_umap(n_neighbours = 15L, n_components = 10L, metric = "euclidean")
#'
#' # using euclidean distance measure and not specifying numeric inputs as integers (done internally in function)
#' reducer <- bt_make_reducer_umap(n_neighbours = 15, n_components = 10, metric = "euclidean")
#' 
#'  # using cosine distance measure and not specifying numeric inputs as integers (done internally in function)
#' reducer <- bt_make_reducer_umap(n_neighbours = 20, n_components = 6, metric = "cosine")
#' 
bt_make_reducer_umap <- function( ..., n_neighbours = 15L, n_components = 5L, min_dist = 0.0, metric = "euclidean", random_state = 42L, low_memory = FALSE, verbose = TRUE
) {

  #Early stopping and input validation ----
  stopifnot(is.logical(verbose),
            is.numeric(n_neighbours),
            is.numeric(n_components), is.numeric(random_state),
            is.numeric(min_dist),
            is.character(metric))

    #Import umap library for reduction ----
    umap <- reticulate::import("umap")

    #Instantiate a UMAP model with the given parameters
    reducer <- umap$UMAP(n_neighbors = as.integer(n_neighbours),
                         n_components = as.integer(n_components),
                         min_dist = min_dist,
                         metric = metric,
                         random_state = as.integer(random_state),
                         verbose = verbose,
                         low_memory = low_memory,
                         ... #Allows user to give additional arguments to the umap$UMAP function.
    )


  return(reducer)
}

#' Create pca dimensionality reduction model
#'
#' @description  
#' This function wraps the PCA functionality from Python's sklearn package for 
#' use in R via reticulate. It allows you to perform dimension reduction on 
#' high-dimensional data, its intended use is in a BertopicR pipeline. If you're 
#' concerned about processing time, you most likely will only want to reduce the 
#' dimensions of your dataset once. In this case, when compiling your model with 
#' bt_compile_model you should call `reducer <- bt_base_reducer()`.
#'
#' @param ... Sent to sklearn.decomposition.PCA function for adding additional arguments
#' @param n_components Number of components to keep
#' @param svd_solver method for reducing components can be auto, full, arpack, randomized
#'
#' @return A PCA Model that can be input to bt_do_reducing to reduce dimensions of data
#' @export
#' 
#' @seealso
#'  \url{https://scikit-learn.org/stable/modules/generated/sklearn.decomposition.PCA.html}
#'  \url{https://scikit-learn.org/stable/modules/decomposition.html#pca}
#'  
#' @examples
#' # using default svd_solver
#' reducer <- bt_make_reducer_pca(n_components = 100)
#' 
#' # speciying extra pca arguments
#' reducer <- bt_make_reducer_pca (n_components = 20, svd_solver = "full", random_state = 42L)
#' 
bt_make_reducer_pca <- function(..., 
                                n_components,
                                svd_solver = "auto"){
  
  #### input validation ####
  
  #Import the sklearn decomposition library i
  skl <- reticulate::import("sklearn.decomposition")
  
  #Convert the `...` (dot-dot-dot or ellipsis) to list for checking purposes
  dots <- rlang::list2(...)
  
  #Instantiate empty model to get valid arguments
  empty_model <- skl$PCA()
  
  #Stop function early if bad arguments fed with ellipsis and send message to user pointing out which arguments were bad
  if(any(!names(dots) %in% names(empty_model))){
    
    bad_args <- names(dots)[!names(dots) %in% names(empty_model)]
    stop(paste("Bad argument(s) attempted to be sent to PCA():", bad_args, sep = ' '))
  }
  
  # correct UK/US spelling convention
  if (svd_solver == "randomised"){svd_solver = "randomized"}
  
  stopifnot(is.numeric(n_components),
            svd_solver %in% c("auto", "full", "arpack", "randomized"))
  
  #### End of input validation ####
  
  n_components <- as.integer(n_components) # convert to intger
  
  pca_model <- skl$PCA(n_components = n_components,
                       svd_solver = svd_solver,
                       ...)
  
  return(pca_model)
}


#' Created Truncated SVD dimensionality reduction model
#' 
#' @description  
#' This function wraps the Truncated SVD (Single Value Decomposition) functionality
#' from Python's sklearn package for use in R via reticulate. It allows you to 
#' perform dimension reduction on high-dimensional data. 
#' Its intended use is in a BertopicR pipeline. If you're concerned about processing 
#' time, you most likely will only want to reduce the dimensions of your dataset once. 
#' In this case, when compiling your model with bt_compile_model you should call 
#' `reducer <- bt_base_reducer()`.
#'
#' @param ... Sent to sklearn.decomposition Truncated SVD function for adding additional arguments
#' @param n_components Number of components to keep
#' @param n_iter Number of iterations for randomised svd solver. Not used if svd solver is "arpack".
#' @param svd_solver method for reducing components can be arpack or randomized
#'
#' @return Truncated SVD Model that can be input to bt_do_reducing to reduce dimensions of data
#' @export
#' 
#' @seealso
#'  \url{https://scikit-learn.org/stable/modules/generated/sklearn.decomposition.TruncatedSVD.html}
#'  \url{https://scikit-learn.org/stable/modules/decomposition.html#lsa}
#' 
#' @examples
#' reducer <- bt_make_reducer_truncated_svd(n_components = 5)
#' 
bt_make_reducer_truncated_svd <- function(..., 
                                n_components,
                                n_iter = 5,
                                svd_solver = "randomized"){
  
  #### input validation ####
  
  #Import the sklearn decomposition library i
  skl <- reticulate::import("sklearn.decomposition")
  
  #Convert the `...` (dot-dot-dot or ellipsis) to list for checking purposes
  dots <- rlang::list2(...)
  
  #Instantiate empty model to get valid arguments
  empty_model <- skl$TruncatedSVD()
  
  #Stop function early if bad arguments fed with ellipsis and send message to user pointing out which arguments were bad
  if(any(!names(dots) %in% names(empty_model))){
    
    bad_args <- names(dots)[!names(dots) %in% names(empty_model)]
    stop(paste("Bad argument(s) attempted to be sent to TruncatedSVD():", bad_args, sep = ' '))
  }
  
  # correct UK/US spelling convention
  if (svd_solver == "randomised"){svd_solver = "randomized"}
  
  stopifnot(is.numeric(n_components),
            is.numeric(n_iter),
            svd_solver %in% c("arpack", "randomized"))
  
  #### End of input validation ####
  
  n_components <- as.integer(n_components) # convert to integer
  n_iter <- as.integer(n_iter)
  
  truncated_svd_model <- skl$TruncatedSVD(n_components = n_components,
                                n_iter = n_iter,
                       algorithm = svd_solver,
                       ...)

  
  return(truncated_svd_model)
}


#' Perform dimensionality reduction on your embeddings
#'
#' @param reducer Your dimensionality reduction model
#' @param embeddings Your embeddings
#'
#' @return Embeddings with reduced number of dimensions
#' @export
#'
#' @examples 
#' # make reducuction model
#' reducer <- bt_make_reducer_umap(n_neighbours = 2, n_components = 2)
#' 
#' # mock embeddings
#' embeddings <- matrix(runif(50), nrow = 5, ncol = 10)
#' 
#' # reduce the embeddings
#' reduced_embeddings <- bt_do_reducing(reducer, embeddings)
#' 
bt_do_reducing <- function(reducer, embeddings) {

  #Early stopping
  stopifnot(is.array(embeddings) | is.data.frame(embeddings)) #Might be bad # was bad, check for array or data.frame

  #Should we return the model or just the reduced embeddings
  #Try to reduce dimensions, and if unsuccessful we'll check what happened and try again. This follows the _bertopic.py implementation
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
  attr(reduced_embeddings, "reduced") <- dim(reduced_embeddings)[[2]] < dim(embeddings)[[2]]
  attr(reduced_embeddings, "original_dim") <- dim(embeddings)
  
  if ("n_neighbors" %in% names(reducer)) {
    attr(reduced_embeddings, "n_neighbors") <- reducer$n_neighbors
    } else {
      attr(reduced_embeddings, "n_neighbors") <- NA
    }
  
  if ("metric" %in% names(reducer)) {
    attr(reduced_embeddings, "metric") <- reducer$metric
  } else {
    attr(reduced_embeddings, "metric") <- NA
  }
  
  if ("random_state" %in% names(reducer)) {
    attr(reduced_embeddings, "random_state") <- reducer$random_state
  } else {
    attr(reduced_embeddings, "random_state") <- NA
  }
  
  if ("svd_solver" %in% names(reducer)) {
    attr(reduced_embeddings, "svd_solver") <- reducer$svd_solver
  } else {
    attr(reduced_embeddings, "svd_solver") <- NA
  }
  
  if ("n_components" %in% names(reducer)) {
    attr(reduced_embeddings, "n_components") <- reducer$n_components
  } else {
    attr(reduced_embeddings, "n_components") <- NA
  }
  
  #Return the reduced embeddings
  return(reduced_embeddings)
}
