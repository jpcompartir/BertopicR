#From https://maartengr.github.io/BERTopic/getting_started/tips_and_tricks/tips_and_tricks.html#diversify-topic-representation
#"Since MMR is using word embeddings to diversify the topic representations, it is necessary to pass the embedding model to BERTopic if you are using pre-computed embeddings:"

#' Build a BERTopic model
#'
#' Keep `*_model` = NULL to proceed with a model made from default parameters (see each individual `make_*` function for parameters). However, it is not advisable to accept default parameters for each model; you should tune each model according to your dataset and the business question you are answering.
#'
#' @param embedding_model Model for creating embeddings (Python object)
#' @param reduction_model Model for reducing embeddings' dimensions (Python object)
#' @param clustering_model Model for clustering (Python object)
#' @param vectoriser_model Model for vectorising input for topic representations (Python object)
#' @param ctfidf_model Model for performing class-based tf-idf (ctf-idf) (Python object)
#'
#' @return a BERTopic model
#' @export
#'
bt_compile_model <- function(embedding_model = NULL, reduction_model = NULL, clustering_model = NULL, vectoriser_model = NULL, ctfidf_model = NULL){

  #Check if the inputted arguments are python objects or NULL (in which case we'll make some). This doesn't check they're the right python objects though, will maybe include when full implementation is done and we know all of the embedding models, reduction models etc. and their classes to test with
    stopifnot(test_is_python_object(embedding_model) | is.null(embedding_model),
              test_is_python_object(reduction_model) | is.null(reduction_model),
              test_is_python_object(clustering_model) | is.null(clustering_model),
              test_is_python_object(vectoriser_model) | is.null(vectoriser_model),
              test_is_python_object(ctfidf_model) | is.null(ctfidf_model))

    #Import bertopic inside function scope
    bertopic <- reticulate::import("bertopic")

  #Provide a default embedding model for: Since MMR is using word embeddings to diversify the topic representations, it is necessary to pass the embedding model to BERTopic if you are using pre-computed embeddings:"
  if(is.null(embedding_model)){
    embedding_model <- bt_make_embedder(model_name = "all-mpnet-base-v2")
    message("\nNo embedding model provided, defaulting to 'all-mpnet-base-v2' model as embedder.")
    }

  #If no UMAP model given, provide empty
  if(is.null(reduction_model)){
    reduction_model <- bt_make_reducer()
    message("\nNo umap_model provided, using default parameters.")
  }

  if(is.null(clustering_model)){
    clustering_model <- bt_make_clusterer_hdbscan()
    message("\nNo clustering model provided, using hdbscan with default parameters.")
  }

  if(is.null(vectoriser_model)){
    vectoriser_model <- bt_make_vectoriser()
    message("\nNo vectorising model provided, creating model with default parameters")
  }

  if(is.null(ctfidf_model)){
    ctfidf_model <- bt_make_ctfidf()
    message("\nNo ctfidf model provided, creating model with default parameters")
  }

    #Instantiate a BERTopic object with given models
  model <- bertopic$BERTopic(
    embedding_model = embedding_model,
    umap_model = reduction_model,
    hdbscan_model = clustering_model,
    vectorizer_model = vectoriser_model,
    ctfidf_model = ctfidf_model)

  message("\nModel built")

  return(model)
}

#' Fit a topic model on your documents & embeddings
#'
#' If you've already performed dimensionality reduction on your embeddings, you can feed in the reduced dimension embeddings to the embeddings argument, make sure to supply `bt_compile_model` with a base reducer (the output of `bt_base_reducer()`)
#'
#' @param model Output of bt_compile_model() or another bertopic topic model
#' @param documents Your documents to topic model on
#' @param embeddings Your embeddings, can be reduced dimensionality or not
#' @param topic_labels Pre-existing labels, for supervised topic modelling
#'
#' @return a fitted BERTopic model
#' @export
#'
bt_fit_model <- function(model, documents, embeddings, topic_labels = NULL){

  stopifnot(
    grepl("^bertopic", methods::S3Class(model)[[1]]),
    is.array(embeddings)| is.data.frame(embeddings) |is.null(embeddings),
    is.character(documents)
  )

  if(!is.null(embeddings)){
    #Check the length of documents is equal to the number of embeddings, and if not, stop.
    if(!length(documents) == dim(embeddings)[[1]]) {
      stop(
        paste0("The dimensions of your documents and embeddings do not mach up.\nNumber of documents: ", length(documents),"\nNumber of embeddings: ",dim(embeddings)[[1]]))
    }
  }

  if(!is.null(topic_labels)){
    #Check the length of documents is equal to the number of embeddings, and if not, stop.
    if(!length(documents) == length(topic_labels)) {
      stop(
        paste0("The dimensions of your documents and topic_labels do not mach up.\nNumber of documents: ", length(documents),"\nNumber of topic labels: ",length(topic_labels)))
    }
  }

  fitted_model <- model$fit(documents = documents, embeddings = embeddings, y = topic_labels)

  return(fitted_model)
}

