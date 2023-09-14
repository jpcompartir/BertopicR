#' Create an embedding model
#'
#' Initially this function is built upon the `sentence_transformers` Python library, but it may be expanded to accept other frameworks. You should feed in your documents as a list. You can use hardware accelerators e.g. GPUs, to speed up computation.
#'
#' @param model_name Name of embedding model as a string (not case sensitive)
#'
#' @return a Python object
#' @export
#'
#' @examples
#' embedder <- bt_make_embedder("all-mpnet-base-v2")
#'
#' embedder <- bt_make_embedder("aLL-minilm-l6-v2")
bt_make_embedder <- function(model_name) {

  #Can leave space for a second argument, which is model_source and then use switch() to allow for embedding models other than sentence_transformers if the need arises.

  if(!is.character(model_name)){
    stop("'model_name' should be a string of text e.g. 'all-mpnet-base-v2")
  }

   #Import sentence transformers to embed documents. In the future we might want to use an argument + switch to allow the user to use other platforms for embeddings.
  sentence_transformers <- reticulate::import("sentence_transformers")

  #Instantiate embedding model, pass ellipsis here
  embedder <- sentence_transformers$SentenceTransformer(model_name_or_path = model_name)

  attr(embedder, "embedding_model") <- model_name

  return(embedder)
}


#' Embed your documents
#'
#' Takes a document, or list of documents, and returns a numerical embedding which can be used as features for machine learning model or for semantic similarity search. If you have pre-computed your embeddings you can skip this step. the bt_embed function is designed to be used as one step in a topic modelling pipeline.
#'
#' Initially this function is built upon the `sentence_transformers` Python library, but it may be expanded to accept other frameworks. You should feed in your documents as a list. You can use hardware accelerators e.g. GPUs, to speed up computation.
#'
#' The function currently returns an object with two additional attributes: embedding_model, n_documents, they have been appended to the embeddings for extraction at later steps in the pipeline, e.g. when merging data frames later on it's important to check how many documents we entered.
#'
#' @param embedder An embedding model (output of bt_make_embedder)
#' @param documents A character vector of the documents to be embedded, e.g. your text variable
#' @param ... Optional or additional parameters passed to SentenceTransformer's encode function, e.g. batch_size
#' @param accelerator A string containing the name of a hardware accelerator, e.g. "mps", "cuda". If NULL no acceleartor is used.
#' @param progress_bar A logical value indicating whether a progress bar is shown in the console
#'
#' @return An array of floating point numbers
#' @export
#' 
#' @examples
#' docs <- c("i am", "a list of", "documents", "to be embedded")
#' 
#' embedder <- bt_make_embedder("aLL-minilm-l6-v2")
#' 
#' embeddings <- bt_do_embedding(embedder, docs)
#' 
bt_do_embedding <- function(embedder, documents, ..., accelerator = "mps", progress_bar = TRUE) {

  #If the embedder isn't a sentence transformers object, stop early.
  if(!grepl("^sentence_tran",class(embedder)[[1]])){
    stop("This package currently only supports embedding models from the sentence transformer library, embedder should be a sentence transformers model")
  }

  #Store the attributes associated with the embedder for adding the embedding_model later
  embedder_attributes <- attributes(embedder)

  #Stop early if conditions aren't met
  stopifnot(is.character(accelerator) | is.null(accelerator),
            is.logical(progress_bar))

  #Create embeddings
  embeddings <-
    embedder$encode(
      documents,
      device = accelerator,
      show_progress_bar = progress_bar,
      ... #This allows knowledgeable users to include other arguments, without bloating the autofill for inexperienced users
    )

  #Give the user a quick console nudge that the embeddings are ready
  message("\nEmbedding proccess finished")

  #Keep track of the number of documents that were fed into the bt_embed function, should be useful later when merging data frames and documents aren't present any more. Should we just return a data frame with the documents, and nested embeddings?
  n_documents <- length(documents)
  attr(embeddings, "n_documents") <- n_documents


  #Add the embedding_model attribute if we can (this will help later on, or when debugging for other users.)
  if("embedding_model" %in% names(embedder_attributes)){
    attr(embeddings, "embedding_model") <- embedder_attributes$embedding_model
    message(paste0(embedder_attributes$embedding_model, " added to embeddings attributes"))
  } else{
    message("No embedding_model attribute found on embedder, proceeding without adding")
  }

  return(embeddings)

}