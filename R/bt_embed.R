#' Embed your documents
#'
#' Takes a document, or list of documents, and returns a numerical embedding which can be used as features for machine learning model or for semantic similarity search. If you have pre-computed your embeddings you can skip this step. the bt_embed function is designed to be used as one step in a topic modelling pipeline.
#'
#' Initially this function is built upon the `sentence_transformers` Python library, but it may be expanded to accept other frameworks. You should feed in your documents as a list. You can use hardware accelerators e.g. GPUs, to speed up computation.
#'
#' The function currently returns an object with two additional attributes: embedding_model, n_documents, they have been appended to the embeddings for extraction at later steps in the pipeline, e.g. when merging data frames later on it's important to check how many documents we entered.
#'
#'
#' @param documents A character vector of the documents to be embedded, e.g. your text variable
#' @param ... Optional or additional parameters passed to SentenceTransformer's encode function, e.g. batch_size
#' @param embedding_model A string containing the name of the SentenceTransformers embedding model
#' @param accelerator A string containing the name of a hardware accelerator, e.g. "mps", "cuda". If NULL no acceleartor is used.
#' @param progress_bar A logical value indicating whether a progress bar is shown in the console
#'
#' @return An array of floating point numbers
#' @export
#'
bt_embed <- function(documents, ..., embedding_model = "all-MiniLM-L6-v2", accelerator = "mps", progress_bar = TRUE) {

  #Input validation, stop early if the conditions aren't met, could add some `...` stuff here with the dots 2 list pattern
  stopifnot(is.character(embedding_model),
            is.character(accelerator) | is.null(accelerator),
            is.logical(progress_bar))

  #Import sentence transformers to embed documents. In the future we might want to use an argument + switch to allow the user to use other platforms for embeddings.
  sentence_transformers <- reticulate::import("sentence_transformers")

  #Instantiate embedding model, pass ellipsis here
  st_model <-
    sentence_transformers$SentenceTransformer(model_name_or_path = embedding_model)

  embeddings <-
    st_model$encode(
      documents,
      device = accelerator,
      show_progress_bar = progress_bar,
      ... #This allows knowledgeable users to include other arguments, without bloating the autofill for inexperienced users
      )

  #Give the user a quick console nudge that the embeddings are ready
  message("\nEmbedding proccess finished")

  #Leave a trace on the embeddings to extract which model they were from. Not 100% sure this is the right way to go, but seems like it should work. Can grab it later with attributes(embeddings)[["embeedding_model"]]
  attr(embeddings, "embedding_model") <- embedding_model

  #Keep track of the number of documents that were fed into the bt_embed function, should be useful later when merging data frames and documents aren't present any more. Should we just return a data frame with the documents, and nested embeddings?
  n_documents <- length(documents)
  attr(embeddings, "n_documents") <- n_documents

  return(embeddings)
}
