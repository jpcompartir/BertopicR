bt_embed <- function(documents, ..., embedding_model = "all-MiniLM-L6-v2", accelerator = "mps", progress_bar = TRUE) {

  stopifnot(is.character(embedding_model),
            is.character(accelerator) | is.null(accelerator),
            is.logical(progress_bar))

  #Import sentence transformers to embed documents
  sentence_transformers <- reticulate::import("sentence_transformers")

  #Instantiate embedding model
  st_model <-
    sentence_transformers$SentenceTransformer(model_name_or_path = embedding_model)

  embeddings <-
    st_model$encode(
      documents,
      device = accelerator,
      show_progress_bar = progress_bar,
      ... #This allows knowledgeable users to include other arguments, without bloating the autofill for inexperienced users
      )

  message("\nEmbedding proccess finished")

  #Leave a trace on the embeddings to extract which model they were from. Not 100% sure this is the right way to go, but seems like it should work. Can grab it later with attributes(embeddings)[["embeedding_model"]]
  attr(embeddings, "embedding_model") <- embedding_model

  return(embeddings)
}
