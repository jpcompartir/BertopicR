#' fit a bertopic model to cleaned data
#'
#' @param cleaned_text cleaned text column that the model should be fit with
#' @param calculated_embeddings if embeddings are already calculated input embeddings as matrix
#' @param min_topic_size minimum topic size 
#' @param nr_topics the number of topics to find within the dataset
#' @param openai_model openai model to use for topic representation
#' @param openai_auth_key openai authentication key
#' @param nr_docs number of docs to send to openai api
#' @param embedding_model which embedding model to use or was used to produce embeddings
#' @param accelerator accelerator to use - default is mps, use NULL if none
#' @param random_state random state to pass to umap
#'
#' @return list containing fitted bertopic model and embeddings used to fit the model
#' @export
#'
#' @usage fit_transform_model(
#' cleaned_text,
#' calculated_embeddings = NULL,
#' min_topic_size = 10,
#' nr_topics = NULL,
#' openai_auth_key = sk-",
#' nr_docs = 5,
#' embedding_model = "all-MiniLM-L6-v2",
#' accelerator = "mps",
#' random_state = NULL)
#' 
bt_fit_transform_openai <- function(cleaned_text,
                                    calculated_embeddings = NULL,
                                    min_topic_size = 10,
                                    nr_topics = NULL,
                                    openai_model = "gpt-3.5-turbo",
                                    openai_auth_key = "sk-",
                                    exponential_backoff = TRUE,
                                    nr_docs = 5,
                                    embedding_model = "all-MiniLM-L6-v2",
                                    accelerator = "mps",
                                    random_state = NULL){
  
  # embedding model
 
  
  # embeddings
  if (!is.null(calculated_embeddings)){
    sentence_model = NULL
    embeddings <- calculated_embeddings
  } else{
    sentence_transformers <- reticulate::import("sentence_transformers")
    sentence_model <- sentence_transformers$SentenceTransformer(embedding_model)
    embeddings <- sentence_model$encode(cleaned_text, device = accelerator)
  }
  
  # create integer for random_state
  if (is.null(random_state)){
    random_state_int <- NULL
  } else{
    random_state_int <- as.integer(random_state)
  }
  
  # create min_topic_size integer
  min_topic_int = as.integer(min_topic_size)
  
  # create nr_topics integer
  if (is.null(nr_topics)){
    nr_topics_int <- NULL
  } else{
    nr_topics_int <- as.integer(nr_topics)
  }
  
  # create nr_docs integer
  nr_docs_int <- as.integer(nr_docs)
  
  # create umap model
  umap <- reticulate::import("umap")
  umap_model <- umap$UMAP(n_neighbors=15L, 
                          n_components=5L, 
                          min_dist=0.0, 
                          metric='cosine', 
                          random_state = random_state_int)
  
  # create representation model
  representation <- reticulate::import("bertopic.representation")
  openai <- reticulate::import("openai")
  openai$api_key <- openai_auth_key
  representation_model <- representation$OpenAI(openai_model, 
                                                chat = TRUE, 
                                                nr_docs = nr_docs_int,
                                                exponential_backoff = exponential_backoff)
 
  # initiate model
  model <- py$bertopic$BERTopic(min_topic_size = min_topic_int,
                                nr_topics = nr_topics_int,
                                umap_model = umap_model,
                                embedding_model = sentence_model,
                                representation_model = representation_model)
  
  output <- model$fit_transform(cleaned_text,
                                embeddings = embeddings)
  
  if (!is.null(calculated_embeddings)){
    return(model) # if embeddings provided, only return the model
  } else{
    return(list(model, embeddings)) # if embeddings not provided, return embeddings and model
  }
  
  
}
