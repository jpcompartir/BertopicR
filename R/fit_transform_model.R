#' fit a bertopic model to cleaned data
#'
#' @param cleaned_text cleaned text column that the model should be fit with
#' @param calculated_embeddings if embeddings are already calculated input embeddings as matrix
#' @param min_topic_size minimum topic size 
#' @param ngram_range ngram range for topic representation - input must be of type 
#' tuple: use the reticulate function reticulate::tuple()
#' @param nr_topics the number of topics to find within the dataset
#' @param embedding_model which embedding model to use or was used to produce embeddings
#' @param accelerator accelerator to use - default is mps, use NULL if none
#' @param diversity diversity of topic representation (1 = diverse, 0 = not diverse)
#' @param stopwords whether or not to remove stopwords in topic representations
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
#' ngram_range = tuple(1,1),
#' embedding_model = "all-MiniLM-L6-v2",
#' accelerator = "mps",
#' diversity = 0.1,
#' stopwords = TRUE,
#' random_state = NULL)
#' 
fit_transform_model <- function(cleaned_text,
                                calculated_embeddings = NULL,
                                min_topic_size = 10,
                                nr_topics = NULL,
                                ngram_range = c(1, 1),
                                embedding_model = "all-MiniLM-L6-v2",
                                accelerator = "mps",
                                diversity = 0.1,
                                stopwords = TRUE,
                                random_state = NULL){
  
  # embedding model
  sentence_transformers <- reticulate::import("sentence_transformers")
  sentence_model <- sentence_transformers$SentenceTransformer(embedding_model)
  
  # embeddings
  if (!is.null(calculated_embeddings)){
    embeddings <- calculated_embeddings
  } else{
    embeddings <- sentence_model$encode(cleaned_text, device = accelerator)
  }

  # create tuple for ngram_range
  ngram_tuple <- reticulate::tuple(as.integer(ngram_range[1]), as.integer(ngram_range[2]))
  
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
  
  # create umap model
  umap <- reticulate::import("umap")
  umap_model <- umap$UMAP(n_neighbors=15L, 
                          n_components=5L, 
                          min_dist=0.0, 
                          metric='cosine', 
                          random_state = random_state_int)
  # create representation model
  representation <- reticulate::import("bertopic.representation")
  representation_model <- representation$MaximalMarginalRelevance(diversity = diversity)
  
  # create vectoriser model
  if (stopwords){
    stopword = "english"
  } else {
    stopword = NULL
  }
  
  vectorizer <- reticulate::import("sklearn.feature_extraction.text")
  vectorizer_model <- vectorizer$CountVectorizer(ngram_range = ngram_tuple,
                                                  stop_words = stopword)
  
  # initiate model
  model <- py$bertopic$BERTopic(min_topic_size = min_topic_int,
                                nr_topics = nr_topics_int,
                                umap_model = umap_model,
                                embedding_model = sentence_model,
                                representation_model = representation_model,
                                vectorizer_model = vectorizer_model)
  
  output <- model$fit_transform(cleaned_text,
                                embeddings = embeddings)
  
  if (!is.null(calculated_embeddings)){
    return(model) # if embeddings provided, only return the model
  } else{
    return(list(model, embeddings)) # if embeddings not provided, return embeddings and model
  }
  
    
}
