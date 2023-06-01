#' fit a bertopic model to cleaned data
#'
#' @param cleaned_text cleaned text column that the model should be fit with
#' @param min_topic_size minimum topic size 
#' @param ngram_range ngram range for topic representation - input must be of type 
#' tuple: use the reticulate function reticulate::tuple()
#' @param embedding_model which embedding model to use
#' @param use_mps accelerator to use - default is mps, use NULL if none
#' @param diversity diversity of topic representation (1 = diverse, 0 = not diverse)
#' @param stopwords whether or not to remove stopwords in topic representations
#' @param random_state random state to pass to umap
#'
#' @return
#' @export
#'
#' @usage fit_model(
#' cleaned_text,
#' min_topic_size = NULL,
#' ngram_range = tuple(1,1),
#' embedding_model = "all-MiniLM-L6-v2",
#' accelerator = "mps",
#' diversity = 0.3,
#' stopwords = TRUE,
#' random_state = 42)
#' 
fit_model <- function(cleaned_text,
                      min_topic_size = NULL,
                      ngram_range = tuple(1L, 2L),
                      embedding_model = "all-MiniLM-L6-v2",
                      accelerator = "mps",
                      diversity = 0.3,
                      stopwords = TRUE,
                      random_state = 42L){
  
  # create umap model
  umap <- reticulate::import("umap")
  umap_model <- umap$UMAP(n_neighbors=15, 
                          n_components=5, 
                          min_dist=0.0, 
                          metric='cosine', 
                          random_state = 42L)
  
  # create representation model
  representation <- reticulate::import("bertopic.representation")
  representation_model <- representation$MaximalMarginalRelevance(diversity = diversity)

  # create vectoriser model
  if (stopwords){
    stopword = "english"
  }
  else {
    stopword = NULL
  }
  vectorizer <- reticulate::import("sklearn.feature_extraction.text")
  vecrtorizer_model <- vectorizer$CountVectorizer(ngram_range = ngram_range,
                                                  stop_words = stopword)
  
  # embeddings
  sentence_transformers <- reticulate::import("sentence_transformers")
  sentence_model <- sentence_transformers$SentenceTransformer(embedding_model)
  embeddings <- sentence_model$encode(cleaned_text, device = accelerator)
  
  # initiate model
  model <- py$bertopic$BERTopic(min_topic_size = min_topic_size,
                                umap_model = umap_model,
                                representation_model = representation_model,
                                vectorizer_model = vecrtorizer_model)
  
  output <- model$fit_transform(data$text_clean,
                                embeddings = embeddings)
  
  return(model)
  
}