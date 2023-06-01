#' fit a bertopic model to cleaned data
#'
#' @param cleaned_text cleaned text column that the model should be fit with
#' @param min_topic_size minimum topic size 
#' @param ngram_range ngram range for topic representation
#' @param diversity diversity of topic representation (1 = diverse, 0 = not diverse)
#' @param stopwords 
#' @param random_state 
#'
#' @return
#' @export
#'
#' @examples
fit_model <- function(cleaned_text,
                      min_topic_size = NULL,
                      ngram_range = c(1,3),
                      embedding_model = "all-MiniLM-L6-v2",
                      use_mps = TRUE,
                      diversity = diversity,
                      stopwords = TRUE,
                      random_state = 42){
  
  # create umap model
  umap <- reticulate::import("umap")
  umap_model <- umap$UMAP(random_state = random_state)
  
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
  vecrtorizer_model <- vectorizer$CountVectorizer(ngram_range = tuple(1L,3L),
                                                  stop_words = stopword)
  
  # embeddings
  sentence_transformers <- reticulate::import("sentence_transformers")
  sentence_model <- sentence_transformers$SentenceTransformer(embedding_model)
  embeddings <- sentence_model$encode(cleaned_text)
  
  # initiate model
  model <- py$bertopic$BERTopic(vectorizer_model = vecrtorizer_model)
  output <- model$fit_transform(data$text_clean)
  
  return(model)
  
}