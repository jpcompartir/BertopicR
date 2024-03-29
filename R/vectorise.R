#' Create a text vectoriser
#'
#' This function uses Python's sklearn for feature extraction and count vectorisation.
#' It creates a CountVectorizer object with the specified parameters.
#' CountVectorizer is a way to convert text data into vectors as model input. Used inside a BertopicR topic modelling pipeline.
#'
#' @param ... Additional parameters passed to sklearn's CountVectorizer
#' @param ngram_range A vector of length 2 (default c(1, 2)) indicating the lower and upper boundary of the range of n-values for
#'        different word n-grams or char n-grams to be extracted as features. All values of n such
#'        that min_n <= n <= max_n will be used. For example an ngram_range of c(1, 1) means only unigrams,
#'        c(1, 2) means unigrams and bigrams, and c(2, 2) means only bigrams.
#' @param stop_words String (default 'english'). If a string, it is passed to _check_stop_list and the
#'        appropriate stop list is returned. 'english' is currently the default.
#' @param min_frequency Integer or float (default 0.1). When building the vocabulary ignore terms that have a
#'        corpus frequency strictly lower than the given threshold. If min_frequency is explicitly defined to be
#'        an integer, it is assumed to represent the absolute count. If min_frequency is not explicitly specified
#'        as an integer and is between 0 and 1, it is assumed to represent a proportion of documents, if it is a 
#'        whole number it is assumed to represent the absolute count.
#' @param max_features Integer or NULL (default NULL). If not NULL, build a vocabulary that only considers
#'        the top max_features ordered by term frequency across the corpus.
#'
#' @return An sklearn CountVectorizer object configured with the provided parameters
#' @export
#' 
#' @examples
#' # vectoriser model that converts text docs to ngrams with between 1 - 2 tokens
#' vectoriser <- bt_make_vectoriser(ngram_range = c(1, 2), stop_words = "english")
#' 
#'# vectoriser model that converts text docs to ngrams with between 1 - 3 tokens
#' vectoriser <- bt_make_vectoriser(ngram_range = c(1, 3), stop_words = "english")
#' 
#' # You can implement custom stopwords or stopwords from other sources
#' \dontrun{
#' stopwords_cat <- tm::stopwords(kind = "catalan")
#' vectoriser <- bt_make_vectoriser(ngram_range = c(1, 3), stop_words = stopwords_cat)
#' }
#' 
#' custom_stopwords <- c("these", "words", "are", "not", "helpful")
#' vectoriser <- bt_make_vectoriser(ngram_range = c(1,2), stop_words = custom_stopwords)
#' 
bt_make_vectoriser <- function(..., ngram_range = c(1L, 2L), stop_words = "english", min_frequency = 0.1, max_features = NULL) {
  
  stopifnot(is.vector(ngram_range),
            length(ngram_range) == 2,
            is.numeric(ngram_range),
            is.character(stop_words),
            is.numeric(min_frequency))
  
  #Create a tuple from the ngram_range input
  ngram_tuple <-  reticulate::tuple(as.integer(ngram_range[1]), as.integer(ngram_range[2]))
  
  if (!(!is.integer(min_frequency) & min_frequency > 0 & min_frequency < 1)){
    min_frequency = as.integer(min_frequency)
  }
  
  min_frequency <- as.integer(min_frequency) #convert to int for Python
  
  #Text feature extraction from sklearn
  sklearn_fet <- reticulate::import("sklearn.feature_extraction.text")
  
  vectorizer <- sklearn_fet$CountVectorizer(
    ngram_range = ngram_tuple,
    stop_words = stop_words,
    min_df = min_frequency,
    max_features = max_features, ...)
  
  return(vectorizer)
}