bt_make_vectoriser <- function(ngram_range = NULL, stop_words = NULL, min_frequency= NULL, max_features = NULL, ...) {

  stopifnot(is.vector(ngram_range), length(ngram_range) == 2, is.numeric(ngram_range))

  #Create a tuple from the ngram_range input
  ngram_range <-  reticulate::tuple(as.integer(ngram_range[1]), as.integer(ngram_range[2]))

  #Text feature extraction from sklearn
  sklearn_fet <- reticulate::import("sklearn.feature_extraction.text")

  vectorizer <- sklearn_fet$CountVectorizer(ngram_range = ngram_range, stop_words = stop_words, min_df = min_frequency, max_features = max_features)
}
