bt_make_ctfidf <- function(reduce_frequent_words = TRUE, bm25_weighting = FALSE) {

  #Input validation
  stopifnot(is.logical(bm25_weighting),
            is.logical(reduce_frequent_words))

  #Import library
  bt_vectorisers <- reticulate::import("bertopic.vectorizers")

  #Instantiate with args
  vectoriser <- bt_vectorisers$ClassTfidfTransformer(bm25_weighting = bm25_weighting, reduce_frequent_words = reduce_frequent_words)

  #Return object
  return(vectoriser)
}
