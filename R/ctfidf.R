#' Create an instance of the ClassTfidfTransformer from the bertopic.vectorizers module
#'
#' This function creates an instance of the ClassTfidfTransformer from the bertopic.vectorizers module,
#' with the provided arguments. It is used to generate representations for topics by selecting words which are frequent within a topic and less frequent in the entire corpus.
#'
#' @param reduce_frequent_words should frequent words be reduced? Default is TRUE.
#' @param bm25_weighting  should BM25 weighting be used? Default is FALSE.
#'
#' @return A ctfidf model (Python object).
#'
#' @export
#' 
#' @examples
#' ctfidf <- bt_make_ctfidf(reduce_frequent_words = TRUE, bm25_weighting = FALSE)
#' 
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
