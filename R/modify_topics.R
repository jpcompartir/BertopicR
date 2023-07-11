#' Merge topics
#' Merge topics that have been generated with a BERTopic model
#'
#' @param model Output of bt_compile_model() or another bertopic topic model
#' @param documents Your documents to topic model on
#' @param topics_to_merge list of topics created by your bertopic model that you wish to merge
#'
#' @return bertopic model with specified topics merged
#' @export
#'
bt_merge_topics <- function(model = model,
                            documents,
                            topics_to_merge = list()){
  #Input validation
  if(!grepl("^bertopic", methods::S3Class(model)[[1]])){
    stop("model should be a BERTopic model")
  }
  
  if(is.null(model$topics_)){
    stop("BERTopic model is not fitted, use bt_fit_model to fit.")
  }
  
  if(!is.list(topics_to_merge)){
    stop("topics_to_merge must be a list or where you want to perform multiple merges, a list of lists/vectors") 
  }
  
  if(!is.character(documents)){
    stop("documents must be of type character") 
  }
  
  # covert topics to integers 
  topics_to_merge_int <- list()
  for (i in 1:length(topics_to_merge)) {
    topics_to_merge_int[[i]] <- as.integer(topics_to_merge[[i]])
  }
  
  # convert to python list
  topics_to_merge <- reticulate::r_to_py(topics_to_merge)
  
  model$merge_topics(documents,
                      topics_to_merge_int)
  
return(model)
}


#' Merge topics
#' Merge topics that have been generated with a BERTopic model
#'
#' @param model Output of bt_compile_model() or another bertopic topic model
#' @param documents Your documents to topic model on
#' @param topics_to_merge list of topics created by your bertopic model that you wish to merge
#'
#' @return bertopic model with specified topics merged
#' @export
#'
bt_merge_topics <- function(model = model,
                            documents,
                            topics_to_merge = list()){
  #Input validation
  if(!grepl("^bertopic", methods::S3Class(model)[[1]])){
    stop("model should be a BERTopic model")
  }
  
  if(is.null(model$topics_)){
    stop("BERTopic model is not fitted, use bt_fit_model to fit.")
  }
  
  if(!is.list(topics_to_merge)){
    stop("topics_to_merge must be a list or where you want to perform multiple merges, a list of lists/vectors") 
  }
  
  if(!is.character(documents)){
    stop("documents must be of type character") 
  }
  
  # covert topics to integers 
  topics_to_merge_int <- list()
  for (i in 1:length(topics_to_merge)) {
    topics_to_merge_int[[i]] <- as.integer(topics_to_merge[[i]])
  }
  
  # convert to python list
  topics_to_merge <- reticulate::r_to_py(topics_to_merge)
  
  model$merge_topics(documents,
                     topics_to_merge_int)
  
  return(model)
} 

bt_reduce_outliers <- function(model = model,
                            documents,
                            probabilities = NULL,
                            method = c("probabilities", 
                                       "distributions",
                                       "ctfidf",
                                       "embeddings")){
  #Input validation
  if(!grepl("^bertopic", methods::S3Class(model)[[1]])){
    stop("model should be a BERTopic model")
  }
  
  if(is.null(model$topics_)){
    stop("BERTopic model is not fitted, use bt_fit_model to fit.")
  }
  
  if(!is.null(probabilities) & !is.list(probabilties)){
    stop("probabilities must be a list") 
  }
  
  if(!method %in% c("probabilities", 
                    "distributions",
                    "ctfidf",
                    "embeddings")){
    stop("method must be one of: probabilties, distributions, ctf-idf or embeddings") 
  }
  
  # covert probabilties to integers 
  topics_to_merge_int <- list()
  for (i in 1:length(topics_to_merge)) {
    topics_to_merge_int[[i]] <- as.integer(topics_to_merge[[i]])
  }
  
  # convert to python list
  topics_to_merge <- reticulate::r_to_py(topics_to_merge)
  
  model$merge_topics(documents,
                     topics_to_merge_int)
  
  return(model)
} 
