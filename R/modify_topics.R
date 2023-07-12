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

# bt_distribute_outliers <- function(model = model,
#                             documents,
#                             topics,
#                             probability_matrix = NULL,
#                             threshold,
#                             embeddings = NULL,
#                             method = c("probabilities", 
#                                        "distributions",
#                                        "ctfidf",
#                                        "embeddings")){
#   #Input validation
#   if(!grepl("^bertopic", methods::S3Class(model)[[1]])){
#     stop("model should be a BERTopic model")
#   }
#   
#   if(is.null(model$topics_)){
#     stop("BERTopic model is not fitted, use bt_fit_model to fit.")
#   }
#   
#   if(!is.null(probabilities) & !is.list(probabilties)){
#     stop("probabilities must be a matrix") 
#   }
#   
#   if(!method %in% c("probabilities", 
#                     "distributions",
#                     "ctfidf",
#                     "embeddings")){
#     stop("method must be one of: probabilties, distributions, ctf-idf or embeddings") 
#   }
#   
#   
#   # what do I want to return from this? 
#   new_topics <- model$reduce_outliers(documents = documents, 
#                                       topics = topics,
#                                       probabilities = probability_matrix,
#                                       strategy = method,
#                                       threshold = threshold)
#   return(model)
# } 


#' Title
#'
#' @param model 
#' @param documents 
#' @param topics 
#' @param probability_matrix 
#' @param threshold 
#'
#' @return
#' @export
#'
#' @examples
bt_outliers_probs <- function(model = model,
                                     documents,
                                     topics,
                                     probability_matrix,
                                     threshold){
  
  #Input validation
  if(!grepl("^bertopic", methods::S3Class(model)[[1]])){
    stop("model should be a BERTopic model")
  }
  
  if(is.null(model$topics_)){
    stop("BERTopic model is not fitted, use bt_fit_model to fit.")
  }
  
  if(!is.null(probability_matrix) & !is.matrix(probability_matrix)){
    stop("topic-document probabilitiy matrix must be a matrix") 
  }
  
  stopifnot(length(topics) == length(documents), # same number of topics as docs
            dim(probability_matrix)[1] == length(documents), # same number of docs 
            dim(probability_matrix)[2] == length(unique(topics)) - 1, # same number of topics
            is.character(documents), 
            is.numeric(threshold))
  
  ## validation finished ##
  
  
  new_topics <- model$reduce_outliers(documents = documents,
                                      topics = topics,
                                      probabilities = probability_matrix,
                                      strategy = "probabilities",
                                      threshold = threshold)

  new_topics_matched <- data.frame(message = documents,
                                   old_topics = topics,
                                   new_topics = new_topics)
  
  return(new_topics_matched)
}


#' Title
#'
#' @param model 
#' @param documents 
#' @param topics 
#' @param embeddings 
#' @param threshold 
#'
#' @return
#' @export
#'
#' @examples
bt_outliers_embeddings <- function(model = model,
                                          documents,
                                          topics,
                                          embeddings,
                                          threshold){
  #Input validation
  if(!grepl("^bertopic", methods::S3Class(model)[[1]])){
    stop("model should be a BERTopic model")
  }
  
  if(is.null(model$topics_)){
    stop("BERTopic model is not fitted, use bt_fit_model to fit.")
  }
  
  stopifnot(length(topics) == length(documents), # same number of topics as docs
            nrow(embeddings) == length(documents), # same number of topics
            is.array(embeddings)| is.data.frame(embeddings) | is.null(embeddings),
            is.character(documents),
            is.numeric(threshold))
  
  ## validation finished ##
  
  
  new_topics <- model$reduce_outliers(documents = documents,
                                      topics = topics,
                                      embeddings = embeddings,
                                      strategy = "embeddings",
                                      threshold = threshold)
  
  new_topics_matched <- data.frame(message = documents,
                                   old_topics = topics,
                                   new_topics = new_topics)
  
  return(new_topics_matched)
  
}

#' Title
#'
#' @param model 
#' @param documents 
#' @param topics 
#' @param threshold 
#'
#' @return
#' @export
#'
#' @examples
bt_outliers_tokenset_similarity <- function(...,
                                            model = model,
                                            documents,
                                             topics,
                                             window,
                                             stride,
                                             threshold){
  ##### NEED TO DO MORE WORK HERE ON ... #####
  #Input validation
  if(!grepl("^bertopic", methods::S3Class(model)[[1]])){
    stop("model should be a BERTopic model")
  }
  
  if(is.null(model$topics_)){
    stop("BERTopic model is not fitted, use bt_fit_model to fit.")
  }
  
  stopifnot(length(topics) == length(documents), # same number of topics as docs
            nrow(embeddings) == length(documents), # same number of topics
            is.array(embeddings)| is.data.frame(embeddings) | is.null(embeddings),
            is.character(documents),
            is.numeric(threshold))
  
  ## validation finished ##
  
  
  new_topics <- model$reduce_outliers(documents = documents,
                                      topics = topics,
                                      embeddings = embeddings,
                                      strategy = "embeddings",
                                      threshold = threshold)
  
  new_topics_matched <- data.frame(message = documents,
                                   old_topics = topics,
                                   new_topics = new_topics)
  
  return(new_topics_matched)
}


#' Title
#'
#' @param model 
#' @param documents 
#' @param topics 
#' @param threshold 
#'
#' @return
#' @export
#'
#' @examples
bt_outliers_ctfidf <- function(model = model,
                                      documents,
                                      topics,
                                      threshold){
  
  
}
