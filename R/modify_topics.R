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

#' Redistributes outliers using probabilities
#' 
#' Uses HDBSCAN calculated probabilities to find the most appropriate topic for 
#' 
#' each outlier document.
#'
#' @param model BERTopic model
#' @param documents documents to which the model was fit
#' @param topics current topics associated with the documents
#' @param probability_matrix topic-document probability matrix (probability of each document being a member of each topic)
#' @param threshold minimum probability for outlier to be reassigned
#'
#' @return df with document, old topic, new topic
#' @export
#'
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
                                           new_topics = unlist(new_topics))
          
          return(new_topics_matched)
        }


#' Redistributes outliers using embeddings
#' 
#' Uses cosine similarity of embeddings to find the most appropriate topic for 
#' 
#' each outlier document.
#'
#' @param model BERTopic model
#' @param documents documents to which the model was fit
#' @param topics current topics associated with the documents
#' @param embeddings embeddings used to create topics
#' @param threshold minimum probability for outlier to be reassigned
#'
#' @return df with document, old topic, new topic
#' @export
#'
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
                                   new_topics = unlist(new_topics))
  
  return(new_topics_matched)
  
}

#' Redistributes outliers using tokenset c-TF-IDF scores
#' 
#' Divides documents into tokensets and calculates their c-TF-IDF similarity to 
#' 
#' each topic. Similarities of each tokenset to each topic are summed to and the 
#' 
#' outlier is redistributed to the most similar topic.
#'
#' @param ... Optional or additional parameters passed to approximate_distribution function, e.g. batch_size
#' @param model BERTopic model
#' @param documents documents to which the model was fit
#' @param topics current topics associated with the documents
#' @param window size of the moving window which is the number of tokens in a tokenset
#' @param stride how far the window should move at each step (number of words to
#' skip when moving to next tokenset)
#' @param threshold minimum probability for outlier to be reassigned
#'
#' @return df with document, old topic, new topic
#' @export
#'
bt_outliers_tokenset_similarity <- function(...,
                                            model = model,
                                            documents,
                                             topics,
                                             window = 4,
                                             stride = 1,
                                             threshold){
  #Input validation
  if(!grepl("^bertopic", methods::S3Class(model)[[1]])){
    stop("model should be a BERTopic model")
  }
  
  if(is.null(model$topics_)){
    stop("BERTopic model is not fitted, use bt_fit_model to fit.")
  }
  
  stopifnot(length(topics) == length(documents), # same number of topics as docs
            is.character(documents),
            is.numeric(threshold))
  
  ## validation finished ##
  
  dots <- rlang::list2(...) # place ellipses in list

  dots_unlist <- c() # empty vec
  for (i in dots){
    if (is.numeric(i)){
      i = as.integer(i)
    }
    dots_unlist <- append(dots_unlist, i) # place ellipses in vec
  } 
  
  
  
  # Convert window and stride to integers
  window_int <- as.integer(window)
  stride_int <- as.integer(stride)
  
  new_topics <- model$reduce_outliers(documents = documents,
                                      topics = topics,
                                      strategy = "distributions",
                                      threshold = threshold,
                                      distributions_params = 
                                        reticulate::py_dict(
                                          keys = c("window", "stride",
                                                   names(dots_unlist)), 
                                          values = c(4L, 1L,
                                                     dots_unlist), 
                                          convert = TRUE)) # dict for approx_distribution

  new_topics_matched <- data.frame(message = documents,
                                   old_topics = topics,
                                   new_topics = unlist(new_topics))
  
  return(new_topics_matched)
}


#' Redistributes outliers using c-TF-IDF scores
#' 
#' Uses cosine similarity of document and topic c-TF-IDF to find the most 
#' 
#' appropriate topic for each outlier document.
#'
#' @param model BERTopic model
#' @param documents documents to which the model was fit
#' @param topics current topics associated with the documents
#' @param threshold minimum probability for outlier to be reassigned
#'
#' @return df with document, old topic, new topic
#' @export
#'
bt_outliers_ctfidf <- function(model = model,
                                      documents,
                                      topics,
                                      threshold){
  #Input validation
  if(!grepl("^bertopic", methods::S3Class(model)[[1]])){
    stop("model should be a BERTopic model")
  }
  
  if(is.null(model$topics_)){
    stop("BERTopic model is not fitted, use bt_fit_model to fit.")
  }
  
  stopifnot(length(topics) == length(documents), # same number of topics as docs
            is.character(documents),
            is.numeric(threshold))
  
  ## validation finished ##
  
  new_topics <- model$reduce_outliers(documents = documents,
                                      topics = topics,
                                      strategy = "c-tf-idf",
                                      threshold = threshold) # dict for approx_distribution
  
  new_topics_matched <- data.frame(message = documents,
                                   old_topics = topics,
                                   new_topics = unlist(new_topics))
  
  return(new_topics_matched) 
  
}
