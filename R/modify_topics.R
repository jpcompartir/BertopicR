#' Merges list(s) of topics together
#' 
#' @description Merge topics of an already-fitted BertopicR model. 
#' You can feed in a list, to merge topics together, or a list of lists to perform 
#' merges on multiple groups of topics.
#' 
#' @details This function updates the model so that the topics in the topics_to_merge list
#' become 1 topic. The grouped topics take the topic representation (Name) of the 
#' first topic in the list. Any number of topics can be merged together, if you would
#' like to merge two separate groups of topics, you must pass a list of lists/vectors as
#' topics_to_merge eg. list(c(1, 3), c(0, 6, 7)) or list(list(1, 3), list(0, 6, 7))
#'
#' @param fitted_model Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.
#' @param documents documents which the model was fitted to
#' @param topics_to_merge list (or list of lists/vectors) of topics created by your bertopic model that you wish to merge. Topics should be given in numeric form.
#'
#' @return bertopic model with specified topics merged
#' @export
#' 
#' @examples \dontrun{bt_merge_topics(
#' fitted_model = model,
#' documents = documents,
#' topics_to_merge = list(c(1, 3), c(0, 6, 7)))}
#'
bt_merge_topics <- function(fitted_model,
                            documents,
                            topics_to_merge){
  #Input validation
  
  if(!is.null(fitted_model)) {
    # Check fitted model
    test_is_fitted_model(fitted_model)
  }
  
  if(!is.list(topics_to_merge)){
    stop("topics_to_merge must be a list or where you want to perform multiple merges, a list of lists/vectors") 
  }
  
  if(!all(sapply(topics_to_merge, is.numeric))){
    stop("topics must be numeric") 
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
  
  fitted_model$merge_topics(documents,
                            topics_to_merge_int)
  
  return(fitted_model)
}

#' #' Calculates Topic-Document Probability Matrix
#' #' 
#' #' @description
#' #' Uses hdbscan soft clustering to create a matrix with a row per document and 
#' #' column per model topic. The intersection of row and column is the probability
#' #' that that document is part of that topic. This is only applicable for models 
#' #' created using a hdbscan clustering method (bt_make_clusterer_hdbscan or equivalent) 
#' #' 
#' #'
#' #' @param fitted_model Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.
#' #'
#' #' @return topic-document probability matrix
#' #' @export
#' #'
#' bt_probability_matrix <- function(fitted_model){
#'   
#'   #### input validation ###
#'   
#'   # Check fitted model
#'   test_is_fitted_model(fitted_model)
#' 
#'   # validate hdbscan model used as clusterer
#'   if(!any(grepl("hdbscan\\.hdbscan_\\.HDBSCAN", methods::S3Class(fitted_model$hdbscan_model)))){
#'     stop("Clustering method is: ", methods::S3Class(fitted_model$hdbscan_model)[[1]], "\nProbability Matrix required HDBSCAN." )
#'   }
#'   
#'   #### end of input validation ####
#'   
#'   # import hdbscan
#'   hdb <- reticulate::import("hdbscan")
#'   
#'   # get probability matrix
#'   probability_matrix <- hdb$all_points_membership_vectors(fitted_model$hdbscan_model)
#'   
#'   # set environment so that py_eval can access local variables
#'   withr::with_options(c(reticulate.engine.environment = rlang::current_env()), {
#'     
#'     # map probabilities to topics incase any topic merging occured before this
#'     probability_matrix_mapped <- reticulate::py_eval("r.fitted_model._map_probabilities(r.probability_matrix)")
#'   })
#' 
#'   return(probability_matrix_mapped)
#'   }

#' #' Redistributes outliers based on document-topic probability
#' #' 
#' #' @description Uses hdbscan soft clustering calculated probabilities to find 
#' #' the most appropriate topic for each outlier (topic -1) document. Returns a df containing 
#' #' the current and potential new topics for each document. Note that the purpose
#' #' of this function is to obtain a new list of topics that can then be used to 
#' #' update the model, it does not make any changes to the model itself, the topic 
#' #' classification the model outputs does not change after running this function. 
#' #' The bt_update_topics function needs to be used to make the change to the model 
#' #' itself. 
#' #' 
#' #' @details If clustering was performed using hdbscan, you will likely have a 
#' #' number of topics in the -1 outlier category. Soft clustering is a clustering 
#' #' method that assigns a probability of being in each cluster to each datum. We
#' #' can use the hdbscan soft clustering capabilities to reassign the outlier documents
#' #' to topics based on these probabilities. 
#' #'
#' #' @param fitted_model Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.
#' #' @param documents documents to which the model was fit
#' #' @param topics current topics associated with the documents
#' #' @param probability_matrix topic-document probability matrix (probability of each document being a member of each topic). This can be obtained using the bt_probability_matrix function
#' #' @param threshold minimum probability for outlier to be reassigned
#' #'
#' #' @return df with document, old topic, new topic
#' #' @export
#' #'
#' bt_outliers_probs <- function(fitted_model,
#'                               documents,
#'                               topics,
#'                               probability_matrix,
#'                               threshold = 0.3){
#'           
#'           
#'   
#'   #Input validation
#'   
#'   # check is fitted model
#'   if(!is.null(fitted_model)) {
#'     test_is_fitted_model(fitted_model)
#'   }
#'   
#'   # check probability matrix is matrix
#'   if(!is.null(probability_matrix) & !is.matrix(probability_matrix)){
#'     stop("topic-document probabilitiy matrix must be a matrix") 
#'   }
#'   
#'   # check inputs
#'   stopifnot(dim(probability_matrix)[1] == length(documents), # same number of docs 
#'             dim(probability_matrix)[2] == length(unique(topics)) - 1, # same number of topics
#'             is.character(documents), 
#'             is.numeric(threshold))
#'   
#'   # check same number topics as docs
#'   if(!is.null(topics) & !is.null(documents)) {
#'     test_labels_lengths(documents, topics)
#'   }
#'   
#'   ## validation finished ##
#'   
#'   
#'   new_topics <- fitted_model$reduce_outliers(documents = documents,
#'                                       topics = topics,
#'                                       probabilities = probability_matrix,
#'                                       strategy = "probabilities",
#'                                       threshold = threshold)
#' 
#'   new_topics_matched <- data.frame(message = documents,
#'                                    current_topics = topics,
#'                                    new_topics = unlist(new_topics))
#'   
#'   return(new_topics_matched)
#' }


#' Redistributes outliers using embeddings
#' 
#' @description Uses the cosine similarity of the document embeddings to find the
#' topic closest to each outlier document and reassigns these documents accordingly.
#' Note that the purpose
#' of this function is to obtain a new list of topics that can then be used to 
#' update the model, it does not make any changes to the model itself, the topic 
#' classification the model outputs does not change after running this function. 
#' The bt_update_topics function needs to be used to make the change to the model 
#' itself. 
#'
#' @param fitted_model Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.
#' @param documents documents to which the model was fit
#' @param topics current topics associated with the documents
#' @param embeddings embeddings used to create topics.
#' @param threshold minimum probability for outlier to be reassigned
#'
#' @return df with document, old topic, new topic
#' @export
#'
bt_outliers_embeddings <- function(fitted_model,
                                   documents,
                                   topics,
                                   embeddings,
                                   embedding_model = NULL,
                                   threshold = 0.3){
  #Input validation
  # is there an embedding model specified?
  if (is.null(fitted_model$embedding_model$embedding_model) & is.null(embedding_model)){
    stop(fitted_model, " embedding model is NULL and no embedding_model passed to function. Specify embedding_model in function input.")
  }
  
  # Check fitted model
  if(!is.null(fitted_model)) {
    test_is_fitted_model(fitted_model)
  }
  
  # check same number topics as docs
  if(!is.null(topics) & !is.null(documents)) {
    test_labels_lengths(documents, topics)
  }
  
  # Check the length of documents is equal to the number of embeddings and embeddings is array or df, and if not, stop.
  if(!is.null(embeddings)) {
    test_embeddings_dims(documents, embeddings)
    
    embeddings <- convert_to_np_array(embeddings)
  }
  
  stopifnot(is.character(documents),
            is.numeric(threshold))
  
  ## validation finished ##
  
  # set embedding model to that specified in function input
  if (!is.null(embedding_model)){
    bt_backend <- reticulate::import("bertopic.backend._utils")
    embedding_model <- bt_backend$select_backend(embedding_model = embedding_model)
    fitted_model$embedding_model <- embedding_model
  }
  
  # otherwise use current embedding model
  else if (is.null(embedding_model)){
    message("No embedding model provided, using current sentence embedding model:\n", fitted_model$embedding_model$embedding_model)
  }
  
  new_topics <- fitted_model$reduce_outliers(documents = documents,
                                             topics = topics,
                                             embeddings = embeddings,
                                             strategy = "embeddings",
                                             threshold = threshold)
  
  new_topics_matched <- data.frame(message = documents,
                                   current_topics = topics,
                                   new_topics = unlist(new_topics))
  
  return(new_topics_matched)
  
}

#' Redistributes outliers using tokenset c-TF-IDF scores
#' 
#' @description Divides documents into tokensets and calculates the c-TF-IDF 
#' similarity of each tokenset to each topic. For each outlier document, the similarity 
#' scores of each tokenset for each topic are summed together by topic and the outlier is 
#' redistributed to the topic with the highest similarity. Note that the purpose
#' of this function is to obtain a new list of topics that can then be used to 
#' update the model, it does not make any changes to the model itself, the topic 
#' classification the model outputs does not change after running this function. 
#' The bt_update_topics function needs to be used to make the change to the model 
#' itself. 
#'
#' @param ... Optional or additional parameters passed to approximate_distribution function, e.g. batch_size
#' @param fitted_model Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.
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
                                            fitted_model,
                                            documents,
                                            topics,
                                            window = 4,
                                            stride = 1,
                                            threshold = 0.3){
  #Input validation
  
  # check fitted model
  if(!is.null(fitted_model)) {
    test_is_fitted_model(fitted_model)
  }
  
  # check same number topics as docs
  if(!is.null(topics) & !is.null(documents)) {
    test_labels_lengths(documents, topics)
  }
  
  # check types
  stopifnot(is.character(documents),
            is.numeric(threshold),
            is.numeric(window),
            is.numeric(stride))
  
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
  
  new_topics <- fitted_model$reduce_outliers(documents = documents,
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
                                   current_topics = topics,
                                   new_topics = unlist(new_topics))
  
  return(new_topics_matched)
}


#' Redistributes outliers using c-TF-IDF scores
#' 
#' @description Calculates the cosine similarity of c-TF-IDF between documents 
#' and topics and redistributes outliers based on the topic it has the highest 
#' similarity to. Note that the purpose
#' of this function is to obtain a new list of topics that can then be used to 
#' update the model, it does not make any changes to the model itself, the topic 
#' classification the model outputs does not change after running this function. 
#' The bt_update_topics function needs to be used to make the change to the model 
#' itself. 
#'
#' @param fitted_model Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.
#' @param documents documents to which the model was fit
#' @param topics current topics associated with the documents
#' @param threshold minimum probability for outlier to be reassigned
#'
#' @return df with document, old topic, new topic
#' @export
#'
bt_outliers_ctfidf <- function(fitted_model,
                               documents,
                               topics,
                               threshold = 0.3){
  #Input validation
  
  # Check fitted model
  if(!is.null(fitted_model)) {
    test_is_fitted_model(fitted_model)
  }
  
  # same num topics as docs
  if(!is.null(topics) & !is.null(documents)) {
    test_labels_lengths(documents, topics)
  }
  
  stopifnot(is.character(documents),
            is.numeric(threshold))
  
  ## validation finished ##
  
  new_topics <- fitted_model$reduce_outliers(documents = documents,
                                             topics = topics,
                                             strategy = "c-tf-idf",
                                             threshold = threshold) # dict for approx_distribution
  
  new_topics_matched <- data.frame(message = documents,
                                   current_topics = topics,
                                   new_topics = unlist(new_topics))
  
  return(new_topics_matched) 
  
}

#' Update Topic Representations 
#' 
#' @description
#' Updates topics and their representations to be based on the document-topic 
#' classification described in the list of new_topics. As when initiating the model
#' with bt_compile_model, if you want to manipulate the topic representations you
#' must use a vectoriser/ctfidf model, these can be the same as those used in 
#' bt_compile_model. 
#' 
#' NOTE: The bertopic model you are working with is a pointer to a python object 
#' at a point in memory. This means that the input and the output model cannot be 
#' differentiated between without explicitly saving the model before performing 
#' this operation. A model is returned out of convention, in reality, the function
#' is applied to and changes the input model. The input model will be the same as
#' the output model after the operation has been performed.
#' 
#' @details
#' NOTE: If using this function to update outlier topics, it may lead to errors 
#' if topic reduction or topic merging techniques are used afterwards. The reason 
#' for this is that when you assign a -1 document to topic 1 and another -1 document 
#' to topic 2, it is unclear how you map the -1 documents. Is it matched to topic 1 or 2.
#' 
#'
#' @param fitted_model Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.
#' @param documents documents to which the model was fit
#' @param new_topics Topics to update model with
#' @param representation_model model for updating topic representations
#' @param embedding_model if updating representations using a MaximalMarginalRelevance or KeyBERTInspired representation model, a word embedding model is required to calculate word embeddings. If no word embedding model is passed the sentence embedding model passed to bt_compile_model will be used. If this was an empty model, this function will not work for MaximalMarginalRelevance or KeyBERTInspired representation models.
#' @param vectoriser_model Model for vectorising input for topic representations (Python object)
#' @param ctfidf_model Model for performing class-based tf-idf (ctf-idf) (Python object)
#'
#' @return the updated model
#' @export
#'
bt_update_topics <- function(fitted_model, 
                             documents, 
                             new_topics = NULL,
                             representation_model = NULL,
                             embedding_model = NULL,
                             vectoriser_model = NULL,
                             ctfidf_model = NULL){
  
  #### Validation ####
  # Check fitted model
  if(!is.null(fitted_model)) {
    test_is_fitted_model(fitted_model)
  }
  
  # same num topics as docs
  if(!is.null(new_topics) & !is.null(documents)) {
    test_labels_lengths(documents, new_topics)
  }
  
  stopifnot(is.null(new_topics) | is.numeric(new_topics),
            is.character(documents),
            test_is_python_object(vectoriser_model) | is.null(vectoriser_model),
            test_is_python_object(ctfidf_model) | is.null(ctfidf_model))
  
  if (any(grepl("KeyBERT|MaximalMarginalRelevance", methods::S3Class(representation_model))) &
      is.null(fitted_model$embedding_model$embedding_model) & is.null(embedding_model)){
    stop("The current embedding_model instance variable is NULL. Must pass an embedding_model")
  }
  
  # If the embedder isn't a sentence transformers object, stop early.
  if(!is.null(embedding_model) & !grepl("^sentence_tran", class(embedding_model)[[1]])){
    stop("This package currently only supports embedding models from the sentence transformer library, embedder should be a sentence transformers model")
  }
  
  #### end of validation ####
  
  # set embedding model to that specified in function input
  if (!is.null(embedding_model)){
    bt_backend <- reticulate::import("bertopic.backend._utils")
    embedding_model <- bt_backend$select_backend(embedding_model = embedding_model)
    fitted_model$embedding_model <- embedding_model
  }
  
  # otherwise use current embedding model
  else if (is.null(embedding_model)){
    message("No embedding model provided, using current sentence embedding model:\n", fitted_model$embedding_model$embedding_model)
  }
  
  # this was causing errors to do with max and min df - will look into this and reimplement
  # # default vectoriser if none given
  # if(is.null(vectoriser_model)){
  #   vectoriser_model <- bt_make_vectoriser()
  #   message("\nNo vectorising model provided, creating model with default parameters")
  # }
  # 
  # # default ctfidf if none given
  # if(is.null(ctfidf_model)){
  #   ctfidf_model <- bt_make_ctfidf()
  #   message("\nNo ctfidf model provided, creating model with default parameters")
  # }
  
  
  
  fitted_model$update_topics(docs = documents, 
                             topics = new_topics, 
                             representation_model = representation_model,
                             vectorizer_model = vectoriser_model, 
                             ctfidf_model = ctfidf_model)
  
  return(fitted_model)
  
}
