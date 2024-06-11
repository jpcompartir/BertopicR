#' Merges list(s) of topics together
#' 
#' @description Merge topics of an already-fitted BertopicR model. 
#' You can feed in a list, to merge topics together, or a list of lists to perform 
#' merges on multiple groups of topics.
#' 
#' NOTE: The bertopic model you are working with is a pointer to a python object 
#' at a point in memory. This means that the input and the output model cannot be 
#' differentiated between without explicitly saving the model before performing 
#' this operation. We do not need to specify an output to the bt_fit_model function 
#' as the function changes the input model in place. If you do decide to explicitly assign a function output,
#' be aware that the output model and the input model will be the same as one another.
#' 
#' @details This function updates the model so that the topics in the topics_to_merge list
#' become 1 topic. The grouped topics take the topic representation (Name) of the 
#' first topic in the list. Any number of topics can be merged together, if you would
#' like to merge two separate groups of topics, you must pass a list of lists/vectors as
#' topics_to_merge eg. list(c(1L, 3L), c(0L, 6L, 7L)) or list(list(1L, 3L), list(0L, 6L, 7L))
#'
#' @param fitted_model Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.
#' @param documents documents which the model was fitted to
#' @param topics_to_merge list (or list of lists/vectors) of topics created by your bertopic model that you wish to merge. Topics should be given in numeric form.
#'
#' @return bertopic model with specified topics merged
#' @export
#' 
#' @examples \dontrun{
#' 
#' # merge two topics
#' bt_merge_topics(fitted_model = model, documents = documents, topics_to_merge = list(1L, 2L))
#' 
#' # merge three topics
#' bt_merge_topics(fitted_model = model, documents = documents, topics_to_merge = list(1L, 2L, 3L))
#' 
#' # merge multiple sets of topics as a list of vectors
#' bt_merge_topics(fitted_model = model, documents = documents, topics_to_merge = list(c(1L, 3L), c(0L, 6L, 7L), c(2L, 12L)))
#' 
#' # merge multiple sets of topics as a list of lists
#' bt_merge_topics(fitted_model = model, documents = documents, topics_to_merge = list(list(1L, 3L), list(0L, 6L, 7L), list(2L, 12L)))
#' }
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
  
  message("\nTopics merged & input model updated accordingly")
}

#' Redistributes outliers using embeddings
#' 
#' @description Uses the cosine similarity of the document embeddings to find the
#' topic closest to each outlier document and reassigns these documents accordingly.
#' Note that the purpose of this function is to obtain a new list of topics that can then be used to 
#' update the model, it does not make any changes to the model itself, the topic 
#' classification the model outputs does not change after running this function. 
#' The bt_update_topics function needs to be used to make the change to the model 
#' itself. 
#' 
#' @details
#' It is possible to chain outlier reduction methods together as the operation works on
#' the list of topics input to the argument, which can vary. You will see in the examples
#' that we are able to perform one outlier reduction method, eg. bt_outliers_tokenset_similarity,
#' which will output a list of potential new topics, and input that list into another
#' outlier reduction method, eg. bt_outliers_embeddings, which will determine the output topic
#' suggestions based on the input list. In this way we can use aspects of multiple outlier
#' reduction strategies and chain them together.
#'
#' @param fitted_model Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.
#' @param documents documents to which the model was fit
#' @param topics current topics associated with the documents
#' @param embeddings embeddings used to create topics.
#' @param embedding_model If you did not instantiate the model with an embedding model you will need to pass one here
#' @param threshold minimum probability for outlier to be reassigned
#'
#' @return df with document, old topic, new topic
#' @export
#' 
#' @examples \dontrun{
#' # Reducing outliers original clustering model identified
#' outliers <- bt_outliers_embeddings(fitted_model = topic_model, documents = docs, topics = topic_model$topics_, embeddings = embeddings)
#' 
#' # Using chain strategies to build on outliers identified by another reduction strategy to redistribute outlier docs
#' # using tokenset similarity to redistribute outliers
#' outliers_ts <- bt_outliers_tokenset_similarity(fitted_model = topic_model, documents = docs, topics = topic_model$topics_)
#' 
#' # using embedding outlier reduction method on top of tokenset similarity method to redistribute outliers
#' outliers_chain <- bt_outliers_embeddings(fitted_model = topic_model, documents = docs, topics = outliers_ts$new_topics, embeddings = embeddings)
#' 
#' }
#'
bt_outliers_embeddings <- function(fitted_model,
                                   documents,
                                   topics,
                                   embeddings,
                                   embedding_model = NULL,
                                   threshold = 0.3){
  #Input validation
  
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
  
  # is there an embedding model specified?
  if (is.null(fitted_model$embedding_model$embedding_model) & is.null(embedding_model)){
    stop(fitted_model, "\nembedding model is NULL and no embedding_model passed to function. Specify embedding_model in function input.")
  }
  
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
  
  # get outlier embeddings ----
  outliers_index <- which(topics == -1)
  outlier_embeddings <- embeddings[outliers_index,]
  
  # calculate topic embeddings ----
  topic_embeddings <- list()
  
  for (topic in sort(unique(topics))){
    if (topic != -1){
      index <- which(topics == topic) # index of topic
      embeddings_segmented = embeddings[index, ]
      topic_embeddings[[topic + 1]] = colMeans(embeddings_segmented)
    }
  }
  
  topic_embeddings_matrix <- do.call(rbind, topic_embeddings)
  
  # find cosine similarity ----
  sklearn <- reticulate::import("sklearn")
  sim_matrix <- sklearn$metrics$pairwise$cosine_similarity(outlier_embeddings, topic_embeddings_matrix)
  
  # update topics ----
  sim_matrix[sim_matrix < threshold] <- 0 # set anything below threshold to 0
  new_topic_list <- list()
  for (i in 1:dim(sim_matrix)[1]){
    if (sum(sim_matrix[i,]) > 0){
      index <- which.max(sim_matrix[i,])
      new_topic_list[[i]] <- index - 1 # topic numbers start at 0
    }
    else {new_topic_list[[i]] <- -1}
  }
  
  new_topics <- topics # create new_topics list
  new_topics[new_topics == -1] <- unlist(new_topic_list) # update outliers topics in new_topics list
  
  new_topics_matched <- data.frame(message = documents,
                                   current_topics = topics,
                                   new_topics = new_topics)
  
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
#' @details
#' It is possible to chain outlier reduction methods together as the operation works on
#' the list of topics input to the argument, which can vary. You will see in the examples
#' that we are able to perform one outlier reduction method, eg. bt_outliers_embeddings,
#' which will output a list of potential new topics, and input that list into another
#' outlier reduction method, eg. bt_outliers_tokenset_similarity, which will determine the output topic
#' suggestions based on the input list. In this way we can use aspects of multiple outlier
#' reduction strategies and chain them together.
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
#' @examples
#' \dontrun{
#' # Reducing outliers original clustering model identified
#' outliers <- bt_outliers_tokenset_similarity(fitted_model = topic_model, documents = docs, topics = topic_model$topics_)
#' 
#' # Using chain strategies to build on outliers identified by another reduction strategy to redistribute outlier docs
#' # using embeddings to redistribute outliers
#' outliers_embed <- bt_outliers_embedings(fitted_model = topic_model, documents = docs, topics = topic_model$topics_, embeddings = embeddings, threshold = 0.5)
#' 
#' # using tokenset similarity outlier reduction method on top of embeddings method to redistribute outliers
#' outliers_chain <- bt_outliers_tokenset_similarity(fitted_model = topic_model, documents = docs, topics = outliers_embed$new_topics, threshold = 0.2)
#' 
#' }
#'
#'
bt_outliers_tokenset_similarity <- function(fitted_model,
                                            documents,
                                            topics,
                                            ...,
                                            window = 4L,
                                            stride = 1L,
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
  
  # test ellipses inputs
  dots <- rlang::list2(...) # place ellipses in list
  
  inspect <- reticulate::import("inspect")
  empty_model <- fitted_model$approximate_distribution
  available_args <- inspect$getfullargspec(empty_model)$args
  
  if(any(!names(dots) %in% available_args)){
    
    bad_args <- names(dots)[!names(dots) %in% names(empty_model)]
    stop(paste("Bad argument(s) attempted to be sent to fitted_model.approximate_distribution():", bad_args, sep = ' '))
  }
  ## validation finished ##
  
  
  # Convert window and stride to integers
  window_int <- as.integer(window)
  stride_int <- as.integer(stride)
  
  dist_params_dict <- reticulate::py_dict(
    keys = c("window", "stride",
             names(dots)), 
    values = c(window, stride,
               unlist(dots, use.names = FALSE)), 
    convert = TRUE) # dict for approx_distribution
  
  new_topics <- fitted_model$reduce_outliers(documents = documents,
                                             topics = topics,
                                             strategy = "distributions",
                                             threshold = threshold,
                                             distributions_params = dist_params_dict) 
  
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
#' @details
#' It is possible to chain outlier reduction methods together as the operation works on
#' the list of topics input to the argument, which can vary. You will see in the examples
#' that we are able to perform one outlier reduction method, eg. bt_outliers_embeddings,
#' which will output a list of potential new topics, and input that list into another
#' outlier reduction method, eg. bt_outliers_ctfidf, which will determine the output topic
#' suggestions based on the input list. In this way we can use aspects of multiple outlier
#' reduction strategies and chain them together.
#'
#' @param fitted_model Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.
#' @param documents documents to which the model was fit
#' @param topics current topics associated with the documents
#' @param threshold minimum probability for outlier to be reassigned
#'
#' @return df with document, old topic, new topic
#' @export
#' 
#' @examples
#' \dontrun{
#' # Reducing outliers original clustering model identified
#' outliers <- bt_outliers_ctfidf(fitted_model = topic_model, documents = docs, topics = topic_model$topics_)
#' 
#' # Using chain strategies to build on outliers identified by another reduction strategy to redistribute outlier docs
#' # using embeddings to redistribute outliers
#' outliers_embed <- bt_outliers_embedings(fitted_model = topic_model, documents = docs, topics = topic_model$topics_)
#' 
#' # using ctfidf outlier reduction method on top of embeddings method to redistribute outliers
#' outliers_chain <- bt_outliers_ctfidf(fitted_model = topic_model, documents = docs, topics = outliers_embed$new_topics, threshold = 0.2)
#' 
#' }
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
                                             threshold = threshold) 
  
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
#' this operation. We do not need to specify an output to the bt_fit_model function 
#' as the function changes the input model in place. If you do decide to explicitly assign a function output,
#' be aware that the output model and the input model will be the same as one another.
#' 
#' @details
#' NOTE: If using this function to update outlier topics, it may lead to errors 
#' if topic reduction or topic merging techniques are used afterwards. The reason 
#' for this is that when you assign a -1 document to topic 1 and another -1 document 
#' to topic 2, it is unclear how you map the -1 documents. Is it matched to topic 1 or 2.
#'
#' @param fitted_model Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.
#' @param documents documents to which the model was fit
#' @param new_topics Topics to update model with
#' @param representation_model model for updating topic representations
#' @param vectoriser_model Model for vectorising input for topic representations (Python object)
#' @param ctfidf_model Model for performing class-based tf-idf (ctf-idf) (Python object)
#'
#' @return the updated model
#' @export
#'
#' @examples  \dontrun{
#' # update model with new topic distribution
#' # reduce outliers
#' outliers <- bt_outliers_ctfidf(fitted_model = topic_model, documents = docs, threshold = 0.2)
#' 
#' # update the model with the new topic distribution
#' bt_update_topics(fitted_model = topic_model, documents = docs, new_topics = outliers$new_topics)
#' 
#' # update topic representation
#' bt_update_topics(fitted_model = topic_model, documents = docs, vectoriser_model = update_vec_model)
#' }
bt_update_topics <- function(fitted_model, 
                             documents, 
                             new_topics = NULL,
                             representation_model = NULL,
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
  
  new_topics_int <- as.integer(new_topics)
  fitted_model$update_topics(docs = documents, 
                             topics = new_topics_int, 
                             representation_model = representation_model,
                             vectorizer_model = vectoriser_model, 
                             ctfidf_model = ctfidf_model)
  
  message("\nInput model updated")
  
}