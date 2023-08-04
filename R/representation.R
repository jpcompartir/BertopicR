#' Create representation model using keybert
#'
#' @description
#' This creates a representation model based on the KeyBERT algorithm. This model
#' can be used to update topic representation using the bt_update_topics function.
#' 
#' @details
#' KeyBERT is a python package that is used for extraction of key words from 
#' documents. This model could be paired with a vectoriser model to adjust the 
#' ngram range of the key words / terms.
#' 
#' @param top_n_words number of keywords/phrases to be extracted
#' @param nr_repr_docs number of representative docs to be examined for key words per cluster
#' @param nr_samples number of samples to select representative docs from for each cluster
#' @param nr_candidate_words number of words to examine per topic
#' @param random_state random state for sampling docs
#'
#' @return KeyBERTInspired representation model
#' @export
#'
bt_representation_keybert <- function(fitted_model,
                                      documents,
                                      top_n_words = 10,
                                      nr_repr_docs = 50,
                                      nr_samples = 500,
                                      nr_candidate_words = 100,
                                      random_state = 42){
  
  #### input validation ####
  stopifnot(is.numeric(top_n_words),
            is.numeric(nr_docs),
            is.numeric(nr_samples),
            is.numeric(nr_candidate_words),
            is.numeric(random_state))
  #### end validation ####
  
  # bt_rep <- reticulate::import("bertopic.representation") # import library
  # 
  # representation_model <- bt_rep$KeyBERTInspired(top_n_words = as.integer(top_n_words),
  #                                                nr_repr_docs = as.integer(nr_docs),
  #                                                nr_samples = as.integer(nr_samples),
  #                                                nr_candidate_words = as.integer(nr_candidate_words),
  #                                                random_state = as.integer(random_state))
  
  # first we should get the representative docs ----
  # pandas df with Topic and Document cols for input to _extract_representative_docs()
  docs_df <- data.frame(Document = documents,
                     Topic = fitted_model$get_document_info(documents)$Topic) %>% reticulate::r_to_py()  
  
  c_tf_idf <- reticulate::py_eval("r.fitted_model.c_tf_idf_") # ctfidf for input to _extract_representative_docs
  docs_ind <- list()  # initiate list variable for multiple outputs from _extract_representative_docs
  nr_samples <- as.integer(nr_samples) # integer
  nr_repr_docs <- as.integer(nr_repr_docs) # integer
  docs_ind <- reticulate::py_eval("r.fitted_model._extract_representative_docs(r.c_tf_idf, r.docs_df, r.fitted_model.topic_representations_, r.nr_samples, r.nr_repr_docs)")
  
  # assign correct list components
  representative_docs <- docs_ind[[2]]
  repre_doc_indices <- docs_inds[[3]]
  
  # now we extract the candidate words from each topic ----
  bt <- reticulate::import("bertopic")
  candidate_words <- reticulate::py_eval("r.bt.representation.KeyBERTInspired._extract_candidate_words(topic_model = r.fitted_model, c_tf_idf = r.c_tf_idf, topics = r.fitted_model.topic_representations_)")
  
  
  return(representation_model)
}

#' Create representation model using Maximal Marginal Relevance 
#' 
#' @description
#' Calculates the maximal marginal relevance between candidate words and documents.
#' Considers similarity between keywords and phrases and already selected keywords 
#' and phrases and chooses representation based on this to maximise diversity.
#' 
#'
#' @param diversity  How diverse representation words/phrases are. 0 = not diverse, 1 = completely diverse
#' @param top_n_words Number of keywords/phrases to be extracted
#'
#' @return MaximalMarginalRelevance representation model
#' @export
#'
bt_representation_mmr <- function(diversity = 0.1,
                                  top_n_words = 10){
  
  #### input validation ####
  
  stopifnot(is.numeric(diversity), diversity >= 0, diversity <= 1,
            is.numeric(top_n_words))
  
  #### end validation ####
  
  bt_rep <- reticulate::import("bertopic.representation") # import library
  
  representation_model <- bt_rep$MaximalMarginalRelevance(diversity = diversity,
                                                          top_n_words = as.integer(top_n_words))
  
  return(representation_model)
  
}

#' Create representation model that uses OpenAI text generation models
#' 
#' @description
#' Uses the OpenAI API to generate topic labels based on one of their Completion
#' (chat = FALSE) or ChatCompletion (chat = TRUE) models.
#' 
#'
#' @param ... Sent to bertopic.representation OpenAI() function for adding additional arguments
#' @param openai_model openai model to use. If using a gpt-3.5 model, set chat = TRUE
#' @param nr_docs The number of documents to pass to OpenAI if a prompt with the \["DOCUMENTS"\] tag is used.
#' @param api_key openai ai api authentication key. This can be found on your openai account.
#' @param exponential_backoff Retry requests with a random exponential backoff. 
#' A short sleep is used when a rate limit error is hit, then the requests is retried. 
#' Increase the sleep length if errors are hit until 10 unsuccessful requests. 
#' If True, overrides delay_in_seconds.
#' @param chat set to TRUE if using gpt-3.5 model
#' @param delay_in_seconds The delay in seconds between consecutive prompts, this is to avoid rate limit errors.
#' @param prompt The prompt to be used with the openai model. If NULL, the default prompt is used.
#'
#' @return OpenAI representation model
#' @export
#'
bt_representation_openai <- function(...,
                                     openai_model = "text-ada-001",
                                     nr_docs = 10,
                                     api_key = "sk-",
                                     exponential_backoff = FALSE,
                                     chat = FALSE,
                                     delay_in_seconds = NULL,
                                     prompt = NULL){
  
  #### input validation ####
  
  stopifnot(is.character(openai_model),
            is.logical(chat),
            is.numeric(nr_docs),
            stringr::str_detect(api_key, "^sk-"),
            is.logical(exponential_backoff),
            is.numeric(delay_in_seconds) | is.null(delay_in_seconds),
            is.character(prompt) | is.null(prompt))
  
  # if using gpt model, must specify chat = TRUE
  if (chat == FALSE & stringr::str_detect(openai_model, "^gpt-")){
    stop("If using a gpt model, you must specify chat = TRUE")
  }
  
  # get list of available openai models
  openai <- reticulate::import("openai")
  openai$api_key <- api_key
  openai_models <- openai$Model$list()$data %>% # list of all available openai models
    lapply(function(sublist) sublist$id) %>%
    unlist()
  
  # Is the input model available?
  if (!openai_model %in% openai_models){
    stop("The input model, ", openai_model, ", is not an available OpenAI model.")
  }
  
  #Stop function early if bad arguments fed with ellipsis and send message to user pointing out which arguments were bad
  bt_rep <- reticulate::import("bertopic.representation")
  empty_model <- bt_rep$OpenAI() # empty model
  dots <- rlang::list2(...) # put extra args into list
  
  # check extra arguments available to OpenAI function
  if(any(!names(dots) %in% names(empty_model))){
    bad_args <- names(dots)[!names(dots) %in% names(empty_model)]
    stop(paste("Bad argument(s) attempted to be sent to OpenAI():", bad_args, sep = ' '))
  }
  
  #### end of input validation ####

  representation_model <- bt_rep$OpenAI(model = openai_model, 
                                        chat = chat,
                                        nr_docs = as.integer(nr_docs),
                                        prompt = prompt,
                                        delay_in_seconds = as.integer(delay_in_seconds),
                                        exponential_backoff = as.integer(exponential_backoff),
                                        ...)
  
  representation_model2 <- representation_model
  
  return(representation_model)

}



#' Use Huggingface models to create topic representation
#'
#' @details
#' Representative documents are chosen from each topic by sampling (nr_samples)
#' a number of documents from the topic and calculating which of those documents are
#' most representative of the topic by cosine similarity between the topic and the
#' individual documents. From this the most representative documents (the number
#' is defined by the nr_repr_docs parameter) is extracted and passed to the huggingface
#' model and topic description predicted. 
#'
#' @param ... arguments sent to the transformers.pipeline function
#' @param task Task defining the pipeline that will be returned. See https://huggingface.co/transformers/v3.0.2/main_classes/pipelines.html for more information. Use "text-generation" for gpt-like models and "text2text-generation" for T5-like models
#' @param hf_model The model that will be used by the pipeline to make predictions 
#' @param topic_model The fitted bertopic model
#' @param documents the documents the topic model was fitted to
#' @param default_prompt Whether to use the "keywords" or "documents" default prompt. Passing a custom_prompt will render this argument NULL. Default is "keywords" prompt.
#' @param custom_prompt The custom prompt to be used in the pipeline. If not specified, the "keywords" or "documents" default_prompt will be used. Use "\[KEYWORDS\]" and "\[DOCUMENTS\]" in the prompt to decide where the keywords and documents are inserted.
#' @param nr_samples When choosing representative documents to be sent to 
#' @param nr_repr_docs Number of representative documents to be sent to the huggingface model
#' @param diversity diversity of documents to be sent to the huggingface model. 0 = no diversity, 1 = max diversity. 

#'
#' @return updated representation of each topic
#' @export
#'
bt_representation_hf <- function(...,
                                 task,
                                 hf_model,
                                 topic_model,
                                 documents,
                                 default_prompt = "keywords",
                                 custom_prompt = NULL,
                                 nr_samples = 500,
                                 nr_repr_docs = 20,
                                 diversity = 10){

  #### Validation ####
  stopifnot(!is.null(default_prompt) | !is.null(custom_prompt),
            is.character(task),
            is.character(hf_model),
            is.character(documents),
            is.character(default_prompt) | is.null(default_prompt),
            is.character(custom_prompt) | is.null(custom_prompt),
            is.numeric(nr_samples),
            is.numeric(nr_repr_docs),
            is.numeric(diversity))

  # Check fitted model
  if(!is.null(topic_model)) {
    test_is_fitted_model(topic_model)
  }

  # check extra arguments passed
  transformer <- reticulate::import("transformers") # import transformers library
  pipeline_args <- args(transformer$pipeline) %>% as.list() %>% names()
  
  #Convert the `...` (dot-dot-dot or ellipsis) to list for checking purposes
  dots <- rlang::list2(...)
  
  #Stop function early if bad arguments fed with ellipsis and send message to user pointing out which arguments were bad
  if(any(!names(dots) %in% pipeline_args)){
    
    bad_args <- names(dots)[!names(dots) %in% pipeline_args]
    stop(paste("Bad argument(s) attempted to be sent to pipeline():", bad_args, sep = ' '))
  }
  
  #### end of validation ####

  # Set prompt to use
  if (!is.null(custom_prompt)){
    prompt <- custom_prompt
  }
  else if (default_prompt == "keywords"){
    prompt <- "I have a topic described by the following keywords: [KEYWORDS]. The name of this topic is:"
  }
  else if (default_prompt == "documents"){
    prompt <- "I have a topic described by the following documents: [DOCUMENTS]. The name of this topic is:"
  }

  # create pipeline
  generator <- transformer$pipeline(task = task, model = hf_model, ...) # create pipeline

  empty_string <- ""
  updated_representation <- list()
  # record_prompt <- list()
  for (doc in seq_along(topic_model$get_topic_info()$Topic)) {
    
    updated_prompt <- prompt # don't want to overwrite prompt
    
    if (stringr::str_detect(prompt, "\\[KEYWORDS\\]")){
      
      # representative keywords
      topic_representation <- topic_model$topic_representations_
      
      # concatenate keywords into single string per topic
      keywords <- list()
      # words_per_topic <- list()
      for (topic in seq_along(topic_representation)){
        topic_list <- topic_representation[[topic]]
        # words_per_topic <- character()
        words_per_topic <- NULL
        for (word_info in seq_along(topic_list)){
          word_list <- topic_list[[word_info]][[1]]
          words_per_topic <- c(words_per_topic, word_list)
          keywords[[topic]] <- paste(words_per_topic, collapse = ", ")
          
        }
      }
      
      updated_prompt <- gsub("\\[KEYWORDS\\]", keywords[[doc]], updated_prompt)
    }
    
    if (stringr::str_detect(prompt, "\\[DOCUMENTS\\]")){
      
      # first get representative docs and keywords
      c_tf_idf <- reticulate::py_eval("r.topic_model.c_tf_idf_") # ctfidf for input to _extract_representative_docs
      
      # _extract_representative_docs requires df with Document and Topic columns
      docs <- data.frame(Document = documents,
                         Topic = topic_model$get_document_info(documents)$Topic) %>% reticulate::r_to_py() # pandas df with Topic and Document cols
      
      # convert to integers
      nr_samples <- as.integer(nr_samples)
      nr_repr_docs <- as.integer(nr_repr_docs)
      diversity = as.integer(diversity)
      
      # representative docs
      # set environment so that py_eval can access local variables
      withr::with_options(c(reticulate.engine.environment = rlang::current_env()), {
        
        # get representative docs
        representative_docs <- reticulate::py_eval("r.topic_model._extract_representative_docs(r.c_tf_idf, r.docs, r.topic_model.topic_representations_, r.nr_samples, r.nr_repr_docs, r.diversity)")[[1]]
      })
      format_docs <- paste(empty_string, paste0("- ", substr(representative_docs[[doc]], 1, 255)), "\n")
      join_docs <- paste(format_docs, collapse = "")

      updated_prompt <- gsub("\\[DOCUMENTS\\]", join_docs, updated_prompt)
    }
    
    updated_representation[[doc]] <- generator(updated_prompt)[[1]][[1]] 
    # record_prompt[[doc]] <- updated_prompt

  }
  
  return(updated_representation)

   # return(list("updated_representation" = updated_representation, "prompts" = record_prompt))

  }
  

