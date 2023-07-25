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
#' @param nr_docs number of representative docs to be examined for key words per cluster
#' @param nr_samples number of samples to select representative docs from for each cluster
#' @param nr_candidate_words number of words to examine per topic
#' @param random_state random state for sampling docs
#'
#' @return KeyBERTInspired representation model
#' @export
#'
bt_representation_keybert <- function(top_n_words = 10,
                                      nr_docs = 50,
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
  
  bt_rep <- reticulate::import("bertopic.representation") # import library
  
  representation_model <- bt_rep$KeyBERTInspired(top_n_words = as.integer(top_n_words),
                                                 nr_repr_docs = as.integer(nr_docs),
                                                 nr_samples = as.integer(nr_samples),
                                                 nr_candidate_words = as.integer(nr_candidate_words),
                                                 random_state = as.integer(random_state))
  
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
                                                          top_n_words = top_n_words)
  
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
  
  return(representation_model)

}

bt_representation_hf <- function(...,
                                 task = "text-generation",
                                 model = "gpt2",
                                 prompt = NULL,
                                 nr_docs = 10,
                                 diversity = NULL){
  
  # There is an error in this function: AttributeError:'TextGeneration' object has no attribute 'random_state'
  # This does not prevent the use of the function, only the ability to set the random state 
  
  #### Input Validation ####
  stopifnot(is.character(task),
            is.character(model),
            is.character(prompt) | is.null(prompt),
            is.numeric(nr_docs),
            is.numeric(diversity) | is.null(diversity))
  
  transformers <- reticulate::import("transformers")
  # empty_model <- transformers$pipeline()
  
  # gather extra arguments for input to py_dict
  dots <- rlang::list2(...) # place ellipses in list
  
  dots_unlist <- c() # empty vec
  for (i in dots){
    if (is.numeric(i)){
      i = as.integer(i)
    }
    dots_unlist <- append(dots_unlist, i) # place ellipses in vec
  } 
  
  # import modules
  bt_rep <- reticulate::import("bertopic.representation")
  
  generator <- transformers$pipeline(task = task, model = model)
  representation_model <- reticulate::py_suppress_warnings(bt_rep$TextGeneration(model = generator, 
                                                prompt = prompt, 
                                                nr_docs = as.integer(nr_docs), 
                                                diversity = as.integer(diversity),
                                                pipeline_kwargs = 
                                                  reticulate::py_dict(
                                                    keys = c(names(dots_unlist)), 
                                                    values = c(dots_unlist), 
                                                    convert = TRUE)))
}
