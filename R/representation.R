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
  
  representation_model2 <- representation_model
  
  return(representation_model)

}

# create class ----
pyclass_test <- reticulate::PyClass("TextGeneration",
                                    defs = list(
                                      transformers = NULL,
                                      model = NULL,
                                      prompt = NULL,
                                      pipeline_kwargs = NULL,
                                      nr_docs = NULL,
                                      diversity = NULL,
                                      # "_create_prompt" = NULL,
                                      '__init__' = function(self,
                                                            model = "google/flan-t5-base",
                                                            prompt = NULL,
                                                            pipeline_kwargs = list(),
                                                            # random_state = 42,
                                                            nr_docs = 4,
                                                            diversity = NULL) {
                                        DEFAULT_PROMPT <- "I have a topic described by the following keywords: [KEYWORDS]. The name of this topic is:"
                                        transformers <- reticulate::import("transformers", convert = FALSE)
                                        if (is.character(model)){
                                          self$model <- transformers$pipeline("text2text-generation", model = model)
                                        }
                                        else if (any(grepl("pipelines", class(model)))){
                                          self$model <- model
                                        }
                                        else {
                                          stop("Make sure that the HF model that youpass is either a string referring to a HF model or a `transformers.pipeline` object.")
                                        }
                                        if (!is.null(prompt)) {
                                          self$prompt <- prompt
                                        }
                                        else {
                                          self$prompt <- DEFAULT_PROMPT
                                        }
                                        self$default_prompt_ <- DEFAULT_PROMPT
                                        self$pipeline_kwargs <- pipeline_kwargs
                                        self$nr_docs <- nr_docs
                                        self$diversity <- diversity
                                      },
                                      "_create_prompt" = function(){
                                        # self$"_create_prompt" <- reticulate::py_eval("r.bt.representation.TextGeneration._create_prompt")
                                      }))



# bt_representation_hf <- function(task = "text_generation",
#                                  model = "gpt2"){
# 
# 
#   np <- reticulate::import("numpy")
#   transformers <- reticulate::import("transformers")
#   generator <- transformers$pipeline(task = task, model = model)
#   bt <- reticulate::import("bertopic")
#   c_tf_idf = model$c_tf_idf_
# 
# 
#   top_n_words <- max(model$top_n_words, 30) %>% as.integer()
#   indices <- reticulate::py_eval("r.bt.BERTopic._top_n_idx_sparse(r.c_tf_idf, r.top_n_words)")
#   scores <- reticulate::py_eval("r.bt.BERTopic._top_n_values_sparse(r.c_tf_idf, r.indices)")
#   sorted_indices <- np$argsort(scores, 1L)
#   indices <- reticulate::r_to_py(indices)
#   sorted_indices <- reticulate::r_to_py(sorted_indices)
#   scores <- reticulate::r_to_py(scores)
#   indices <- np$take_along_axis(indices, sorted_indices$astype("int"), axis=1L)
#   scores <- np$take_along_axis(scores, sorted_indices$astype("int"), axis=1L)
# 
#   labels <- model$get_topic_info()$Name %>% unique()
#   docs <- sentences
#   pd <- reticulate::import("pandas")
#   documents <- reticulate::py_eval("r.pd.DataFrame({\"Document\": r.docs, \"Topic\": r.model.get_document_info(r.docs).Topic})")
#   documents <- reticulate::r_to_py(documents)
#   documents_per_topic = reticulate::py_eval("r.documents.groupby(['Topic'], as_index=False).agg({'Document': ' '.join})")
#   words <- reticulate::py_eval("r.model._c_tf_idf(r.documents_per_topic)")[[2]]
# 
#   topics <- reticulate::py_eval("{label: [(r.words[word_index], score)
#                     if word_index is not None and score > 0
#                     else (\"\", 0.00001)
#                     for word_index, score in zip(r.indices[index][::-1], r.scores[index][::-1])]
#     for index, label in enumerate(r.labels)}")
# 
#   # if I can't get this working I will try the next line instead
#   # representative_docs <- reticulate::py_run_string("r.bt.BERTopic._extract_representative_docs(r.c_tf_idf")
#   create_prompt <- reticulate::py_run_string("r.bt.representation.TextGeneration._create_prompt")
#   bt$representation$TextGeneration$extract_topics(self = test,
#                                                   topic_model = model,
#                                                   documents = sentences,
#                                                   c_tf_idf = c_tf_idf,
#                                                   topics = topics)
# 
# }

# New strategy is to get representative docs and send these to hf independently

bt_representation_hf <- function(task,
                                 hf_model,
                                 topic_model,
                                 default_prompt = "keywords",
                                 custom_prompt = NULL,
                                 nr_samples = 100,
                                 nr_repr_docs = 2,
                                 diversity = 10,
                                 ...){
  
  #### Validation ####
  stopifnot(!is.null(default_prompt) | !is.null(custom_prompt),
            is.str(task),
            is.str(hf_model),
            is.str(custom_prompt) | is.null(custom_prompt))
  
  # Check fitted model
  if(!is.null(topic_model)) {
    test_is_fitted_model(topic_model)
  }
  
  #### end of validation ####
  
  # Set prompt to use
  if (custom_prompt){
    prompt <- custom_prompt
  }
  else if (default_prompt == "keywords"){
    prompt <- "I have a topic described by the following keywords: [KEYWORDS]. The name of this topic is:"
  }
  else if (default_prompt == "documents"){
    prompt <- "I have a topic described by the following documents: [DOCUMENTS]. The name of this topic is:"
  }

  
  
  # labels <- model$get_topic_info()$Name %>% unique()
  # docs <- sentences
 
  # first get representative docs and keywords
  c_tf_idf <- reticulate::py_eval("r.topic_model.c_tf_idf_") # ctfidf for input to _extract_representative_docs
  
  docs <- data.frame(Document = sentences,
                     Topic = topic_model$get_document_info(sentences)$Topic) %>% reticulate::r_to_py() # pandas df with Topic and Document cols
  
  # representative docs
  representative_docs <- reticulate::py_eval("r.topic_model._extract_representative_docs(r.c_tf_idf, r.docs, r.model.topic_representations_, r.nr_samples, r.nr_repr_docs, r.diversity)")[[1]]
  # representative keywords
  topic_representation <- topic_model$topic_representations_
  
  # concatenate keywords into single string per topic
  keywords <- list()
  for (topic in seq_along(topic_representation)){
    topic_list <- topic_representation[[topic]]
    for (word_info in seq_along(topic_list)){
      word_list <- topic_list[[word_info]][[1]]
      keywords[[topic]] <- paste(word_list, collapse = ", ")
    }
  }
  
  # create pipeline 
  transformer <- reticulate::import("transformers") # import transformers library
  generator <- transformer$pipeline(task = task, model = hf_model) # create pipeline
  
  # updated_representation <- list()
  # updated_prompt <- list()
  # empty_string <- ""
  # format_docs <- list()
  # join_docs <- list()
  
  # # add in something about custom prompts
  # if (prompt == "keywords"){
  #   for (topic in seq_along(keywords)){
  #     updated_prompt <- gsub("\\[KEYWORDS\\]", keywords[[topic]], prompt_keywords)
  #     updated_representation[[topic]] <- generator(updated_prompt) # add pipeline_kwargs here
  #   }
  # }
  # else if (prompt == "documents"){
  #   for (doc in seq_along(representative_docs)){
  #     format_docs <- paste(empty_string, paste0("- ", substr(representative_docs[[doc]], 1, 255)), "\n")
  #     join_docs <- paste(format_docs, collapse = "")
  #     
  #     updated_prompt <- gsub("\\[DOCUMENTS\\]", join_docs, prompt_documents)
  #     updated_representation[[doc]] <- generator(updated_prompt)[[1]][[1]] # add pipeline_kwargs here
  #   }
  # }
  # 
  
  empty_string <- ""
  updated_representation <- list()
  record_prompt <- list()
  for (doc in seq_along(representative_docs)) {
    use_prompt <- prompt
    if (stringr::str_detect(prompt, "\\[KEYWORDS\\]")){
      use_prompt <- gsub("\\[KEYWORDS\\]", keywords[[doc]], use_prompt)
    }
    if (stringr::str_detect(prompt, "\\[DOCUMENTS\\]")){
      format_docs <- paste(empty_string, paste0("- ", substr(representative_docs[[doc]], 1, 255)), "\n")
      join_docs <- paste(format_docs, collapse = "")
      
      use_prompt <- gsub("\\[DOCUMENTS\\]", join_docs, use_prompt)
    }
    updated_representation[[doc]] <- generator(updated_prompt)[[1]][[1]] # add pipeline_kwargs here
    record_prompt[[doc]] <- use_prompt
    
  }
  
   return(updated_representation, record_prompt) 
    
  }
  



# bt_representation_hf <- function(...,
#                                  task = "text-generation",
#                                  model = "gpt2",
#                                  prompt = NULL,
#                                  nr_docs = 10,
#                                  diversity = NULL){
#   
#   # There is an error in this function: AttributeError:'TextGeneration' object has no attribute 'random_state'
#   # This does not prevent the use of the function, only the ability to set the random state 
#   
#   #### Input Validation ####
#   stopifnot(is.character(task),
#             is.character(model),
#             is.character(prompt) | is.null(prompt),
#             is.numeric(nr_docs),
#             is.numeric(diversity) | is.null(diversity))
#   
#   transformers <- reticulate::import("transformers")
#   # empty_model <- transformers$pipeline()
#   inspect <- reticulate::import("inspect")
#   args <- inspect$signature(transformers$pipeline) %>% 
#     as.character() %>%
#     stringr::str_match_all("\\w+(?=:)") %>% 
#     sapply(function(x) x[,1])
#   
#   
#   # gather extra arguments for input to py_dict
#   dots <- rlang::list2(...) # place ellipses in list
#   
#   dots_unlist <- c() # empty vec
#   for (i in dots){
#     if (is.numeric(i)){
#       i = as.integer(i)
#     }
#     dots_unlist <- append(dots_unlist, i) # place ellipses in vec
#   } 
#   
#   # import modules
#   bt_rep <- reticulate::import("bertopic.representation")
#   
#   generator <- transformers$pipeline(task = task, model = model)
#   representation_model <- reticulate::py_suppress_warnings(bt_rep$TextGeneration(model = generator, 
#                                                 prompt = prompt, 
#                                                 nr_docs = as.integer(nr_docs), 
#                                                 diversity = as.integer(diversity),
#                                                 pipeline_kwargs = 
#                                                   reticulate::py_dict(
#                                                     keys = c(names(dots_unlist)), 
#                                                     values = c(dots_unlist), 
#                                                     convert = TRUE)))
# }

# library(R6)
# library(reticulate)
# 
# CustomTextGeneration <- R6Class(
#   classname = "CustomTextGeneration",
#   public = list(
#     transformers = NULL,
#     model = NULL,
#     prompt = NULL,
#     default_prompt_ = NULL,
#     pipeline_kwargs = NULL,
#     nr_docs = NULL,
#     diversity = NULL,
#     initialize = function(
#     model,
#     prompt = NULL,
#     pipeline_kwargs = list(),
#     # random_state = 42,
#     nr_docs = 4,
#     diversity = NULL
#     ) {
#       
#       transformers <- reticulate::import("transformers", convert = FALSE)  # Import Python module into the environment
#       
#       # python$set_seed(random_state)
#       if (is.character(model)) {
#         self$model <- transformers$pipeline("text-generation", model=model)
#       } else if (inherits(model, "PythonEnv")) {
#         self$model <- model
#       } else {
#         stop("Make sure that the HF model that you pass is either a string referring to a HF model or a `transformers.pipeline` object.")
#       }
#       self$prompt <- if (!is.null(prompt)) prompt else "DEFAULT_PROMPT"
#       self$default_prompt_ <- "DEFAULT_PROMPT"
#       self$pipeline_kwargs <- pipeline_kwargs
#       self$nr_docs <- nr_docs
#       self$diversity <- diversity
#     }
#   )
# )
# 

# pyclass_test <- reticulate::PyClass("TextGeneration",
#                         defs = list(
#                           transformers = NULL,
#                           model = NULL,
#                           prompt = NULL,
#                           pipeline_kwargs = NULL,
#                           nr_docs = NULL,
#                           diversity = NULL,
#                           '__init__' = function(self,
#                                                 model = "google/flan-t5-base",
#                                                 prompt = NULL,
#                                                 pipeline_kwargs = list(),
#                                                 # random_state = 42,
#                                                 nr_docs = 4,
#                                                 diversity = NULL) {
#                             DEFAULT_PROMPT <- "I have a topic described by the following keywords: [KEYWORDS]. The name of this topic is:"
#                             transformers <- reticulate::import("transformers", convert = FALSE)
#                             if (is.character(model)){
#                               self$model <- transformers$pipeline("text2text-generation", model = model)
#                             }
#                             else if (any(grepl("pipelines", class(model)))){
#                               self$model <- model
#                             }
#                             else {
#                               stop("Make sure that the HF model that youpass is either a string referring to a HF model or a `transformers.pipeline` object.")
#                             }
#                             if (!is.null(prompt)) {
#                               self$prompt <- prompt
#                               }
#                             else {
#                               self$prompt <- DEFAULT_PROMPT
#                             }
#                             self$default_prompt_ <- DEFAULT_PROMPT
#                             self$pipeline_kwargs <- pipeline_kwargs
#                             self$nr_docs <- nr_docs
#                             self$diversity <- diversity
#                           }, 
#                           'extract_topics' = function(self,
#                                                       topic_model,
#                                                       documents,
#                                                       c_tf_idf,
#                                                       topics) {
#                             if (self$prompt != DEFAULT_PROMPT & "[DOCUMENTS]" %in% self$prompt){
#                               repr_docs_mappings <- topic_model$_extract_representative_docs(c_tf_idf,
#                                                                          documents,
#                                                                          topics,
#                                                                          500,
#                                                                          self$nr_docs,
#                                                                          self$diversity)[1]
#      
#                             }
#                             else {
#                               for (topic in names(topics)) {
#                                 repr_docs_mappings[[topic]] <- NULL
#                               }
#                             }
#                             
#                             updated_topics <- list()
#                             
#                             for (topic in topics$items()){
#                               prompt <- self$_create_prompt(repr_docs_mappings[topic], topic, topics)
#                               topic_description <- self$model(prompt)
#                               # topic_description <- [(description["generated_text"]$replace(prompt, ""), 1) for description in topic_description]
#                               process_description <- function(description) {
#                                 generated_text <- gsub(prompt, "", description$generated_text)
#                                 list(generated_text, 1)
#                               }
#                               topic_description <- lapply(topic_description, process_description)
#                               
#                               # if (length(topic_description) < 10){
#                               #   topic_description
#                               # }
#                             }
#                             updated_topics[topic] <- topic_description
#                             return(updated_topics)
#                           }),
#                         inherits(_create_prompt,
#                                  _extract_resentative_docs]))

    
    

  

# s3_test <- list(model = "gpt2", prompt = NULL, pipeline_kwargs = NULL, nr_docs = 4L, diversity = NULL)
# class(s3_test) <- "representation"


# test <- function(model,
#                  prompt = NULL,
#                  pipeline_kwargs = NULL,
#                  nr_docs = 4L,
#                  diversity = NULL){
#   
#   test_instance <- "
#   if isinstance(model, str):
#     self.model = pipeline(\"text-generation\", model=model)
# elif isinstance(model, Pipeline):
#     self.model = model
# else:
#     raise ValueError(\"Make sure that the HF model that you pass is either a string referring to a HF model or a `transformers.pipeline` object.\")
# In the corrected code, the if, elif, and else blocks are indented at the same level, and the code should work as intended. Make sure to use the correct indentation in your Python code to ensure proper execution and avoid syntax errors.
#   "
#   
#   set_params <- "self.prompt = prompt if prompt is not None else DEFAULT_PROMPT
#   self.default_prompt_ = DEFAULT_PROMPT
#   self.pipeline_kwargs = pipeline_kwargs
#   self.nr_docs = nr_docs
#   self.diversity = diversity"
#   
#   reticulate::py_run_string(test_instance)
#   reticulate::py_run_string(set_params)
#     
# }

# Define the S3 generic function
# # CustomTextGeneration <- function(model,
#                                  prompt = NULL, 
#                                  pipeline_kwargs = list(), 
#                                  nr_docs = 4L, 
#                                  diversity = NULL) {
#   UseMethod("CustomTextGeneration")
# }
# 
# # Define the S3 method for character class
# CustomTextGeneration.character <- function(model, prompt = NULL, pipeline_kwargs = list(), nr_docs = 4L, diversity = NULL) {
#   transformers <- reticulate::import("transformers")
#   model_obj <- transformers$pipeline("text-generation", model=model)
#   CustomTextGeneration(model_obj, prompt, pipeline_kwargs, random_state, nr_docs, diversity)
# }
# 
# # Define the S3 method for pipeline class
# CustomTextGeneration.Pipeline <- function(model, prompt = NULL, pipeline_kwargs = list(), random_state = 42, nr_docs = 4, diversity = NULL) {
#   self <- list()
#   class(self) <- "CustomTextGeneration"
#   
#   # Your init logic
#   set.seed(random_state)
#   self$model <- model
#   self$prompt <- if (!is.null(prompt)) prompt else "DEFAULT_PROMPT"
#   self$default_prompt_ <- "DEFAULT_PROMPT"
#   self$pipeline_kwargs <- pipeline_kwargs
#   self$nr_docs <- nr_docs
#   self$diversity <- diversity
#   
#   return(self)
# }
# 
# # Sample usage
# # Assuming you have defined the CustomTextGeneration S3 function as shown above
# # You can now use the S3 method with different types of model inputs:
# 
# # If the model is a character (string) representing the model name
# custom_generator <- CustomTextGeneration("gpt2")
# 
# # If the model is an existing Pipeline object
# model_path <- "path/to/your/model"  # Replace with the path to your HF model
# model <- py$transformers$AutoModelForCausalLM$from_pretrained(model_path)
# custom_generator <- CustomTextGeneration(model)
