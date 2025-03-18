test_is_python_object <- function(object){
  return(any(grepl("python\\.builtin\\.object", methods::S3Class(object))))
}

test_embeddings_dims <- function(documents, embeddings){
  if(!length(documents) == dim(embeddings)[[1]]) {
    stop(
      paste0("The dimensions of your documents and embeddings do not mach up.\nNumber of documents: ", length(documents),"\nNumber of embeddings: ",dim(embeddings)[[1]]))
  }
}

test_labels_lengths <- function(documents, topic_labels){
  if(!length(documents) == length(topic_labels)) {
    stop(
      paste0("The dimensions of your documents and topic_labels do not mach up.\nNumber of documents: ", length(documents),"\nNumber of topic labels: ",length(topic_labels)))
  }
}
  
test_is_fitted_model <- function(model) {
  if(!grepl("^bertopic", methods::S3Class(model)[[1]])){
    stop("Model should be a BERTopic model")
  }
    
  # if(!test_is_python_object(model)){
  #   stop("model is not a python object")
  # }

  if(is.null(model$topics_)){
    stop("BERTopic model is not fitted, use bt_fit_model to fit.")
  }

  return(TRUE)

}

convert_to_np_array <- function(x, ...){

  stopifnot(is.data.frame(x) | is.array(x))

  #Reticulate's function doesn't seem to be as flexible as this approach
  np <- reticulate::import("numpy")

  x <- np$array(x, ...)

  return(x)
}


update_prompt <- function(prompt,
                           repr_doc_mapping,
                          topic_keywords,
                           fitted_model){
  
  topic_docs <- repr_doc_mapping #
  updated_prompt <- prompt # don't want to overwrite prompt
  
  # Create the prompt ----
  if (stringr::str_detect(prompt, "\\[DOCUMENTS\\]")){
    format_topic_docs <- paste0("- ", substr(topic_docs, 1, 255))
    topic_docs_joined <- paste(format_topic_docs, collapse = "\n")
    updated_prompt <- gsub("\\[DOCUMENTS\\]", topic_docs_joined, updated_prompt)
  }
  
  if (stringr::str_detect(prompt, "\\[KEYWORDS\\]")){
    keywords_joined <- paste(topic_keywords, collapse = ", ")
    updated_prompt <- gsub("\\[KEYWORDS\\]", keywords_joined, updated_prompt)
  }
  return(updated_prompt)
  
}

openai_api_call <- function(updated_prompt,
                            delay_in_seconds,
                            chat,
                            openai_model,
                            api_key){
  
  openai <- reticulate::import("openai")
  openai$api_key <- api_key
  
  if (!is.null(delay_in_seconds)){
    time <- reticulate::import("time")
    time$sleep(delay_in_seconds)
  }
  
  if (chat){
    
    messages = list(
      list(role = "system", content = "You are a helpful assistant."),
      list(role = "user", content = updated_prompt)
    )
    
    # updated_representation <- openai::create_chat_completion(
    #   model = openai_model,
    #   messages = messages
    # )$choices$message.content
    
    updated_representation <- openai$ChatCompletion$create(
      model = openai_model,
      messages = messages
    )$choices[[1]]$message$content
    
    updated_representation <- stringr::str_remove_all(updated_representation, "^topic: ")
    
  } else{
    updated_representation <- openai$Completion$create(
      model = openai_model,
      prompt = updated_prompt[[1]]
    )$choices[[1]]$text
  }
  
  return(updated_representation)
}

stop_tokenizers_warning<- function(){
  Sys.setenv("TOKENIZERS_PARALLELISM" = "0")
  
  message("TOKENIZERS_PARALLELISM set to '0'")
  invisible()
}
