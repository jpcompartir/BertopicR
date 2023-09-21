#' Create an embedding model using [sentence-transformers](https://www.sbert.net/docs/pretrained_models.html)
#'
#' Initially this function is built upon the `sentence_transformers` Python library, but it may be expanded to accept other frameworks. You should feed in your documents as a list. You can use hardware accelerators e.g. GPUs, to speed up computation.
#'
#' @param model_name Name of embedding model as a string (not case sensitive)
#'
#' @return a Python object
#' @export
#'
#' @examples
#' embedder <- bt_make_embedder("all-mpnet-base-v2")
#'
#' embedder <- bt_make_embedder("aLL-minilm-l6-v2")
bt_make_embedder_st <- function(model_name) {
  
  #Can leave space for a second argument, which is model_source and then use switch() to allow for embedding models other than sentence_transformers if the need arises.
  
  if(!is.character(model_name)){
    stop("'model_name' should be a string of text e.g. 'all-mpnet-base-v2")
  }
  
  #Import sentence transformers to embed documents. In the future we might want to use an argument + switch to allow the user to use other platforms for embeddings.
  sentence_transformers <- reticulate::import("sentence_transformers")
  
  #Instantiate embedding model, pass ellipsis here
  embedder <- sentence_transformers$SentenceTransformer(model_name_or_path = model_name)
  
  attr(embedder, "embedding_model") <- model_name
  
  return(embedder)
}

#' Create an embedding model using a model available from the [Spacy Library](https://spacy.io/models)
#'
#' @param model The pipeline used to make predictions
#' @param ... additional arguments to be sent to the spacy.load function
#' @param exclude name of pipeline components to exclude
#'
#' @return a pipeline formed according to the model defined
#' @export
#'
#' @examples
#' # specify a non-transformer model, excluding features not required
#' embedder <- bt_make_embedder_spacy(model = "en_core_web_md", exclude = c("tagger", "parser", "ner", "attribute_ruler", "lemmatizer"))
#' 
#' # specify a transformer model and exclude features not required
#' embedder <- bt_make_embedder_spacy(model = "en_core_web_trf", exclude = c("tagger", "parser", "ner", "attribute_ruler", "lemmatizer"))
#' 
bt_make_embedder_spacy <- function(model, ..., exclude = NULL){
  
  # input argument validation ----
  # is spacy in installed packages?
  installed_packages <- reticulate::py_list_packages()
  
  if(!"spacy" %in% installed_packages[["package"]]){
    message("spacy is not in installed packages of current environment, run reticulate::py_install(\"spacy\").")
  } 
  
  stopifnot(is.character(model),
            is.null(exclude) | is.character(exclude) | is.list(exclude))
  # end validation ----
  
  spacy <- reticulate::import("spacy")
  spacy$prefer_gpu() 
  
  # if exclude is null, do not enter as argument as default is from spacy utils
  if (!is.null(exclude)){
    nlp <- try(spacy$load(name = model, exclude = exclude, ...)) 
  } else{
    nlp <- try(spacy$load(name = model, ...)) 
  }
  
  if(any(class(nlp) == "try-error")){
    spacy$cli$download(model)
    if (!is.null(exclude)){
      nlp <- try(spacy$load(name = model, exclude = exclude, ...)) 
    } else{
      nlp <- try(spacy$load(name = model, ...)) 
    }
  }
  
  return(nlp)
}

bt_make_embedder_flair <- function(){
  
  # input argument validation ----
  # is spacy in installed packages?
  installed_packages <- reticulate::py_list_packages(model)
  
  if(!"flair" %in% installed_packages[["package"]]){
    message("flair is not in installed packages of current environment, run reticulate::py_install(\"flair\").\n
            Note that if you receive a module not found error, you may need to instead run reticulate::py_install(\"flair\", pip = TRUE) to force installation with pip instead of conda.")
  } 
  
  
  flair <- reticulate::import("flair")
}
#' Embed your documents
#'
#' Takes a document, or list of documents, and returns a numerical embedding which can be used as features for machine learning model or for semantic similarity search. If you have pre-computed your embeddings you can skip this step. the bt_embed function is designed to be used as one step in a topic modelling pipeline.
#'
#' Initially this function is built upon the `sentence_transformers` Python library, but it may be expanded to accept other frameworks. You should feed in your documents as a list. You can use hardware accelerators e.g. GPUs, to speed up computation.
#'
#' The function currently returns an object with two additional attributes: embedding_model, n_documents, they have been appended to the embeddings for extraction at later steps in the pipeline, e.g. when merging data frames later on it's important to check how many documents we entered.
#'
#' @param embedder An embedding model (output of bt_make_embedder)
#' @param documents A character vector of the documents to be embedded, e.g. your text variable
#' @param ... Optional or additional parameters passed to SentenceTransformer's encode function, e.g. batch_size
#' @param accelerator A string containing the name of a hardware accelerator, e.g. "mps", "cuda". If NULL no acceleartor is used.
#' @param progress_bar A logical value indicating whether a progress bar is shown in the console
#'
#' @return An array of floating point numbers
#' @export
#' 
#' @examples
#' docs <- c("i am", "a list of", "documents", "to be embedded")
#' 
#' embedder <- bt_make_embedder("aLL-minilm-l6-v2")
#' 
#' embeddings <- bt_do_embedding(embedder, docs)
#' 
bt_do_embedding <- function(embedder, documents, ..., accelerator = "mps", progress_bar = TRUE) {
  
  # update this to be compatible with compatible embedding models
  # if(!grepl("^sentence_tran",class(embedder)[[1]])){
  #   stop("This package currently only supports embedding models from the sentence transformer library, embedder should be a sentence transformers model")
  # }
  
  #Store the attributes associated with the embedder for adding the embedding_model later
  embedder_attributes <- attributes(embedder)
  
  #Stop early if conditions aren't met
  stopifnot(is.character(accelerator) | is.null(accelerator),
            is.logical(progress_bar))
  
  #Create embeddings for sentence transformer
  if(grepl("^sentence_tran",class(embedder)[[1]])){
    embeddings <-
      embedder$encode(
        documents,
        device = accelerator,
        show_progress_bar = progress_bar,
        ... #This allows knowledgeable users to include other arguments, without bloating the autofill for inexperienced users
      )
  }
  else if(grepl("^spacy",class(embedder)[[1]])){
    
    embeddings <- c()
    
    for (doc in 1:500){
      
      embedding <- embedder(documents[doc])
      
      if (embedding$has_vector){
        embedding = embedding$vector
      }
      else{
        embedding = reticulate::py_eval("r.embedding._.trf_data.tensors[-1][0]")
        embedding = reticulate::py_to_r(embedding)
      }
      # this following part is implemented in the python code but I haven't found an applicable use case yet
      if (!is.array(embedding) && hasMethod("get", embedding)) {
        embedding = embedding$get()
      }
      embeddings <- rbind(embeddings, embedding)
    }
    attributes(embeddings)$dimnames <- NULL
  }
  else if(grepl("^flair",class(embedder)[[1]])){
    flair <- reticulate::import("flair")
    
    # disable fine tune to prevent CUDA OOM error (have not experienced this myself - from BERTopic python package)
    if ("fine_tune" %in% names(embedder)){
      embedder$fine_tune <- FALSE 
    }
    
    # want document embeddings, not token embeddings so using mean pooling to convert token to document embeddings
    if (grepl("TokenEmbeddings", class(embedder)[[2]])){
      embedder <- flair$embeddings$DocumentPoolEmbeddings(embedder) 
    }
    
    embeddings <- c()
    for (doc in 1:length(documents)){
      sentence <- flair$data$Sentence(documents[doc]) # convert document to flair object
      embedding_step <- embedder$embed(sentence) # embed sentence
      embedding <- sentence$embedding$detach()$numpy() # extract embedding and tensor values and convert values to numpy array
      embeddings <- rbind(embeddings, embedding) # concatenate document embeddings
    }
    attributes(embeddings)$dimnames <- NULL # remove dimnames attribute
  }
    
    
  # implement this if we introduce a hugging face embedder function
  # else if(grepl("transformers.*pipelines",class(embedder)[[1]])){
  #   
  # }
  
  
 
  #Give the user a quick console nudge that the embeddings are ready
  message("\nEmbedding proccess finished")
  
  #Keep track of the number of documents that were fed into the bt_embed function, should be useful later when merging data frames and documents aren't present any more. Should we just return a data frame with the documents, and nested embeddings?
  n_documents <- length(documents)
  attr(embeddings, "n_documents") <- n_documents
  
  
  #Add the embedding_model attribute if we can (this will help later on, or when debugging for other users.)
  if("embedding_model" %in% names(embedder_attributes)){
    attr(embeddings, "embedding_model") <- embedder_attributes$embedding_model
    message(paste0(embedder_attributes$embedding_model, " added to embeddings attributes"))
  } else{
    message("No embedding_model attribute found on embedder, proceeding without adding")
  }
  
  return(embeddings)
  
}

# do we need a bt_make_embedder_hf when we have st? Should I be using pipeline or different function to make the embedder ----
#' Create an embedding model using the [hugging face library](https://huggingface.co/models)
#'
#' @param ... Additional arguments sent to transformers.pipeline()
#' @param task The task defining the pipeline to be returned, this defaults to feature extraction.
#' @param model The model used by the pipeline to make predictions
#'
#' @return a pipeline formed according to the task defined
#' @export
#'
#' @examples
#' # define task and use default model for that task
#' embedder <- bt_make_embedder_hf(task = "feature-extraction")
#' 
#' # define model and use task specified for that model
#' embedder <- bt_make_embedder_hf(model = "distilbert-base-cased")
#' 
#' # define task and model
#' embedder <- bt_make_embedder_hf(task = "feature-extraction", model = "facebook/bart-base")
#' 
#' # define 
# bt_make_embedder_hf <- function(..., task = "feature-extraction", model = NULL){
#   # input argument validation
#   if (is.null(model) & is.null(task)){
#     stop("Either model or task input argument must be specified.")
#   }
#   
#   stopifnot(is.null(task)| is.character(task),
#             is.null(model) | is.character(model))
#   
#   dots <- rlang::list2(...) # extra inputs as list
#   transformers <- reticulate::import("transformers") # import transformers library
#   inspect <- reticulate::import("inspect") # import inspect to validate extra arguments
#   empty_model <- transformers$pipeline # function to be used with no arguments
#   available_args <- inspect$getfullargspec(empty_model)$args # arguments allowed
#   
#   if(any(!names(dots) %in% available_args)){
#     bad_args <- names(dots)[!names(dots) %in% names(empty_model)] # non-applicable args
#     stop(paste("Bad argument(s) attempted to be sent to transformers.pipeline():", bad_args, sep = ' '))
#   }
#   
#   # end input validation
#   
#   pipeline <- transformers$pipeline(task = task,
#                                     model = model,
#                                     ...)
#   
#   return(pipeline)
#   
# }
