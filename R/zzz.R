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
