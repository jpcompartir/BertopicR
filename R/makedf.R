#' Title
#'
#' @param model bertopic model
#' @param embeddings embeddings used to generate the model
#' @param original_text uncleaned text column
#' @param cleaned_text cleaned text column which model was fitted on
#' @param date date column
#' @param sentiment sentiment column
#' @param permalink permalink column
#'
#' @return df with bertopic output merged with input columns from the sprinkr export
#' @export
#'
#' @usage makedf(
#' model
#' embeddings
#' original_text = df$message
#' cleaned_text = df$text_clean,
#' date = df$created_time,
#' sentiment = df$sentiment,
#' permalink = df$permalink)
makedf <- function(model = model, 
                   embeddings = embeddings, 
                   original_text = df$message, 
                   cleaned_text = df$text_clean, 
                   date = df$created_time, 
                   sentiment = df$sentiment, 
                   permalink = df$permalink){
  
  # get bertopic table
  doc_info <- model$get_document_info(cleaned_text)
  
  # import umap
  umap <- import("umap", convert = TRUE)
  
  # get umap co-ords
  reduced_embeddings <- umap$UMAP(n_neighbors=10, 
                               n_components=2, 
                               min_dist=0.0, 
                               metric='cosine')$fit_transform(embeddings)
  
  merged_df <- doc_info %>%
    janitor::clean_names() %>%
    rename(text_clean = document) %>%
    mutate(V1 = reduced_embeddings[,1],
           V2 = reduced_embeddings[,2],
           document = dplyr::row_number(),
           text_og = original_text,
           date = as.Date(date),
           sentiment = sentiment,
           permalink = permalink) %>%
    relocate(document)

  
  }

