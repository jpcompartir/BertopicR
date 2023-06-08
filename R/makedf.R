#' make a df combining bertopic output with columns in original data export
#'
#' @param df original Sprinklr export topic modelling was performed on to merge with bertopic output
#' @param model bertopic model
#' @param embeddings embeddings used to generate the model
#' @param text_var original, uncleaned text column, of text used to fit model
#' @param date_var date column in df corresponding to text used to fit model
#' 
#' @return df with bertopic output merged with input columns from the sprinkr export
#' @importFrom rlang :=
#' @export
#'
#' @usage makedf(
#' df,
#' model,
#' embeddings,
#' text_var = message,
#' date_var = created_time)
#' 
makedf <- function(df,
                   model = model, 
                   embeddings = embeddings,
                   text_var = message,
                   date_var = created_time){ 
                 
  text_sym <- rlang::ensym(text_var)
  date_sym <- rlang::ensym(date_var)
  
  # text data sed to fit model
  docs <- df %>% dplyr::pull(!!text_sym)

  # get bertopic table
  doc_info <- model$get_document_info(docs)
  
  # import umap
  umap <- reticulate::import("umap", convert = TRUE)
  
  # get umap co-ords
  reduced_embeddings <- umap$UMAP(n_neighbors=10, 
                               n_components=2, 
                               min_dist=0.0, 
                               metric='cosine')$fit_transform(embeddings)

  # create df
  merged_df <- df %>%
    dplyr::mutate(V1 = reduced_embeddings[,1],
                  V2 = reduced_embeddings[,2],
                  document = dplyr::row_number(), 
                  topic = doc_info$Topic,
                  topic_title = doc_info$Name,
                  !!date_sym := as.Date(!!date_sym)) %>% # convert date to correct format
    dplyr::relocate(document)

  
  }

