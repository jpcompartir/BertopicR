# bt_compile_fit_model <- function(documents,
#                                  min_cluster_size = 10,
#                                  n_neighbours = 15,
#                                  nr_clusters = NULL,
#                                  topic_labels = NULL,
#                                  embeddings = NULL,
#                                  embedding_model = NULL, 
#                                  reduction_model = NULL,
#                                  clustering_model = NULL, 
#                                  vectoriser_model = NULL, 
#                                  ctfidf_model = NULL){
#   
#   # Validate Inputs ----
#   stopifnot(is.character(documents),
#             is.numeric(min_cluster_size) | is.null(min_cluster_size),
#             is.numeric(n_neighbours) | is.null(n_neighbours), 
#             is.numeric(nr_clusters) | is.null(nr_clusters), 
#             is.vector(topic_labels) | is.null(topic_labels ),
#             is.array(embeddings)| is.data.frame(embeddings), # |is.null(embeddings)
#             test_is_python_object(embedding_model) | is.null(embedding_model),
#             test_is_python_object(reduction_model) | is.null(reduction_model),
#             test_is_python_object(clustering_model) | is.null(clustering_model),
#             test_is_python_object(vectoriser_model) | is.null(vectoriser_model),
#             test_is_python_object(ctfidf_model) | is.null(ctfidf_model))
#   
#   #Provide a default embedding model for: Since MMR is using word embeddings to diversify the topic representations, it is necessary to pass the embedding model to BERTopic if you are using pre-computed embeddings:"
#   if(is.null(embedding_model)){
#     embedding_model <- bt_make_embedder(model_name = "all-mpnet-base-v2")
#     message("\nNo embedding model provided, defaulting to 'all-mpnet-base-v2' model as embedder.")
#   }
#   
#   #If no UMAP model given, provide empty
#   if(is.null(reduction_model)){
#     reduction_model <- bt_make_reducer_umap()
#     message("\nNo reduction_model provided, using default 'bt_reducer_umap' parameters.")
#   }
#   
#   if(is.null(clustering_model)){
#     clustering_model <- bt_make_clusterer_hdbscan(n_neighbours = n_neighbours, min_cluster_size = min_cluster_size)
#     message("\nNo clustering model provided, using hdbscan with default parameters.")
#   }
#   
#   if(is.null(vectoriser_model)){
#     vectoriser_model <- bt_make_vectoriser()
#     message("\nNo vectorising model provided, creating model with default parameters")
#   }
#   
#   if(is.null(ctfidf_model)){
#     ctfidf_model <- bt_make_ctfidf()
#     message("\nNo ctfidf model provided, creating model with default parameters")
#   }
# 
#   
#   
#   bt <- reticulate::import("bertopic")
#   
#   model <- bt$BERTopic(embedding_model = embedding_model,
#                        umap_model = reduction_model,
#                        hdbscan_model = clutster_model,
#                        ctfidf_model = ctfidf_model,
#                        vectoriser_model = vectoriser_model,
#                        nr_topics = nr_clusters
#                        )
#   
#   model$fit(documents = documents,
#             embeddings = embeddings,
#             y = topic_labels)
#   
# }