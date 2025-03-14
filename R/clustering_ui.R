clustering_ui <- function(id) {
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel with inputs
    sidebarPanel(
      selectInput("cluster_method", "Clustering Method", choices = c("HDBSCAN", "K-Means")),
      conditionalPanel(
        condition = "input.cluster_method == 'K-Means'",
        numericInput("n_clusters", "Number of Clusters", value = 10)
      ),
      conditionalPanel(
        condition = "input.cluster_method == 'HDBSCAN'",
        sliderInput("min_cluster_size", "Minimum cluster size:",
                    # min = 2, max = nrow(df)/2, value = 20), # arbitrarily setting the defaults
                    min = 2, max = 100, value = 20), # arbitrarily setting the defaults
        sliderInput("min_sample_size", "Minimum number of samples:",
                    min = 1, max = 10, value = 1), # these values update as defined in the server
        radioButtons("hdbscan_metric", "Metric", choices = c("cosine", "euclidean")),
        selectInput("hdbscan_metric", "Metric", choices = c(
          # "cosine",
          "braycurtis", "canberra", "chebyshev", "cityblock", "correlation",  "dice", "euclidean", "hamming", "jaccard", "jensenshannon", "kulczynski1", "mahalanobis", "matching", "minkowski", "rogerstanimoto", "russellrao", "seuclidean", "sokalmichener", "sokalsneath", "sqeuclidean", "yule")),
        radioButtons("hdbscan_cluster_selection", "Cluster Selection Method", choices = c("eom", "leaf"))
      ),
      actionButton("do_modelling", "Model", class = "btn-succes"),
      actionButton("reset_model", "Reset", classs = "btn-danger"),
      verbatimTextOutput("complete_message")
    ),
    
    # Main panel with the plot
    mainPanel(
      plotOutput("cluster_plot")
    )
  ) # sidebarLayout
}