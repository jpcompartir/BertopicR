% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.R
\name{bt_make_clusterer_hdbscan}
\alias{bt_make_clusterer_hdbscan}
\title{Create an HDBSCAN clustering model}
\usage{
bt_make_clusterer_hdbscan(
  ...,
  min_cluster_size = 10L,
  min_samples = 10L,
  metric = "euclidean",
  cluster_selection_method = c("eom", "leaf"),
  prediction_data = FALSE
)
}
\arguments{
\item{...}{Additional arguments sent to hdbscan.HDBSCAN()}

\item{min_cluster_size}{Minimum number of data points for each cluster, enter as integer by adding L to number}

\item{min_samples}{Controls the number of outliers generated, lower value = fewer outliers.}

\item{metric}{Distance metric to calculate clusters with}

\item{cluster_selection_method}{The method used to select clusters. Default is "eom".}

\item{prediction_data}{Set to TRUE if you intend on using model with any functions from hdbscan.prediction eg. if using bt_outliers_probabilities}
}
\value{
An instance of the HDBSCAN clustering model (Python object.
}
\description{
Instantiates an HDBSCAN clustering model using the hdbscan Python library.
}
\examples{
# using minkowski metric for calculating distance between documents - when using minkowski metric, a value for p must be specified as an additional argument
clustering_model <- bt_make_clusterer_hdbscan(metric = "minkowski", p = 1.5)

# specify integer numeric inputs as integer, using additional gen_min_span_tree argument
clusterer = bt_make_clusterer_hdbscan(min_cluster_size = 5L, gen_min_span_tree = TRUE)

# not specifying numeric inputs as integers (converted to integers internally)
clusterer = bt_make_clusterer_hdbscan(min_cluster_size = 5, cluster_selection_method = "leaf")

}
\seealso{
\url{https://hdbscan.readthedocs.io/en/latest/}
\url{https://hdbscan.readthedocs.io/en/latest/how_hdbscan_works.html}
\url{https://hdbscan.readthedocs.io/en/latest/basic_hdbscan.html#what-about-different-metrics}
}
