% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify_topics.R
\name{bt_merge_topics}
\alias{bt_merge_topics}
\title{Merges list(s) of topics together}
\usage{
bt_merge_topics(fitted_model, documents, topics_to_merge)
}
\arguments{
\item{fitted_model}{Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.}

\item{documents}{documents which the model was fitted to}

\item{topics_to_merge}{list (or list of lists/vectors) of topics created by your bertopic model that you wish to merge. Topics should be given in numeric form.}
}
\value{
bertopic model with specified topics merged
}
\description{
Merge topics of an already-fitted BertopicR model.
You can feed in a list, to merge topics together, or a list of lists to perform
merges on multiple groups of topics.

NOTE: The bertopic model you are working with is a pointer to a python object
at a point in memory. This means that the input and the output model cannot be
differentiated between without explicitly saving the model before performing
this operation. We do not need to specify an output to the bt_fit_model function
as the function changes the input model in place. If you do decide to explicitly assign a function output,
be aware that the output model and the input model will be the same as one another.
}
\details{
This function updates the model so that the topics in the topics_to_merge list
become 1 topic. The grouped topics take the topic representation (Name) of the
first topic in the list. Any number of topics can be merged together, if you would
like to merge two separate groups of topics, you must pass a list of lists/vectors as
topics_to_merge eg. list(c(1L, 3L), c(0L, 6L, 7L)) or list(list(1L, 3L), list(0L, 6L, 7L))
}
\examples{
\dontrun{

# merge two topics
bt_merge_topics(fitted_model = model, documents = documents, topics_to_merge = list(1L, 2L))

# merge three topics
bt_merge_topics(fitted_model = model, documents = documents, topics_to_merge = list(1L, 2L, 3L))

# merge multiple sets of topics as a list of vectors
bt_merge_topics(fitted_model = model, documents = documents, topics_to_merge = list(c(1L, 3L), c(0L, 6L, 7L), c(2L, 12L)))

# merge multiple sets of topics as a list of lists
bt_merge_topics(fitted_model = model, documents = documents, topics_to_merge = list(list(1L, 3L), list(0L, 6L, 7L), list(2L, 12L)))
}

}
