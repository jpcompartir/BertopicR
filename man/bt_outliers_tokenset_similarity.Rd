% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify_topics.R
\name{bt_outliers_tokenset_similarity}
\alias{bt_outliers_tokenset_similarity}
\title{Redistributes outliers using tokenset c-TF-IDF scores}
\usage{
bt_outliers_tokenset_similarity(
  fitted_model,
  documents,
  topics,
  ...,
  window = 4L,
  stride = 1L,
  threshold = 0.3
)
}
\arguments{
\item{fitted_model}{Output of bt_fit_model() or another bertopic topic model. The model must have been fitted to data.}

\item{documents}{documents to which the model was fit}

\item{topics}{current topics associated with the documents}

\item{...}{Optional or additional parameters passed to approximate_distribution function, e.g. batch_size}

\item{window}{size of the moving window which is the number of tokens in a tokenset}

\item{stride}{how far the window should move at each step (number of words to
skip when moving to next tokenset)}

\item{threshold}{minimum probability for outlier to be reassigned}
}
\value{
df with document, old topic, new topic
}
\description{
Divides documents into tokensets and calculates the c-TF-IDF
similarity of each tokenset to each topic. For each outlier document, the similarity
scores of each tokenset for each topic are summed together by topic and the outlier is
redistributed to the topic with the highest similarity. Note that the purpose
of this function is to obtain a new list of topics that can then be used to
update the model, it does not make any changes to the model itself, the topic
classification the model outputs does not change after running this function.
The bt_update_topics function needs to be used to make the change to the model
itself.
}
\details{
It is possible to chain outlier reduction methods together as the operation works on
the list of topics input to the argument, which can vary. You will see in the examples
that we are able to perform one outlier reduction method, eg. bt_outliers_embeddings,
which will output a list of potential new topics, and input that list into another
outlier reduction method, eg. bt_outliers_tokenset_similarity, which will determine the output topic
suggestions based on the input list. In this way we can use aspects of multiple outlier
reduction strategies and chain them together.
}
\examples{
\dontrun{
# Reducing outliers original clustering model identified
outliers <- bt_outliers_tokenset_similarity(fitted_model = topic_model, documents = docs, topics = topic_model$topics_)

# Using chain strategies to build on outliers identified by another reduction strategy to redistribute outlier docs
# using embeddings to redistribute outliers
outliers_embed <- bt_outliers_embedings(fitted_model = topic_model, documents = docs, topics = topic_model$topics_, embeddings = embeddings, threshold = 0.5)

# using tokenset similarity outlier reduction method on top of embeddings method to redistribute outliers
outliers_chain <- bt_outliers_tokenset_similarity(fitted_model = topic_model, documents = docs, topics = outliers_embed$new_topics, threshold = 0.2)

}


}
