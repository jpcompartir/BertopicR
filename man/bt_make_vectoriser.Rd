% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vectorise.R
\name{bt_make_vectoriser}
\alias{bt_make_vectoriser}
\title{Create a text vectoriser}
\usage{
bt_make_vectoriser(
  ...,
  ngram_range = c(1, 2),
  stop_words = "english",
  min_frequency = 10L,
  max_features = NULL
)
}
\arguments{
\item{...}{Additional parameters passed to sklearn's CountVectorizer}

\item{ngram_range}{A vector of length 2 (default c(1, 2)) indicating the lower and upper boundary of the range of n-values for
different word n-grams or char n-grams to be extracted as features. All values of n such
that min_n <= n <= max_n will be used. For example an ngram_range of c(1, 1) means only unigrams,
c(1, 2) means unigrams and bigrams, and c(2, 2) means only bigrams.}

\item{stop_words}{String (default 'english'). If a string, it is passed to _check_stop_list and the
appropriate stop list is returned. 'english' is currently the default.}

\item{min_frequency}{Integer (default 10L). When building the vocabulary ignore terms that have a
corpus frequency strictly lower than the given threshold.}

\item{max_features}{Integer or NULL (default NULL). If not NULL, build a vocabulary that only considers
the top max_features ordered by term frequency across the corpus.}
}
\value{
An sklearn CountVectorizer object configured with the provided parameters
}
\description{
This function uses Python's sklearn for feature extraction and count vectorisation.
It creates a CountVectorizer object with the specified parameters.
CountVectorizer is a way to convert text data into vectors as model input. Used inside a BertopicR topc modelling pipeline.
}
