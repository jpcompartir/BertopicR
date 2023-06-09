% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_df.R
\name{bt_make_df}
\alias{bt_make_df}
\title{make a df combining bertopic output with columns in original data export}
\usage{
bt_make_df(
df,
model,
embeddings,
text_var = message,
date_var = created_time)
}
\arguments{
\item{df}{original Sprinklr export topic modelling was performed on to merge with bertopic output}

\item{model}{bertopic model}

\item{embeddings}{embeddings used to generate the model}

\item{text_var}{original, uncleaned text column, of text used to fit model}

\item{date_var}{date column in df corresponding to text used to fit model}
}
\value{
df with bertopic output merged with input columns from the sprinkr export
}
\description{
make a df combining bertopic output with columns in original data export
}
